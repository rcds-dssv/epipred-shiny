
# Module for the Home Tab
HomeUI <- function(id) {
  page_fixed(
    layout_columns(
      card(
        span(
          p(shiny::img(src = "gene2mouse.jpg"), style = "text-align:center"),
          h1("EpiMVP", style="text-align:center;")
        ),
        card_body(
          shiny::HTML("
             <h5><p><div> 
                                 
                An The Epilepsy Multiplatform Variant Prediction project is an NIH-sponsoredCenter Without Walls (CWOW) that will develop a modular, 
                highly integrated platform approach to accelerate determination of the functional, pharmacological, neuronal network and whole animal 
                consequences of genetic variants among a range of clinical epilepsy types. Read more about <a href=https://epimvp.med.umich.edu/>EpiMVP </a></p>
                
               <img src=gene2ppl.jpg alt=gene2ppl style=float:right;>
                
               <p>The Gene Variant and Curation Core (GVCC) integrates data from clinical genetic data and functional readouts from EpiMVP and 
                utilizes the power of machine learning to create a computational algorithm called EpiPred. </p>
                
               <p>EpiPred is currently only available for the gene <a href=https://www.ncbi.nlm.nih.gov/books/NBK396561/>STXBP1 </a> 
                 but will be expanded to other epilepsy-associated genes in the future. 
                 On this website you can query individual missense variants and receive a prediction score as to whether that variant is likely 
                 pathogenic or benign. We have also calculated EpiPred scores for all possible missense variants in STXBP1.  </p></div>
                
                The manuscript detailing this work is available here and all code is freely available at the EpiMVP github.
             
             </h5>")
        )
      )
    )
  )
}

# Module for Single Missense Variant Prediction Tab
SingleVarUI <- function(id) {
  page_fixed(
    titlePanel(h1("EpiPred Result Explorer", align = "center") ),
    
    br(),  
    
    # Variant input header
    layout_columns(
      h2("Variant Input"),
      col_widths = 12,
      style='border-bottom: 1px solid #c6c7c7'
    ),
    
    # Input for Amino Acid Sequence and Display prediction result
    layout_columns(
      card(
        card_body(
          p("Input Amino Acid Sequence", style="text-align: center;"),
          textInput(NS(id,"val"), label = NULL, value = "p.A2P"),
          actionButton(NS(id,"update"), "Submit", class = "btn btn-primary"),
          align = "center"
        ),
      ),
      tooltip(
        bs_icon("question-circle-fill", color = "grey"),
        "Some Message",
        placement = "bottom"
      ),
      col_widths = c(-4, 4, -3, 1)
    ),
    
    br(),
    
    # result viewer header
    layout_columns(
      h2("Result Viewer"),
      
      # visualization control
      dropdownButton(
        radioButtons(NS(id,"epi_dist"), "Distribution in Colorbar",
                     choices = c("proportion", "density"), selected = "proportion"
        ),
        
        strong("Toggle 3-D Viewer Spin"),
        br(),
        switchInput(NS(id,"NGL_spin"), value = TRUE, size = "small"),
        icon = bs_icon("gear"), width = "300px",
        right = TRUE,
        status = "info",
        circle = FALSE,
        size = "sm"
      ),
      col_widths = c(11,1),
      style='border-bottom: 1px solid #c6c7c7'
    ),
    
    # Epipred colorbar plot
    layout_columns(
      card(
        card_header(
          "Your Sequence's EpiPred Score", HTML('&nbsp;'),
          tooltip(
            bs_icon("question-circle-fill", color = "grey"),
            "Colorbar Description",
            placement = "bottom"
          )
        ),
        card_body(
          plotOutput(NS(id,"epipred_bar"), height = 260), width = "10%"
        )
      ),
      
      # spinning protein
      card(
        card_header(
          "3-D Rendering of Protein", HTML('&nbsp;'),
          tooltip(
            bs_icon("question-circle-fill", color = "grey"),
            "Protein Description",
            placement = "bottom"
          )
        ),
        NGLVieweROutput(NS(id,"structure"))
      ),
      col_widths = c(12,12)
    )
    
  )

}

SingleVarServer <- function(id, mutations) {
  moduleServer(id, function(input, output, session) {
    
    # used for plotting the colorbar in "for patients" tab
    epi_dist_summary <- reactive(get_epi_distribution_summary(mutations()))
    
    epipred_colorbar <- reactive(
      create_epipred_colorbar2(
        epi_dist_summary = epi_dist_summary(),
        distribution_type = input$epi_dist
      )
    )
    
    # update variant ID on update button press
    # only update if the variant id exists
    variant_id_tmp <- reactive({
      input$update
      isolate(input$val)
    })
    variant_id <- reactiveVal()
    observe({
      if (variant_id_tmp() %in% mutations()$AA_Change) {
        variant_id(variant_id_tmp())
      }
    })
    
    # retrieve prediction result for chosen variant
    epipred_prediction <- reactive({
      get_epipred_prediction(variant_id(), mutations())
    })
    
    # NGLViewer Output
    output$structure <- renderNGLVieweR({
      NGLVieweR(pdbfile) %>%
        addRepresentation(
          "cartoon",
          param = list(name = "cartoon", color = "residueindex")
        ) %>%
      stageParameters(backgroundColor = "white") %>%
      setQuality("high") %>%
      setFocus(0) %>%
      setSpin(TRUE)
    })
    
    observeEvent(input$NGL_spin,{
      NGLVieweR_proxy("structure") %>%
        updateSpin(input$NGL_spin)
    })
    
    # when variant is updated, highlight the affected residue
    observeEvent(input$update, {
      aa_pos <- mutations() %>%
        filter(AA_Change %in% variant_id()) %>%
        pull(AA_POS) %>% 
        min() %>%
        as.character()
      
      NGLVieweR_proxy("structure") %>%
        removeSelection("my_residue") %>%
        addSelection("surface",
          param = list(
            isolevel = 0.005, isolevelType = "value",
            sele = aa_pos, name = "my_residue"
          )
        )
    })
    
    # variant prediction text output
    output$text <- renderText({
      data <- data.frame(mutations())
      paste(
        "Pathogenic Score is:",
        toString(epipred_prediction()$score),
        "\nPathogenic Class:",
        toString(epipred_prediction()$class)
      )
    })
    
    # epipred colorbar output
    output$epipred_bar <- renderPlot({
      display_epipred_score(
        epipred_prediction = epipred_prediction(),
        epipred_colorbar = epipred_colorbar()
      )
    }, height = 230)
    
    # plot position vs score
    output$epi_score_indiv_plot <- renderPlot({
      if (input$report_gg != "All") {
        mutations_filtered <- mutations() %>%
          filter(Reported == input$report_gg)
      } else {
        mutations_filtered <- mutations()
      }
      plot_epi_raw_violinplot(
        var_id = variant_id(),
        mutations = mutations_filtered
      )
    })
  })
  
  
}

# Module for displaying Table for a given gene
TableDisplayUI <- function(id) {
  page_fluid(
    
    # title
    titlePanel("Missense Variant Table"),
    
    # Display gene
    h3("Gene: STXBP1"),
    
    # display mutation table filter selection and data table
    layout_columns(
      selectInput(NS(id,"class"),
                  "EpiPred Class:",
                  c("All",unique(as.character(mutations$EpiPred_Class)))
      ),
      selectInput(NS(id,"report"),
                  "Reported:",
                  c("All",unique(as.character(mutations$Reported)))
      ),
      card(DT::dataTableOutput(NS(id,"table")), height = 600),
      
      # download button
      downloadButton(NS(id,"downloadData"), label = "Download"),
      col_widths = c(4,4,-4,12,12)
    )
  )
}

TableDisplayServer <- function(id, mutations) {
  moduleServer(id, function(input, output, session) {
    # Filter data table based on selections
    output$table <- DT::renderDataTable(DT::datatable({
      data <- data.frame(mutations())
      if (input$class != "All") {
        data <- data[data$EpiPred_Class == input$class,]
      }
      if (input$report != "All") {
        data <- data[data$Reported == input$report,]
      }
      data
    }) %>% formatStyle(colnames(mutations()), "white-space"="nowrap"))
    
    #For Download Button
    selectedData <- reactive({
      data <- data.frame(mutations())
      if (input$class != "All") {
        data <- data[data$EpiPred_Class == input$class,]
      }
      if (input$report != "All") {
        data <- data[data$Reported == input$report,]
      }
      data
    })
    
    # Downloadable csv of selected dataset ----
    output$downloadData <- downloadHandler(
      filename = function() {
        paste("Table_Out",Sys.Date(),".csv", sep = "")
      },
      content = function(file) {
        write.csv(selectedData(), file, row.names = FALSE)
      }
    )
  })
}

# Module for exploring prediction result for all variants in a given gene
AllVarUI <- function(id) {
  page_fixed(
    # Title
    titlePanel("All Variants Summary"),
    
    layout_sidebar(
      
      # Controls for plot
      sidebar = sidebar(
        selectInput(NS(id,"var1"), "x Variable", choices = scatterplot_vars, selected = scatterplot_vars[1]),
        selectInput(NS(id,"var2"), "y Variable", choices = scatterplot_vars, selected = scatterplot_vars[2]),
        selectInput(NS(id,"margin_type"), "Margin Plot Type", choices = c("density", "histogram", "boxplot", "violin", "densigram")),
        checkboxGroupInput(NS(id,"report"), "Reported:", choices = report_source, selected = report_source),
        bg = "#f5f5f5"
      ),
      
      # Display plot output
      card(
        plotOutput(NS(id,"marginal_plot"))
      )
    )
  )
}

AllVarServer <- function(id, mutations) {
  moduleServer(id, function(input,output,session) {
    output$marginal_plot <- renderPlot({
      req(length(input$report) > 0)
      marginal_plot(
        mutations = mutations(),
        var1 = input$var1,
        var2 = input$var2,
        margin_type = input$margin_type,
        reported = input$report
      )
    })
  })
}
