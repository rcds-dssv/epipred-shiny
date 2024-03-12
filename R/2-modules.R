
# Module for the Home Tab
HomeUI <- function(id) {
  fluidRow(
    column(10, align="center", offset = 1,
           shiny::HTML("<br><br> <h1>EpiMVP</h1><br>"),
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
             
             </h5>"),
           shiny::img(src = "gene2mouse.jpg")
    ),
    
  )
}

# Module for Single Missense Variant Prediction Tab
SingleVarUI <- function(id) {
  tagList(
    # Display NGLViewer
    NGLVieweROutput(NS(id,"structure")),
    
    fluidRow(
      # Input for Amino Acid Sequence and Display prediction result
      column(8, align="center", offset = 2,
        textInput(NS(id,"val"), h3("Input Amino Acid Sequence"), value = "p.A2P"),
        actionButton(NS(id,"update"), "Submit", class = "btn btn-primary"),
        div(style="padding-top:15px;", verbatimTextOutput(NS(id,"text"))),
        plotOutput(NS(id,"epipred_bar"), height = 230)
      )
    ),
    # color bar plot
    fluidRow(
      column(
        4,
        selectInput(
          NS(id,"report_gg"), "Reported:",
          c("All", report_source)
        ),
        offset = 2
      )
    ),
    # epipred score vs position
    fluidRow(
      column(
        12, plotOutput(NS(id,"epi_score_indiv_plot"), width = "80%"), align = "center"
      )
    )
  )
}

SingleVarServer <- function(id, epipred_colorbar, line_orientation) {
  moduleServer(id, function(input, output, session) {
    # update variant ID on update button press
    # only update if the variant id exists
    variant_id_tmp <- reactive({
      input$update
      isolate(input$val)
    })
    variant_id <- reactiveVal()
    observe({
      if (variant_id_tmp() %in% mutations$AA_Change) {
        variant_id(variant_id_tmp())
      }
    })
    
    # retrieve prediction result for chosen variant
    epipred_prediction <- reactive({
      get_epipred_prediction(variant_id(), mutations)
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
    
    # when variant is updated, highlight the affected residue
    observeEvent(input$update, {
      aa_pos <- mutations %>%
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
      data <- data.frame(mutations)
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
        epipred_colorbar = epipred_colorbar,
        line_orientation = line_orientation
      )
    }, height = 200)
    
    # plot position vs score
    output$epi_score_indiv_plot <- renderPlot({
      if (input$report_gg != "All") {
        mutations_filtered <- mutations %>%
          filter(Reported == input$report_gg)
      } else {
        mutations_filtered <- mutations
      }
      plot_epi_raw(
        var_id = variant_id(),
        mutations = mutations_filtered
      )
    })
  })
}

# Module for displaying Table for a given gene
TableDisplayUI <- function(id) {
  tagList(
    titlePanel("Missense Variant Table"),
    h3("Gene: STXBP1"),
    fluidRow(
      column(4,
             selectInput(NS(id,"class"),
                         "EpiPred Class:",
                         c("All",unique(as.character(mutations$EpiPred_Class)))
             )
      ),
      column(4,
             selectInput(NS(id,"report"),
                         "Reported:",
                         c("All",unique(as.character(mutations$Reported)))
             )
      )
      
    ),
    DT::dataTableOutput(NS(id,"table")),
    downloadButton(NS(id,"downloadData"), "Download")
  )
}

TableDisplayServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Filter data table based on selections
    output$table <- DT::renderDataTable(DT::datatable({
      data <- data.frame(mutations)
      if (input$class != "All") {
        data <- data[data$EpiPred_Class == input$class,]
      }
      if (input$report != "All") {
        data <- data[data$Reported == input$report,]
      }
      data
    }))
    
    #For Download Button
    selectedData <- reactive({
      data <- data.frame(mutations)
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
  tagList(
    titlePanel("All Variants Summary"), 
    plotlyOutput(NS(id,"score_comparison_plot"), height = 400, width = 700),
    plotOutput(NS(id,"score_distribution_plot_CADD"), height = 400, width = 700),
    plotOutput(NS(id,"score_distribution_plot_EpiPred"), height = 400, width = 700),
    plotOutput(NS(id,"score_by_position_plot_CADD"), height = 400, width = 700),
    plotOutput(NS(id,"score_by_position_plot_EpiPred"), height = 400, width = 700)
  )
}

AllVarServer <- function(id) {
  moduleServer(id, function(input,output,session) {
    # score comparison plot
    output$score_comparison_plot <- renderPlotly({
      score_comparison_plot(mutations)
    })
    
    # compare score distribution
    output$score_distribution_plot_CADD <- renderPlot({
      score_distribution_plot(mutations, "CADD_PHRED")
    })
    output$score_distribution_plot_EpiPred <- renderPlot({
      score_distribution_plot(mutations, "EpiPred_Raw_Score")
    })
    
    # show score by position plot
    output$score_by_position_plot_CADD <- renderPlot({
      score_v_position(mutations, "CADD_PHRED")
    })
    output$score_by_position_plot_EpiPred <- renderPlot({
      score_v_position(mutations, "EpiPred_Raw_Score")
    })
  })
}
