
home_text <- HTML(
  "
  <h5><div>
  <p>
    An The Epilepsy Multiplatform Variant Prediction project is an NIH-sponsoredCenter Without Walls (CWOW) that will develop a modular, 
    highly integrated platform approach to accelerate determination of the functional, pharmacological, neuronal network and whole animal 
    consequences of genetic variants among a range of clinical epilepsy types. Read more about <a href=https://epimvp.med.umich.edu/>EpiMVP</a>
  </p>
    
   <img src=gene2ppl.jpg alt=gene2ppl style=float:right;>
    
  <p>
    The Gene Variant and Curation Core (GVCC) integrates data from clinical genetic data and functional readouts from EpiMVP and 
    utilizes the power of machine learning to create a computational algorithm called EpiPred.
  </p>
    
  <p>
    EpiPred is currently only available for the gene <a href=https://www.ncbi.nlm.nih.gov/books/NBK396561/>STXBP1 </a> 
    but will be expanded to other epilepsy-associated genes in the future. 
    On this website you can query individual missense variants and receive a prediction score as to whether that variant is likely 
    pathogenic or benign. We have also calculated EpiPred scores for all possible missense variants in STXBP1.
  </p>
  
  <p>  
    The manuscript detailing this work is available here and all code is freely available at the EpiMVP github.
  </p>
  
  </div>
  </h5>"
)

overall_help_text <- HTML(
  "
  <p>
    In this tab, you can search for Amino Acid (AA) sequences from epilepsy-associated genes with a missense variant (mutation).
    The output is an EpiPred raw score and prediction on the sequence's pathogenicity (whether it is likely to cause epilepsy).
  </p>
  
  <strong>What is a missense variant?</strong>
  
  <p>
    A missense variant is a type of mutation in which a change in your DNA sequence can cause an amino acid in a protein to change to a different one.
    This type of mutation can be benign or pathogenic, depending on the location of the mutation and the resulting change in the protein's function.
  </p>
  
  <strong>What is EpiPred?</strong>
  
  <p>
    EpiPred is a machine learning model specifically trained for classification of missense variants as likely pathogenic or benign.
  </p>
  
  <p>
    If you already have a gene and its AA sequence ID in mind, you can input your sequence in the box below.
    To change settings for the visualizations, click on the gear icon next to the \"Result Viewer\".
  </p>
  
  <p>
    <strong>Guideline for Classification Interpretation:</strong>
    <ul>
      <li>Likely Benign</li>
      <li>Possibly Benign</li>
      <li>Possibly Pathogenic</li>
      <li>Likely Pathogenic</li>
  </p>
  "
)
  
colorbar_help_text1 <- 
  "
  Predicted score and class of your amino acid is displayed here.
  "

protein_help_text <- 
  "
  3-D rendering of the protein (from selected gene) is shown here.\n
  Location of the mutation is highlighted by a white blob.
  "

allvar_help_text <- HTML(
  "
  <p>In this tab, you can explore across different genes and compare positions and various scores.
  Use the control on the left to select the gene explore relationships</p>
     
  <p>The table below shows all the missense variants in the gene. You can filter the table by EpiPred class
  and Reported source. Clicking on a row highlights the mutation in the above plot.</p>
  
  <p>You can also download the table by clicking the download button below the table.</p>"
)

distr_help_text <- 
  "
  The distribution of the EpiPred score across all possible sequences in the gene is shown here.
  "

# Home UI -----------------------------------------------------------------

# Module for the Home Tab
HomeUI <- function(id) {
  page_fixed(
    layout_columns(
      card(
        span(
          p(shiny::img(src = "gene2mouse.jpg"), style = "text-align:center"),
          h1("EpiMVP", style="text-align:center;")
        ),
        card_body(home_text)
      )
    )
  )
}

# Single Var --------------------------------------------------------------

# Module for Single Missense Variant Prediction Tab
SingleVarUI <- function(id) {
  page_fixed(
    titlePanel(h1("EpiPred Result Explorer", align = "center") ),
    
    br(),  
    accordion(
      accordion_panel(
        "Need help?",
        overall_help_text
      ),
      open = TRUE
    ),
    br(),
    
    # INPUT
    
    # Gene input header
    layout_columns(
      h2("Gene and Sequence Input"),
      col_widths = 12,
      style = 'border-bottom: 1px solid #c6c7c7'
    ),
    
    
    tags$style(HTML(
      "
      .btn-infoCustom {
        background-color: #4E2A84 !important; 
        color: #FFF !important; padding: 0px 5px
      }
      
      .btn-infoCustom:hover, .btn-infoCustom:active, .btn-infoCustom:visited {
        background-color: #B6ACD1 !important;
      }
      
      "
    )),
    layout_columns(
      card(
        card_body(
          p("Gene Select", style="text-align: center;"),
          selectInput(
            NS(id,"gene"),
            label = NULL,
            choices = genes_avail,
            selected = NULL
          ),
          style = "overflow: visible !important;"
        ),
        style = "overflow: visible !important;"
      ),
      col_widths = c(-4,4,-4)
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
      col_widths = c(-4, 4, -4)
    ),
    
    br(),
    
    # RESULTS
    
    # result viewer header
    layout_columns(
      h2("Result Viewer"),
      style='border-bottom: 1px solid #c6c7c7'
    ),
    
    # EpiPred colorbar plot
    card(
      # header
      card_header(
        class = "d-flex justify-content-between",
        div(
          "Your Sequence's EpiPred Score", HTML('&nbsp;'),
          tooltip(
            bs_icon("question-circle-fill", color = "grey"),
            colorbar_help_text1,
            placement = "right"
          )
        ),
      ),
      # body
      card_body(
        uiOutput(NS(id, "epipred_output_text")),
        plotOutput(NS(id,"epipred_bar"), height = 220), width = "10%",
        style = "text-align:center;"
      )
    ),
    
    layout_column_wrap(
      # spinning protein card
      card(
        card_header(
          class = "d-flex justify-content-between",
          div(
            "3-D Rendering of Protein", HTML('&nbsp;'),
            tooltip(
              bs_icon("question-circle-fill", color = "grey"),
              protein_help_text,
              placement = "right"
            )
          ),
          dropdownButton(
            strong("Toggle 3-D Viewer Spin"), br(),
            switchInput(NS(id,"NGL_spin"), value = TRUE, size = "small"),
            
            icon = bs_icon("gear"), width = "300px",
            right = TRUE,
            status = "infoCustom",
            circle = FALSE,
            size = "sm"
          )
        ),
        NGLVieweROutput(NS(id,"structure"))
      ),
      
      # distribution of score
      card(
        card_header(
          class = "d-flex justify-content-between",
          div(
            "EpiPred Score Distribution", HTML('&nbsp;'),
            tooltip(
              bs_icon("question-circle-fill", color = "grey"),
              distr_help_text,
              placement = "right"
            )
          ),
          dropdownButton(
            strong("Distribution Summary Type"), br(),
            radioButtons(
              NS(id, "epi_dist"), label = NULL,
              choices = c(
                "Class Proportion (Barplot)" = "barplot",
                "Distribution (Boxplot)" = "boxplot",
                "Distribution (Histogram)" = "histogram"
              ),
              selected = "barplot"
            ),
            
            icon = bs_icon("gear"), width = "300px",
            right = TRUE,
            status = "infoCustom",
            circle = FALSE,
            size = "sm"
          ),
        ),
        plotOutput(NS(id,"epi_score_distr"))
      ),
      width = 1/2
    )
  )

}

SingleVarServer <- function(id, mutations, gene, selected) {
  moduleServer(id, function(input, output, session) {
    colorbar_height <- 2/3
    
    # update gene reactive variable based on input
    observeEvent(input$gene,{
      gene(input$gene)
    })
    
    # reactively update gene input based on selection
    # this keeps the gene up to date with the other tab
    observe({
      req(!selected())
      updateSelectInput(
        session = session,
        inputId = "gene",
        label = NULL,
        selected = gene(),
        choices = genes_avail
      )
    })
    
    # used for getting epipred distribution for visualization
    epi_dist_summary <- reactive(get_epi_distribution_summary(mutations()))
    
    # create colorbar
    epipred_colorbar <- reactive(
      # create_epipred_colorbar2(
      #   bar_height = colorbar_height,
      #   epi_dist_summary = NULL,
      #   distribution_type = input$epi_dist
      # )
      create_epipred_colorbar(
        nbars = 10000,
        bar_height = colorbar_height
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
    
    # display prediction output as text
    output$epipred_output_text <- renderUI({
      HTML(
        paste0(
          "<br>",
          "<span>Selected Sequence ID: ", "<strong>", variant_id(), "</strong></span>", 
          "<span>Predicted Pathogenic Class: ", "<strong>", epipred_prediction()$class, "</strong></span>", 
          "<span>Pathogenic Score: ", "<strong>", round(epipred_prediction()$score,2), "</strong></span>"
        )
      )
    })
    
    # epipred colorbar output
    output$epipred_bar <- renderPlot({
      display_epipred_score(
        epipred_prediction = epipred_prediction(),
        epipred_colorbar = epipred_colorbar(),
        bar_height = colorbar_height,
        classification_label_type = 3
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
    
    # plot distribution of epipred score
    output$epi_score_distr <- renderPlot({
      if (input$epi_dist == "barplot") {
        plot_epi_distr_barplot(
          mutations = mutations(),
          epi_dist_summary = epi_dist_summary()
        )
      } else if (input$epi_dist == "boxplot") {
        plot_epi_distr_boxplot(
          mutations = mutations()
        )
      } else if (input$epi_dist == "histogram") {
        plot_epi_distr_histogram(
          mutations = mutations()
        )
      }
    }, res = 108)
  })
}

# Table Display -----------------------------------------------------------

# Module for displaying Table for a given gene
TableDisplayUI <- function(id) {
  page_fluid(
    
    # title
    titlePanel("Missense Variant Table"),
    
    # display mutation table filter selection and data table
    layout_columns(
      selectInput(NS(id,"class"),
                  "EpiPred Class:",
                  c("All",epipred_class)
      ),
      selectInput(NS(id,"report"),
                  "Reported:",
                  c("All",reported_sources)
      ),
      card(DT::dataTableOutput(NS(id,"table")), height = 600),
      
      # download button
      downloadButton(NS(id,"downloadData"), label = "Download"),
      col_widths = c(4,4,-4,12,12)
    )
  )
}

TableDisplayServer <- function(id, mutations, selected, dt_selected_index) {
  moduleServer(id, function(input, output, session) {
    # Filter data table based on selections
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
    
    # data table output
    output$table <- DT::renderDataTable({
      selectedData() %>%
        select(-id) %>%
        DT::datatable() %>% 
        formatStyle(colnames(selectedData() %>% select(-id)), "white-space" = "nowrap")
    })
    
    # update selected id
    observe({
      dt_selected_index(selectedData()[input$table_rows_selected,]$id)
    })
    
    
    # Downloadable csv of selected dataset
    output$downloadData <- downloadHandler(
      filename = function() {
        paste("Table_Out",Sys.Date(),".csv", sep = "")
      },
      content = function(file) {
        write.csv(selectedData() %>% select(-id), file, row.names = FALSE)
      }
    )
  })
}

# All Vars ----------------------------------------------------------------

# Module for exploring prediction result for all variants in a given gene
AllVarUI <- function(id) {
  page_fixed(
    # Title
    titlePanel("All Variants Summary"),
    
    accordion(
      accordion_panel(
        "Need help?",
        allvar_help_text
      ),
      open = TRUE
    ),
    
    br(),
    
    layout_sidebar(
      
      # Controls for plot
      sidebar = sidebar(
        selectInput(
          NS(id,"gene"),
          label = strong("Gene"),
          choices = genes_avail,
          selected = NULL
        ),
        selectInput(NS(id,"var1"), strong("x Variable"), choices = scatterplot_vars, selected = scatterplot_vars[1]),
        selectInput(NS(id,"var2"), strong("y Variable"), choices = scatterplot_vars, selected = scatterplot_vars[2]),
        selectInput(NS(id,"margin_type"), strong("Margin Plot Type"), choices = c("density", "histogram", "boxplot", "violin", "densigram")),
        radioButtons(NS(id,"color_group"), strong("Color By"), choices = c("Reported" = "Reported", "EpiPred Class" = "EpiPred_Class")),
        checkboxGroupInput(NS(id,"report"), strong("Reported"), choices = report_source, selected = report_source),
        bg = "#f5f5f5"
      ),
      
      # Display plot output
      card(
        plotOutput(NS(id,"marginal_plot"))
      )
    )
  )
}

AllVarServer <- function(id, mutations, gene, selected, dt_selected_index) {
  moduleServer(id, function(input,output,session) {
    
    # update gene reactive variable based on input
    observeEvent(input$gene,{
      gene(input$gene)
    })
    
    # reactively update gene input based on selection
    # this keeps the gene up to date with the other tab
    observe({
      req(!selected())
      updateSelectInput(
        session = session,
        inputId = "gene",
        label = NULL,
        selected = gene(),
        choices = genes_avail
      )
    })
    
    # marginal plot output
    output$marginal_plot <- renderPlot({
      req(length(input$report) > 0)
      marginal_plot(
        mutations = mutations(),
        var1 = input$var1,
        var2 = input$var2,
        margin_type = input$margin_type,
        reported = input$report,
        color_group = input$color_group,
        highlight_id = dt_selected_index()
      )
    })
  })
}
