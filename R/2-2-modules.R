# load help texts
source("R/2-1-help-texts.R")

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
    tags$head(tags$style(HTML(sprintf("#%s-val {text-align: center;}", id)))),
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
      h2("Gene and Sequence Input", align = "center"),
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
    
    # Sequence selection input
    layout_columns(
      
      # select input gene
      card(
        card_body(
          p(strong("Gene Select")),
          selectInput(
            NS(id,"gene"),
            label = NULL,
            choices = genes_avail,
            selected = NULL
          ),
          class = "align-items-center",
          style = "overflow: visible !important;"
        ),
        style = "overflow: visible !important;"
      ),
      
      # Text Input Amino Acid Sequence
      card(
        card_body(
          p(strong("Input Amino Acid Sequence"), style="text-align: center;"),
          textInput(NS(id,"val"), label = NULL, value = "A2P"),
          class = "align-items-center",
          align = "center"
        )
      ),
      col_widths = c(-1, 5,5, -1)
    ),
    
    # Submit button
    layout_columns(
      actionButton(NS(id,"update"), "Submit", class = "btn btn-primary"),
      col_widths = c(-5,2,-5)
    ),
    
    conditionalPanel(
      condition = "output.multiple_snps == true",
      ns = NS(id),
      layout_columns(
        span("* The sequence can arise from multiple mutations.
             Please select your mutation below.", style = "color:red;font-size:12px"),
        col_widths = c(-2, 8, -2)
      )
    ),
    
    br(),
    
    # RESULTS
    
    # result viewer header
    layout_columns(
      h2("Result Viewer", align = "center"),
      style='border-bottom: 1px solid #c6c7c7'
    ),
    
    # sequence info and colorbar
    layout_column_wrap(
      width = NULL,
      style = css(grid_template_columns = "1fr 2fr"),
      
      # sequence information in text
      card(
        card_header(
          class = "d-flex justify-content-between",
          div(
            "Sequence Info", HTML('&nbsp;'),
            tooltip(
              bs_icon("question-circle-fill", color = "grey"),
              sequence_info_help_text,
              placement = "right"
            )
          ),
        ),
        "Unique Mutation ID:",
        selectizeInput(
          NS(id, "snp_id"),
          label = NULL,
          width = "100%",
          choices = NULL
        ),
        uiOutput(NS(id, "epipred_output_text")),
        style = "overflow: visible !important;"
      ),
      
      # EpiPred colorbar plot
      card(
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
      create_epipred_colorbar(
        nbars = 10000,
        bar_height = colorbar_height
      )
    )
    
    # update variant ID on update button press
    # only update if the variant id exists
    variant_id_tmp <- reactive({
      input$update
      isolate(toupper(input$val))
    })
    variant_id <- reactiveVal()
    observe({
      if (variant_id_tmp() %in% mutations()$One_letter_Amino_Acid_change) {
        variant_id(variant_id_tmp())
      }
    })
    
    # AA sequence can arise from different SNPs
    # If this is the case, have the user specify the SNP
    output$multiple_snps <- reactive({
      snp_n <- sum(mutations()$One_letter_Amino_Acid_change == variant_id())
      if (snp_n > 1) {
        return(TRUE)
      } else {
        return(FALSE)
      }
    })
    outputOptions(output, "multiple_snps", suspendWhenHidden = FALSE)
    
    observe({
      unique_id_choices <- mutations() %>%
        filter(One_letter_Amino_Acid_change == variant_id()) %>%
        pull(hg38_uniq_ID)
      
      updateSelectizeInput(
        session = session,
        inputId = "snp_id",
        choices = unique_id_choices
      )
    })
    
    # retrieve prediction result for chosen variant
    epipred_prediction <- reactive({
      get_epipred_prediction(isolate(variant_id()), mutations(), input$snp_id)
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
        filter(One_letter_Amino_Acid_change %in% variant_id()) %>%
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
          "<span>Sequence ID: ", "<strong>", variant_id(), "</strong></span>", 
          "<span>Predicted Class: ", "<strong>", epipred_prediction()$class, "</strong></span>", 
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
          epi_dist_summary = epi_dist_summary(),
          all_classes = epipred_class_,
          predicted_class = epipred_prediction()$class
        )
      } else if (input$epi_dist == "boxplot") {
        plot_epi_distr_boxplot(
          mutations = mutations(),
          predicted_score = epipred_prediction()$score
        )
      } else if (input$epi_dist == "histogram") {
        plot_epi_distr_histogram(
          mutations = mutations(),
          predicted_score = epipred_prediction()$score
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
                  c("All",epipred_class_)
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
        data <- data[data$epipred_prediction == input$class,]
      }
      if (input$report != "All") {
        data <- data[data$new_class == input$report,]
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
        radioButtons(NS(id,"color_group"), strong("Color By"), choices = c("Variant Class" = "new_class", "EpiPred Class" = "epipred_prediction")),
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
