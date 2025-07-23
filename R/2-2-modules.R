# load help texts
source("R/2-1-help-texts.R")

# Home UI -----------------------------------------------------------------

# Module for the Home Tab
HomeUI <- function(id) {
  page_fixed(
    layout_columns(
      card(
        div(
          shiny::img(src = "epimvp-logo.png", height = "150px"),
          shiny::img(src = "epipred-logo.jpg", height = "150px"),
          style = "display: flex; justify-content: center; gap:75px; align-items: center;"
        ),
        br(),
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
    # display main help text. default is to show.
    # to hide as default, set open = FALSE
    accordion(
      accordion_panel(
        "Need help?",
        patients_help_text
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
          p(strong("Select Gene")),
          selectInput(
            NS(id,"gene"),
            label = NULL,
            choices = genes_avail_,
            selected = NULL
          ),
          class = "align-items-center",
          style = "overflow: visible !important;",
          align = "center"
        ),
        style = "overflow: visible !important;"
      ),
      
      # Text Input Amino Acid Sequence
      card(
        card_body(
          p(strong("Input Amino Acid Sequence"), style="text-align: center;"),
          selectizeInput(
            NS(id,"val"),
            label = NULL,
            selected = aa_id_default_,
            choices = aa_id_default_
          ),
          # textInput(NS(id,"val"), label = NULL, value = "A2P"),
          class = "align-items-center",
          style = "overflow: visible !important;",
          align = "center"
        ),
        style = "overflow: visible !important;"
      ),
      col_widths = c(-1, 5,5, -1)
    ),
    
    # Submit button
    layout_columns(
      actionButton(NS(id,"update"), "Submit", class = "btn btn-primary"),
      col_widths = c(-5,2,-5)
    ),
    
    conditionalPanel(
      condition = "output.snp_not_found == true",
      ns = NS(id),
      layout_columns(
        uiOutput(NS(id, "snp_not_found_text")),
        col_widths = c(-2, 8, -2)
      )
    ),
    
    conditionalPanel(
      condition = "output.multiple_snps == true",
      ns = NS(id),
      layout_columns(
        span("* The sequence can arise from multiple mutations.
             Please select your mutation below from \"Select Mutation ID\" card.",
             style = "color:red;font-size:14px;display: table; margin: 0 auto;"),
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
      
      # variant ID selection card
      card(
        card_header(
          class = "d-flex justify-content-between",
          div(
            "Select Mutation ID", HTML('&nbsp;'),
            tooltip(
              bs_icon("question-circle-fill", color = "grey"),
              select_mutation_help_text,
              placement = "right"
            )
          ),
        ),
        card_body(
          p(
            a("How do I locate Variant ID?", href = var_id_search_url_, target = "_blank"),
            style = "font-size: 14px;"
          ),
          selectInput(
            NS(id, "snp_id"),
            label = NULL,
            width = "100%",
            choices = NULL
          ),
          style = "overflow: visible !important; justify-content: center;"
        ),
        style = "overflow: visible !important;",
      ),
      
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
        uiOutput(NS(id, "epipred_output_text")),
        style = "overflow: visible !important;"
      )
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
      style = "text-align:center; justify-content: center;"
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
              placement = "right",
              options = list(
                "customClass" = "testing"
              )
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
        choices = genes_avail_
      )
    })
    
    # Get Amino Acid Sequnce IDs
    aa_seq_ids <- reactive({
      mutations() %>%
        pull(One_letter_Amino_Acid_change)
    })
    
    observe({
      updateSelectizeInput(
        session = session,
        inputId = "val",
        label = NULL,
        selected = aa_seq_ids()[1],
        choices = aa_seq_ids()
      )
    })
    
    # used for getting epipred distribution for visualization
    epi_dist_summary <- reactive(get_epi_distribution_summary(mutations()))
    
    # create colorbar
    epipred_colorbar <- reactive(
      create_epipred_colorbar(
        nbars = 10000,
        bar_height = colorbar_height,
        middle_color = "grey90",
        gradient_transform_function = function(x) transform_function_1(x, ambiguous_range_list_[[gene()]])
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
    
    output$snp_not_found <- reactive({
      if (!variant_id_tmp() %in% mutations()$One_letter_Amino_Acid_change) {
        return(TRUE)
      } else {
        return(FALSE)
      }
    })
    outputOptions(output, "snp_not_found", suspendWhenHidden = FALSE)
    
    output$snp_not_found_text <- renderUI(
      span(sprintf("* Variant '%s' not found.", variant_id_tmp()), style = "color:red;font-size:12px;display: table; margin: 0 auto;")
    )
    
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
      
      updateSelectInput(
        session = session,
        inputId = "snp_id",
        choices = unique_id_choices
      )
    })
    
    # retrieve prediction result for chosen variant
    epipred_prediction <- reactive({
      get_epipred_prediction(mutations(), isolate(variant_id()), input$snp_id)
    })
    
    gnomad_info <- reactive({
      get_gnomad_maf(mutations(), isolate(variant_id()), input$snp_id)
    })
    
    # display prediction output as text
    output$epipred_output_text <- renderUI({
      output_string <- paste0(
          "<span>Sequence ID: ", "<strong>", variant_id(), "</strong>", 
          "<br>Predicted Class: ", "<strong>", epipred_class_labels_[epipred_prediction()$class == epipred_class_], "</strong>", 
          "<br>Pathogenic Score: ", "<strong>", round(epipred_prediction()$score,2), "</strong></span>"
        )
      
      if (gnomad_info()$full_allele_info) {
        alt_count <- gnomad_info()$allele_count
        total_count <- gnomad_info()$allele_number
        has_have <- ifelse(alt_count == 1, "has", "have")
        
        gnomad_link <- sprintf(
          "https://gnomad.broadinstitute.org/variant/%s?dataset=gnomad_r4",
          extract_variant_id(isolate(input$snp_id))
        )
        gnomad_string <- sprintf(
          paste0("<span>According to the ",
                 "<a href=\"%s\" target=\"_blank\">gnomAD database</a>, ",
                 "<strong>%s</strong> out of <strong>%s</strong> alleles %s this mutation.</span>"), 
          gnomad_link, alt_count, total_count, has_have
        )
        output_string <- paste0(output_string, gnomad_string)
      }
      
      return(HTML(output_string))
    })
    
    # epipred colorbar output
    output$epipred_bar <- renderPlot({
      display_epipred_score(
        epipred_prediction = epipred_prediction(),
        epipred_colorbar = epipred_colorbar(),
        bar_height = colorbar_height,
        classification_label_type = 3,
        line_center = FALSE,
        line_width = 3.5,
        ambiguous_range = ambiguous_range_list_[[gene()]]
      )
    }, height = 230)
    
    # NGLViewer Output
    output$structure <- renderNGLVieweR({
      NGLVieweR(pdbfile_map_[[gene()]]) %>%
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
          class_labels = epipred_class_labels_,
          predicted_class = epipred_prediction()$class
        )
      } else if (input$epi_dist == "boxplot") {
        plot_epi_distr_boxplot(
          mutations = mutations(),
          predicted_score = epipred_prediction()$score,
          n_panels = 50,
          panel_seq_from = 0,
          panel_seq_to = 1
        )
      } else if (input$epi_dist == "histogram") {
        plot_epi_distr_histogram(
          mutations = mutations(),
          predicted_score = epipred_prediction()$score,
          n_panels = 50,
          panel_seq_from = -0.05,
          panel_seq_to = 1.05
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
                  c("All", epipred_class_)
      ),
      selectInput(NS(id,"report"),
                  "Reported:",
                  c("All", reported_sources_)
      ),
      card(DT::dataTableOutput(NS(id,"table")), height = 600),
      
      # download button
      downloadButton(NS(id,"downloadData"), label = "Download"),
      col_widths = c(3,3,-6,12,12)
    )
  )
}

TableDisplayServer <- function(id, mutations, selected, dt_selected_index) {
  moduleServer(id, function(input, output, session) {
    # Filter data table based on selections
    selectedData <- reactive({
      data <- mutations() %>% 
        data.frame()
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
        write.csv(
          selectedData() 
          %>% select(all_of(mutations_colnames_)), 
          file, row.names = FALSE
        )
      }
    )
  })
}

# All Vars ----------------------------------------------------------------

# Module for exploring prediction result for all variants in a given gene
AllVarUI <- function(id) {
  page_fixed(
    tags$head(
      tags$style(HTML("
        .warning-class pre {
          color: red;
        }                
      "))
    ),
    
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
          choices = genes_avail_,
          selected = NULL
        ),
        selectInput(NS(id,"var1"), strong("x Variable"), choices = scatterplot_vars_, selected = scatterplot_vars_[1]),
        selectInput(NS(id,"var2"), strong("y Variable"), choices = scatterplot_vars_, selected = scatterplot_vars_[2]),
        selectInput(NS(id,"margin_type"), strong("Margin Plot Type"), choices = c("density", "histogram", "boxplot", "violin", "densigram")),
        selectInput(
          NS(id,"color_group"),
          strong("Color By"),
          choices = c(
            "Variant Class" = "new_class",
            "EpiPred Class" = "epipred_prediction",
            "GroupMax Genetic Ancestry" = "GroupMax.FAF.group"
          )),
        checkboxGroupInput(NS(id,"report"), strong("Reported"), choices = report_source_, selected = report_source_),
        checkboxInput(NS(id,"var_with_mac"), strong(" Limit to Variants with Allele Count"), value = FALSE),
        bg = "#f5f5f5"
      ),
      
      # Display plot output
      card(
        plotOutput(NS(id,"marginal_plot"))
      ),
      
      height = "100%"
    ),
    
    # display warning output ouside the plot area
    div(
      verbatimTextOutput(NS(id,"allvar_plot_text"), FALSE),
      class = "warning-class"
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
        choices = genes_avail_
      )
    })
    
    # since there can be missing values, if the subset data has missing values,
    # display a warning message
    # plot_items holds number of observations removed due to missing value
    # after filtering step, and the final data used for plotting
    plot_items <- reactiveValues(
      n_miss_counts = 0,
      plot_data = data.frame(),
      var1 = "",
      var2 = "",
      x_log_scale = FALSE,
      y_log_scale = FALSE
    )
    
    # filter data and prepare inputs for plotting based on user input
    observe({
      # subset mutations data based on reported source
      mutations_subset <- mutations() %>% filter(new_class %in% input$report)
      
      # filter out variants with missing allele count
      if (input$var_with_mac) {
        mutations_subset <- mutations_subset %>% filter(!is.na(gnomAD_AlleleCount))
      }
      
      # if log allele count is chosen, log transform
      if (input$var1 == "log_allele_count") {
        plot_items$var1 <- "gnomAD_AlleleCount"
        plot_items$x_log_scale <- TRUE
      } else {
        plot_items$var1 <- input$var1
        plot_items$x_log_scale <- FALSE
      }
      if (input$var2 == "log_allele_count") {
        plot_items$var2 <- "gnomAD_AlleleCount"
        plot_items$y_log_scale <- TRUE
      } else {
        plot_items$var2 <- input$var2
        plot_items$y_log_scale <- FALSE
      }
      
      # filter out missing values
      mutations_final <- mutations_subset %>%
        filter(!is.na(.data[[plot_items$var1]]) & !is.na(.data[[plot_items$var2]]))
      
      # update reactive values
      # calculate number of missing values removed
      plot_items$n_miss_counts <- nrow(mutations_subset) - nrow(mutations_final)
      # save final data
      plot_items$plot_data <- mutations_final
    })
    
    # display warning message if missing values were removed
    output$allvar_plot_text <- renderText({
      if (plot_items$n_miss_counts > 0) {
        return(sprintf("Warning: %s rows with missing values in %s or %s were removed.", plot_items$n_miss_counts, input$var1, input$var2))
      } else {
        return(NULL)
      }
    })
    
    # marginal plot output
    output$marginal_plot <- renderPlot({
      # only run when data is available
      req(length(input$report) > 0)
      req(nrow(plot_items$plot_data) > 0)

      marginal_plot(
        mutations = plot_items$plot_data,
        var1 = plot_items$var1,
        var2 = plot_items$var2,
        margin_type = input$margin_type,
        color_group = input$color_group,
        highlight_id = dt_selected_index(),
        x_log_scale = plot_items$x_log_scale,
        y_log_scale = plot_items$y_log_scale
      )
    })
  })
}
