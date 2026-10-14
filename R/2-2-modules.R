# load help texts
source("R/2-1-help-texts.R")

# Home UI -----------------------------------------------------------------

# Module for the Home Tab
HomeUI <- function(id) {
  ns <- NS(id)
  page_fixed(
    layout_columns(
      card(
        div(
          shiny::img(src = "epimvp-logo.jpg", height = "150px"),
          shiny::img(src = "epipred-logo.png", height = "150px"),
          style = "display: flex; justify-content: center; gap:75px; align-items: center;"
        ),
        br(),
        card_body(home_text)
      )
    ),

    br(),

    # Gene selector + per-gene ACMG class waffle plot
    card(
      card_header("Variant Classification by Gene"),
      div(
        GeneSelectorUI(ns("gene_selector")),
        style = "display:flex; justify-content:center;"
      ),
      plotOutput(ns("class_waffle"), height = "500px"),
      # Static text explaining the classes (same for all genes)
      card_body(landing_legend_text)
    )
  )
}

# Server for the landing page: mounts the shared gene selector and renders
# the per-gene ACMG class waffle plot. `mutations` is the app-wide reactive
# already filtered to the current gene (same one every other tab uses).
HomeServer <- function(id, mutations, gene) {
  moduleServer(id, function(input, output, session) {
    GeneSelectorServer("gene_selector", gene)

    output$class_waffle <- renderPlot({
      class_waffle_plot(gene(), mutations())
    })
  })
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
    
    # Gene selection
    p(strong("Select Gene"), style = "text-align: center;"),
    GeneSelectorUI(NS(id, "gene_selector")),

    # Sequence selection input
    layout_columns(

      # Amino Acid Sequence input
      card(
        card_body(
          p(strong("Input Amino Acid Sequence"), style="text-align: center;"),
          selectizeInput(
            NS(id,"val"),
            label = NULL,
            selected = aa_id_default_,
            choices = aa_id_default_
          ),
          class = "align-items-center",
          style = "overflow: visible !important;",
          align = "center"
        ),
        style = "overflow: visible !important;"
      ),
      col_widths = c(-4, 4, -4)
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
        # falls back to a message if this gene's .pdb file is missing
        conditionalPanel(
          condition = "output.pdb_available == true",
          ns = NS(id),
          NGLVieweROutput(NS(id,"structure"))
        ),
        conditionalPanel(
          condition = "output.pdb_available == false",
          ns = NS(id),
          div(
            "3-D structure not yet available for this gene.",
            style = "text-align:center; color:#888; padding: 60px 20px;"
          )
        )
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
                "Distribution by Class (Histogram)" = "class_histogram",
                "Distribution (Boxplot)" = "boxplot",
                "Distribution (Histogram)" = "histogram"
              ),
              selected = "class_histogram"
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

SingleVarServer <- function(id, mutations, gene) {
  moduleServer(id, function(input, output, session) {
    colorbar_height <- 2/3
    
    # Gene selection
    GeneSelectorServer("gene_selector", gene)
    
    # Whether this gene's PDB file exists on disk
    pdb_path <- reactive({
      f <- pdbfile_map_[[gene()]]
      if (is.null(f)) NA_character_ else f
    })
    pdb_available <- reactive(!is.na(pdb_path()) && file.exists(pdb_path()))
    output$pdb_available <- reactive(pdb_available())
    outputOptions(output, "pdb_available", suspendWhenHidden = FALSE)

    # Get Amino Acid Sequence IDs
    aa_seq_ids <- reactive({
      mutations() %>%
        pull(variant_id)
    })
    
    observe({
      # server-side selectize: some genes have ~10k variant IDs, too many
      # to send to the browser up front
      updateSelectizeInput(
        session = session,
        inputId = "val",
        label = NULL,
        selected = aa_seq_ids()[1],
        choices = aa_seq_ids(),
        server = TRUE
      )
    })
    
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

    # Whether the last Submit failed to match a variant. Cleared on a gene
    # switch so a stale "not found" warning doesn't linger.
    snp_search_failed_ <- reactiveVal(FALSE)

    # runs only on a Submit click, not on gene switches
    observeEvent(input$update, {
      candidate <- variant_id_tmp()
      if (candidate %in% isolate(mutations())$variant_id) {
        variant_id(candidate)
        snp_search_failed_(FALSE)
      } else {
        snp_search_failed_(TRUE)
      }
    }, ignoreInit = TRUE)

    # Select the gene's first variant whenever the gene changes, replacing
    # the previous gene's variant_id(). A manual Submit still overrides it.
    observe({
      default_id <- aa_seq_ids()[1]
      if (length(default_id) > 0 && !is.na(default_id)) {
        variant_id(default_id)
        snp_search_failed_(FALSE)
      }
    })
    
    output$snp_not_found <- reactive({
      snp_search_failed_()
    })
    outputOptions(output, "snp_not_found", suspendWhenHidden = FALSE)
    
    output$snp_not_found_text <- renderUI(
      span(sprintf("* Variant '%s' not found.", variant_id_tmp()), style = "color:red;font-size:12px;display: table; margin: 0 auto;")
    )
    
    # AA sequence can arise from different SNPs
    # If this is the case, have the user specify the SNP
    output$multiple_snps <- reactive({
      snp_n <- sum(mutations()$variant_id == variant_id())
      if (snp_n > 1) {
        return(TRUE)
      } else {
        return(FALSE)
      }
    })
    outputOptions(output, "multiple_snps", suspendWhenHidden = FALSE)
    
    # variant_key values (individual SNPs) that produce the selected
    # variant_id()'s amino acid change
    unique_id_choices <- reactive({
      req(variant_id())

      mutations() %>%
        filter(variant_id == variant_id()) %>%
        pull(variant_key)
    })

    observe({
      updateSelectInput(
        session = session,
        inputId = "snp_id",
        choices = unique_id_choices()
      )
    })
    
    # retrieve prediction result for chosen variant
    epipred_prediction <- reactive({
      # Right after a gene switch, variant_id() can briefly still hold the
      # previous gene's variant, and input$snp_id lags until the browser
      # reports the new "Select Mutation ID" choices. Wait for both to
      # match the current data instead of erroring.
      req(variant_id() %in% mutations()$variant_id)
      req(input$snp_id %in% unique_id_choices())
      get_epipred_prediction(mutations(), isolate(variant_id()), input$snp_id)
    })
    
    gnomad_info <- reactive({
      # same guards as epipred_prediction() above
      req(variant_id() %in% mutations()$variant_id)
      req(input$snp_id %in% unique_id_choices())
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
        
        allele_sentence <- sprintf(
          "<strong>%s</strong> out of <strong>%s</strong> alleles %s this mutation",
          alt_count, total_count, has_have
        )
        
        # gnomAD v4 is hg38-native, so only link for hg38 data (STXBP1);
        # hg19 genes get the same information as plain text
        if (identical(gnomad_info()$genome_build, "hg38")) {
          gnomad_link <- sprintf(
            "https://gnomad.broadinstitute.org/variant/%s?dataset=gnomad_r4",
            extract_variant_id(gnomad_info()$variant_key)
          )
          gnomad_string <- sprintf(
            "<span>According to the <a href=\"%s\" target=\"_blank\">gnomAD database</a>, %s.</span>",
            gnomad_link, allele_sentence
          )
        } else {
          gnomad_string <- sprintf("<span>According to gnomAD, %s.</span>", allele_sentence)
        }
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
      req(pdb_available())
      NGLVieweR(pdb_path()) %>%
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
      req(pdb_available())
      NGLVieweR_proxy("structure") %>%
        updateSpin(input$NGL_spin)
    })
    
    # when variant is updated, highlight the affected residue. Uses
    # aa_pos_structure (the residue number in the PDB file), which differs
    # from AA_POS for PCDH19.
    observeEvent(input$update, {
      req(pdb_available())
      aa_pos <- mutations() %>%
        filter(variant_id %in% variant_id()) %>%
        pull(aa_pos_structure) %>% 
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
    
    # plot distribution of epipred score
    output$epi_score_distr <- renderPlot({
      if (input$epi_dist == "class_histogram") {
        # cutoff lines match the colorbar's (ambiguous_range_list_)
        plot_epi_distr_class_histogram(
          mutations = mutations(),
          predicted_score = epipred_prediction()$score,
          ambiguous_range = ambiguous_range_list_[[gene()]]
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

TableDisplayServer <- function(id, mutations, dt_selected_index) {
  moduleServer(id, function(input, output, session) {
    # Filter data table based on selections
    selectedData <- reactive({
      data <- mutations() %>% 
        data.frame()
      if (input$class != "All") {
        data <- data[data$epipred_band == input$class,]
      }
      if (input$report != "All") {
        data <- data[data$primary_class == input$report,]
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
    titlePanel("Comparison of VEPs and other gene features"),
    
    accordion(
      accordion_panel(
        "Need help?",
        allvar_help_text
      ),
      open = TRUE
    ),
    
    br(),
    
    # Heatmap of predicted variant effect (uses the scatterplot's gene
    # selector below)
    card(
      card_header("Heatmap of Predicted Variant Effect"),
      plotly::plotlyOutput(NS(id, "heatmap"), height = "500px")
    ),
    
    br(),
    
    layout_sidebar(
      
      # Controls for plot
      sidebar = sidebar(
        strong("Gene"),
        GeneSelectorUI(NS(id, "gene_selector")),
        selectInput(NS(id,"var1"), strong("x Variable"), choices = scatterplot_vars_, selected = scatterplot_vars_[1]),
        selectInput(NS(id,"var2"), strong("y Variable"), choices = scatterplot_vars_, selected = scatterplot_vars_[2]),
        selectInput(NS(id,"margin_type"), strong("Margin Plot Type"), choices = c("density", "histogram", "boxplot", "violin", "densigram")),
        selectInput(
          NS(id,"color_group"),
          strong("Color By"),
          choices = c(
            "Variant Class" = "primary_class",
            "EpiPred Class" = "epipred_band",
            "GroupMax Genetic Ancestry" = "gnomAD_ancestry_group"
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
    
    # display warning output outside the plot area
    div(
      verbatimTextOutput(NS(id,"allvar_plot_text"), FALSE),
      class = "warning-class"
    )
  )
}

AllVarServer <- function(id, mutations, gene, dt_selected_index) {
  moduleServer(id, function(input,output,session) {
    
    # Gene selection
    GeneSelectorServer("gene_selector", gene)
    
    # Heatmap uses all variants, not filtered by the "Reported" checkboxes
    output$heatmap <- plotly::renderPlotly({
      class_heatmap_plot(gene(), mutations())
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
      mutations_subset <- mutations() %>% filter(primary_class %in% input$report)
      
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
