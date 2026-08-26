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

    # Gene selector + per-gene ACMG class waffle plot (TODO.md Phase 2).
    # "EpiPred genes -- these could maybe be buttons... all figures will
    # be the same but pull from different data" (V2 landing page.docx).
    card(
      card_header("Variant Classification by Gene"),
      div(
        GeneSelectorUI(ns("gene_selector")),
        style = "display:flex; justify-content:center;"
      ),
      plotOutput(ns("class_waffle"), height = "500px"),
      # Static text explaining the classes -- doesn't change per gene
      # ("the text in the text box will be static and apply to all
      # genes, it's just an explanation of the data" -- same doc).
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
    
    # Gene selection (shared component -- TODO.md Phase 1)
    p(strong("Select Gene"), style = "text-align: center;"),
    GeneSelectorUI(NS(id, "gene_selector")),

    # Sequence selection input
    layout_columns(

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
        # All 5 genes currently have a .pdb file mapped (see pdbfile_map_
        # in R/1-setup.R) -- STXBP1's is a real structure, the other 4 are
        # temporary rendering placeholders (not the genes' real proteins)
        # pending real files. Still checking file.exists() / showing the
        # friendly fallback below rather than assuming, so this degrades
        # gracefully instead of letting NGLVieweR error if a path here
        # ever goes missing (e.g. a future gene added without a file yet).
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

SingleVarServer <- function(id, mutations, gene, selected) {
  moduleServer(id, function(input, output, session) {
    colorbar_height <- 2/3
    
    # Gene selection (shared component -- TODO.md Phase 1). Replaces the old
    # selectInput + observeEvent/updateSelectInput pairing below (kept only
    # in git history): GeneSelectorServer both updates gene() when a button
    # here is clicked, and keeps this tab's button row in sync when gene()
    # changes from elsewhere (e.g. the For Researchers tab's copy of this
    # same component). `selected` (whether this tab is the active nav
    # panel) is no longer needed for gene sync as a result, but is left in
    # the function signature since app.R still passes it positionally.
    GeneSelectorServer("gene_selector", gene)
    
    # Whether a PDB structure file actually exists on disk for the current
    # gene. Checking file.exists() (not just "is this gene in the map")
    # means the 3D viewer starts working automatically the moment a
    # gene's .pdb file is dropped into data/pdb/ -- no code change needed.
    pdb_path <- reactive({
      f <- pdbfile_map_[[gene()]]
      if (is.null(f)) NA_character_ else f
    })
    pdb_available <- reactive(!is.na(pdb_path()) && file.exists(pdb_path()))
    output$pdb_available <- reactive(pdb_available())
    outputOptions(output, "pdb_available", suspendWhenHidden = FALSE)

    # Get Amino Acid Sequnce IDs
    aa_seq_ids <- reactive({
      mutations() %>%
        pull(variant_id)
    })
    
    observe({
      # server = TRUE switches this to server-side selectize: the full
      # choice vector (up to ~10.8k variant IDs for CHD2) stays on the
      # server, and the client only requests matches as the user types,
      # instead of every ID being pushed into the browser's DOM/JS index
      # up front. This is exactly what the "large number of options...
      # consider using server-side selectize" warning was asking for --
      # eliminates the warning and the client-side slowdown it described.
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

    # Whether the user's most recent manual "type an ID + Submit" attempt
    # failed to match anything. Kept as its own reactiveVal (rather than
    # computed straight from variant_id_tmp()/mutations()) so it can be
    # explicitly cleared on a gene switch below -- otherwise a stale failed
    # search from a previous gene would keep showing a "not found" warning
    # on top of the newly auto-selected, perfectly valid default variant.
    snp_search_failed_ <- reactiveVal(FALSE)

    # ignoreInit = TRUE + triggering only on input$update (not on
    # mutations()/gene changing) means this only runs on an actual Submit
    # click, never as a side effect of switching genes.
    observeEvent(input$update, {
      candidate <- variant_id_tmp()
      if (candidate %in% isolate(mutations())$variant_id) {
        variant_id(candidate)
        snp_search_failed_(FALSE)
      } else {
        snp_search_failed_(TRUE)
      }
    }, ignoreInit = TRUE)

    # Default behavior: automatically select and display a real variant for
    # whichever gene is currently active, rather than requiring the user to
    # type an ID and press Submit before seeing anything. This re-fires
    # every time aa_seq_ids() changes -- i.e. whenever the shared gene
    # selector switches genes -- so it also replaces any stale variant_id()
    # left over from the previously-selected gene (which would otherwise
    # not exist in the new gene's data and could crash the lookup below),
    # and clears any stale "not found" warning from a previous manual
    # search. The manual "type an ID + Submit" flow above still works and
    # takes precedence whenever the user actually uses it, since that
    # observer only re-runs on a button click, not on every gene switch.
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
    
    # The set of variant_key values (individual SNPs) that all produce the
    # currently-selected variant_id()'s amino acid change. Pulled out into
    # its own named reactive (rather than an inline expression, as before)
    # so both the updateSelectInput() call below AND the req() guards on
    # epipred_prediction()/gnomad_info() further down can share the exact
    # same list -- see the comment on those guards for why that sharing
    # matters.
    unique_id_choices <- reactive({
      # variant_id() is normally set automatically as soon as a gene has
      # data (see the default-selection observer above), so this req() is
      # mostly a defensive fallback for the unlikely case of a gene with
      # zero variants -- without it, filter(variant_id == variant_id())
      # would error when variant_id() is NULL (dplyr requires the RHS to
      # be length 1).
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
    # req(variant_id()) is a defensive fallback for the same near-zero-risk
    # case as above (a gene with zero variants): without it, a NULL
    # variant_id() reaching get_epipred_prediction()/get_gnomad_maf() would
    # crash with an opaque, near-blank error (0-row match -> stop(sprintf(
    # ..., NULL, ...)), and sprintf() with a NULL argument returns
    # character(0), so the resulting error has no message at all). In normal
    # operation variant_id() is always set automatically (see the
    # default-selection observer above), so this should never actually
    # trigger -- but if it ever did, req() makes these reactives, and
    # everything downstream (output$epipred_bar, output$epi_score_distr,
    # etc.), silently hold off rendering rather than crash.
    epipred_prediction <- reactive({
      # req() guards against variant_id() being NULL/blank, but that alone
      # isn't enough: right after a gene switch there's a brief window
      # where variant_id() can still hold the *previous* gene's variant
      # while mutations() has already updated to the new gene (the
      # auto-default observer that corrects variant_id() hasn't run yet
      # within this same reactive flush). Without this extra membership
      # check, get_epipred_prediction() would stop() on that stale/mismatched
      # ID -- a real (non-silent) error that briefly flashes red error text
      # in the browser before the corrected variant_id() re-triggers this
      # reactive a moment later. Checking membership makes this reactive
      # silently (invisibly) suspend during that transient window instead.
      req(variant_id() %in% mutations()$variant_id)
      # Same idea, but for input$snp_id: whenever variant_id() changes (a
      # gene switch, a fresh auto-default, OR -- the case that used to
      # fire on literally every single Submit click -- the manual search
      # button above), the unique_id_choices() observer pushes a brand
      # new set of choices to the "Select Mutation ID" dropdown via
      # updateSelectInput(). That's a real round trip to the browser and
      # back: the client has to receive the new choices, pick one (the
      # single/first one, since no selected= is given), and report that
      # value back as the new input$snp_id. Until that round trip
      # completes, input$snp_id server-side is still whatever it was
      # before -- on first page load that's "" (the <select> starts with
      # choices = NULL in the UI), and after a variant switch it's the
      # PREVIOUS variant's key -- neither of which matches any variant_key
      # for the newly-selected variant_id(). select_variant() treats a
      # non-NULL unique_id as an exact-match filter (variant_id == var_id
      # & variant_key == unique_id), so that stale/blank value produced
      # zero rows and get_epipred_prediction() stop()'d -- a real error,
      # visible as red text, every time. Requiring input$snp_id to
      # actually be one of the current choices makes this reactive wait
      # (silently) for that round trip to land instead.
      req(input$snp_id %in% unique_id_choices())
      get_epipred_prediction(mutations(), isolate(variant_id()), input$snp_id)
    })
    
    gnomad_info <- reactive({
      # Same guards as epipred_prediction() above -- see comments there.
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
        
        # gnomAD v4 is hg38-native, so a v4 link only makes sense for
        # hg38-coordinate data (currently just STXBP1 -- the 4 newer genes
        # are hg19-based, see extract_variant_id()'s comment in
        # R/3-utils.R). Show the same information as plain text for those
        # instead of a clickable link that would point at the wrong variant.
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
    
    # when variant is updated, highlight the affected residue.
    # Uses aa_pos_structure, NOT raw AA_POS: for PCDH19, the PDB file's own
    # residue numbering is offset from the full-protein AA_POS by a
    # constant (confirmed empirically in R/0-data-prep.R, Q1b) -- using
    # AA_POS directly would highlight the wrong residue for that gene.
    # aa_pos_structure already falls back to AA_POS for genes with no such
    # offset (everything except PCDH19).
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
        # Replaces the old class-proportion barplot (TODO.md Phase 3),
        # ported from Researcher_asks/code_for_plots/
        # website_EpiPred_distribution_by_class.R -- uses mutations()
        # directly (not a separate per-gene file) so the distribution
        # shown always matches the classification used elsewhere on this
        # tab, for all 5 genes. ambiguous_range_list_ supplies the same
        # per-gene cutoff lines already used for the colorbar, instead of
        # hardcoding 0.1/0.9 (which isn't correct for STXBP1 -- see
        # TODO.md Q2).
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

TableDisplayServer <- function(id, mutations, selected, dt_selected_index) {
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
    
    # update selected id (id = row_number(), assigned fresh in
    # clean_mutations() every time mutations() reloads -- see
    # R/3-utils.R -- so this works the same way for all 5 genes)
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
    
    # Heatmap of predicted variant effect (TODO.md Phase 4), placed above
    # the scatterplot section per for_researchers.docx. Shares the same
    # gene selector as the scatterplot below (mounted once here; the
    # scatterplot's own copy stays in sync via the shared `gene`
    # reactiveVal, same as every other pair of gene selectors in the app).
    card(
      card_header("Heatmap of Predicted Variant Effect"),
      # Interactive (post-delivery enhancement) -- plotOutput() swapped for
      # plotly::plotlyOutput() to match class_heatmap_plot() now returning
      # a plotly object (see R/8-heatmapPlot.R) instead of a static ggplot.
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
    
    # display warning output ouside the plot area
    div(
      verbatimTextOutput(NS(id,"allvar_plot_text"), FALSE),
      class = "warning-class"
    )
  )
}

AllVarServer <- function(id, mutations, gene, selected, dt_selected_index) {
  moduleServer(id, function(input,output,session) {
    
    # Gene selection (shared component -- TODO.md Phase 1). See the same
    # comment in SingleVarServer above for why `selected` is unused here now.
    GeneSelectorServer("gene_selector", gene)
    
    # Heatmap of predicted variant effect (TODO.md Phase 4) -- uses the
    # full mutations() data, unfiltered by the "Reported" checkboxes
    # below, since it's meant to show the whole predicted-effect
    # landscape (mostly simulated variants) rather than only the
    # clinically-observed ones the scatterplot filters to.
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
