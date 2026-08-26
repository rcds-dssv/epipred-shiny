# ==========================================================================
# 2-3-gene-selector.R
#
# Shared gene-selector UI component (TODO.md Phase 1). Renders the purple
# pill-button row shown in the V2 landing page / Check_my_variant /
# for_researchers mockups, in place of the old per-tab gene selectInput
# dropdowns.
#
# Usage (inside another module's UI/server functions -- see SingleVarUI /
# SingleVarServer in R/2-2-modules.R for a worked example):
#
#   UI:      GeneSelectorUI(NS(id, "gene_selector"))
#   Server:  GeneSelectorServer("gene_selector", gene)   # call from inside
#                                                         # the parent's
#                                                         # moduleServer()
#
# `gene` must be the same top-level reactiveVal("STXBP1") app.R creates and
# passes into every tab's *Server() function. This component reads and
# writes it directly, so every tab using it stays in sync automatically --
# the same guarantee the old selectInput + observeEvent/updateSelectInput
# pairing provided, just without needing that boilerplate repeated in each
# tab (compare to the code this replaces in SingleVarServer/AllVarServer).
#
# Note: genes_avail_ (R/1-setup.R) currently has only one entry (STXBP1).
# This component already loops over however many genes are in `genes`, so
# it doesn't need any changes when genes_avail_ is expanded later --
# expanding that list is a separate Phase 0 task (see TODO.md).
#
# Tested in isolation with shiny::testServer() before being wired into the
# app: button clicks correctly update the shared reactiveVal, and the
# "active" highlight correctly follows whichever gene is current, including
# when it was changed via a different instance of this same component.
# ==========================================================================

# UI: a placeholder the server side fills in reactively (so the "active"
# button can update without a full page reload whenever `gene()` changes,
# including changes made from a different tab's copy of this component).
GeneSelectorUI <- function(id) {
  ns <- NS(id)
  tagList(
    tags$style(HTML(
      "
      .gene-selector-row {
        display: flex;
        flex-wrap: wrap;
        gap: 10px;
        justify-content: center;
        margin: 8px 0 18px 0;
      }
      .gene-selector-btn {
        background-color: #8064A2 !important;
        border: 2px solid #1A1A2E !important;
        border-radius: 10px !important;
        color: #FFFFFF !important;
        font-weight: 600;
        padding: 8px 22px !important;
      }
      .gene-selector-btn:hover {
        background-color: #6F5590 !important;
      }
      .gene-selector-btn-active {
        background-color: #4E2A84 !important;
        box-shadow: 0 0 0 3px rgba(78, 42, 132, 0.35);
      }
      "
    )),
    uiOutput(ns("buttons"))
  )
}

# Server: wires each button's click to the shared `gene` reactiveVal, and
# re-renders the row so the button matching the current gene() is visually
# marked active -- regardless of whether gene() changed because THIS
# button row was clicked, or because a different tab's copy of this
# component (or, during the Phase 1-4 transition, an old selectInput) was.
GeneSelectorServer <- function(id, gene, genes = genes_avail_) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # one click observer per gene button, set up once at module init
    lapply(genes, function(g) {
      observeEvent(input[[paste0("gene_", g)]], {
        gene(g)
      }, ignoreInit = TRUE)
    })

    output$buttons <- renderUI({
      current <- gene()
      div(
        class = "gene-selector-row",
        lapply(genes, function(g) {
          actionButton(
            inputId = ns(paste0("gene_", g)),
            label = g,
            class = paste(
              "gene-selector-btn",
              if (identical(g, current)) "gene-selector-btn-active" else ""
            )
          )
        })
      )
    })
  })
}
