
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
      stageParameters(backgroundColor = "black") %>%
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
    titlePanel("STXBP1 Variant Table"),
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
    plotOutput(NS(id,"plot")),
    sliderInput(NS(id,"n"), "Number of Variants", 1, length(mutations$EpiPred_Raw_Score), 50, step = 5),
    plotOutput(NS(id,'dotplot1')),
    fluidRow(
      column(4,
             selectInput(NS(id,'xcol'), 'X Variable', vars),
      ),
      column(4,
             selectInput(NS(id,'ycol'), 'Y Variable', vars, selected = vars[[2]])
      )
    )
  )
}

AllVarServer <- function(id) {
  moduleServer(id, function(input,output,session) {
    output$plot <- renderPlot({
      data <- data.frame(mutations)
      hist(data$EpiPred_Raw_Score[seq_len(input$n)], breaks = 30, main="Distribution of Variants by EpiPred Score", xlab = "Pathogenicity Score")
    })
    
    output$dotplot1 <- renderPlot({
      palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
                "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))
      
      par(mar = c(5.1, 4.1, 0, 1))
      data <- data.frame(mutations)
      plot(data[seq_len(input$n),c(input$xcol, input$ycol)],
           pch = 20, cex = 3)
    })
    
    output$dashedplot <- renderPlot({
      data <- data.frame(mutations)
      data <- data[c("AA_POS", "EpiPred_Raw_Score")]
      agg_min = aggregate(data,
                          by = list(data$AA_POS),
                          FUN = min)
      agg_median = aggregate(data,
                             by = list(data$AA_POS),
                             FUN = median)
      agg_max = aggregate(data,
                          by = list(data$AA_POS),
                          FUN = max)
      #print(agg)
      ggplot() +
        geom_point(data = agg_min, aes(x = AA_POS, y = EpiPred_Raw_Score), color = "blue") + 
        geom_line(data = agg_min, aes(x = AA_POS, y = EpiPred_Raw_Score), linetype = "dashed", color = "blue") +
        geom_point(data = agg_median, aes(x = AA_POS, y = EpiPred_Raw_Score), color = "black") + 
        geom_line(data = agg_median, aes(x = AA_POS, y = EpiPred_Raw_Score),linetype = "dashed", color = "black") +
        geom_point(data = agg_max, aes(x = AA_POS, y = EpiPred_Raw_Score), color = "red") + 
        geom_line(data = agg_max, aes(x = AA_POS, y = EpiPred_Raw_Score),linetype = "dashed", color = "red") 
    })
    
    output$dotplot1 <- renderPlot({
      palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
                "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))
      
      par(mar = c(5.1, 4.1, 0, 1))
      data <- data.frame(mutations)
      plot(data[seq_len(input$n),c(input$xcol, input$ycol)],
           pch = 20, cex = 3)
    })
  })
}
