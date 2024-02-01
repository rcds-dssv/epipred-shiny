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
             div(style="padding-top:15px;",verbatimTextOutput(NS(id,"text"))),
             plotOutput(NS(id,"epipred_bar"))
      )
    ),
    column(4,
           selectInput(NS(id,"report_gg"),
                       "Reported:",
                       c("All",unique(as.character(mutations$Reported))),
           )
    ),
    br(), br(), 
    plotlyOutput(NS(id,'pointplot'))
  )
}

SingleVarServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    # NGLViewer Output
    output$structure <- renderNGLVieweR({
      NGLVieweR("data/pdb/stxbp1.pdb") %>%
        addRepresentation(
          "cartoon",
          param = list(name = "cartoon", color = "residueindex"
        )
      ) %>%
      addRepresentation("ball+stick",
                        param = list(
                          name = "cartoon",
                          sele = "1-20",
                          colorScheme = "element"
                        )
      ) %>%
      stageParameters(backgroundColor = "white") %>%
      setQuality("high") %>%
      setFocus(0) %>%
      setSpin(TRUE)
    })
    
    # variant prediction text output
    output$text <- renderText({
      input$update
      data <- data.frame(mutations)
      isolate(paste("Pathogenic Score is:", toString(unique(data$EpiPred_Raw_Score[data$AA_Change==input$val])), "\nPathogenic Class:", toString(unique(data$EpiPred_Class[data$AA_Change==input$val]))))
    })
    
    # epipred output
    output$epipred_bar <- renderPlot({
      epipred_colorbar()
    })
    
    # plot output (temporary)
    output$pointplot <- renderPlotly({
      data <- data.frame(mutations)
      if (input$report_gg != "All") {
        data <- data[data$Reported == input$report_gg,]
      }
      gg <- ggplot(data, aes(x = AA_POS, y = 0,  group = Reported, color = EpiPred_Class)) + 
        geom_point(aes(size = EpiPred_Raw_Score, ids = AA_Change), alpha = 0.6, shape = 21) + 
        coord_cartesian(ylim = c(-.5, .5)) +
        scale_x_continuous(breaks=seq(0, 650, by = 50)) +
        scale_size_area(max_size = 10) +
        theme(
          panel.grid.major = element_line(color = "lightgrey", size = 0.2),
          panel.grid.major.y = element_blank(),
          panel.background = element_rect(fill = "white"),
          axis.text.y = element_blank(),
          axis.text.x = element_text(size = 6),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.ticks.y = element_blank(),
          plot.title = element_blank(),
          legend.title = element_blank(),
          panel.border = element_rect(colour = "darkgrey", fill = NA, size = 1)
        )
      
      
      ggplotly(gg, tooltip = c( "AA_Change", "EpiPred_Class", "EpiPred_Raw_Score"))
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
