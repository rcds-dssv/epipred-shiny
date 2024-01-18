library(shiny)
library(shinyWidgets)
library(NGLVieweR)
library(ggplot2)
library(dplyr)
library(plotly)

mutations <- read.csv(file="STXBP1_DTv2.csv")
vars <- c('EpiPred_Raw_Score','CADD_PHRED')

fluidPage(
  navbarPage("EpiPred",
             tabPanel("Home", 
                      fluidRow(
                        column(10, align="center", offset = 1,
                               shiny::HTML("<br><br> <h1>EpiMVP</h1><br>"),
                               shiny::HTML("<h5><p><div> 
                               
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
                        
                      ),
             ),
            tabPanel("Score Missense Variant", 
                      NGLVieweROutput("structure"),
                      fluidRow(
                        column(8, align="center", offset = 2,
                               textInput("val", h3("Input Amino Acid Sequence"), value = "p.A2P"),
                               actionButton("update" ,"Submit", class = "btn btn-primary"),
                               div(style="padding-top:15px;",verbatimTextOutput("text"))
                        )
                      ),
                      column(4,
                            selectInput("report_gg",
                                        "Reported:",
                                        c("All",unique(as.character(mutations$Reported))),
  
                            )
                       ),
                       br(), br(), 
                       plotlyOutput('pointplot')
                    
            ),
            tabPanel("STXPB1 Table",
                      titlePanel("STXBP1 Variant Table"),
                      fluidRow(
                        column(4,
                               selectInput("class",
                                           "EpiPred Class:",
                                           c("All",unique(as.character(mutations$EpiPred_Class)))
                                          )
                              ),
                        column(4,
                               selectInput("report",
                                           "Reported:",
                                           c("All",unique(as.character(mutations$Reported)))
                               )
                        )
                        
                      ),
                      DT::dataTableOutput("table"),
                      downloadButton("downloadData", "Download")
            ),
            tabPanel("Plots",
                     plotOutput("plot"),
                     sliderInput("n", "Number of Variants", 1, length(mutations$EpiPred_Raw_Score), 50, step = 5),
                     plotOutput('dotplot1'),
                     fluidRow(
                       column(4,
                              selectInput('xcol', 'X Variable', vars),
                       ),
                       column(4,
                              selectInput('ycol', 'Y Variable', vars, selected = vars[[2]])
                       )
                     )
            )
  )
  
  
)