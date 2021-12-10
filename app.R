##########################
# PURPOSE: 
##########################

library(shiny)
library(shinydashboard)
library(dashboardthemes)
library(ggplot2)
library(dplyr)
library(stringr)
library(plotly)

ui <- dashboardPage(dashboardHeader(title="RNA Differential Expression",
                                    titleWidth=330),
                    dashboardSidebar(width=330,
                                     sidebarMenu(menuItem("Create Plot", tabName="create",
                                                          icon=icon("plus")),
                                                 menuItem("Examples", tabName="examples",
                                                          icon=icon("chart-bar"),
                                                          startExpanded=T,
                                                          menuItem("GSE117280_EBs", tabName="study1"),
                                                          menuItem("GSE117280_ESC", tabName="study2"),
                                                          menuItem("GSE145653", tabName="study3"),
                                                          menuItem("NLRC3", tabName="study4")),
                                                 menuItem("Info", tabName="info",
                                                          icon=icon("info")))),
                    dashboardBody(shinyDashboardThemes(theme="grey_light"),
                                  tabItems(tabItem("create",
                                                   tags$head(tags$style("#mystudyp1{height:80vh !important;}")),
                                                   tags$head(tags$style("#mystudyp2{height:80vh !important;}")),
                                                   tags$head(tags$style("#mystudyp3{height:80vh !important;}")),
                                                   fluidRow(column(8, tabBox(id="mystudy", width="100%", height="100%",
                                                                             tabPanel(title="Upload Information",
                                                                                      fileInput("myCounttable", tags$span("Upload counttable (.csv)",
                                                                                                                          h6("Make sure first column is variable for location."))),
                                                                                      uiOutput("myMeta"),
                                                                                      uiOutput("myExon"),
                                                                                      uiOutput("myTitle"),
                                                                                      uiOutput("myKO"),
                                                                                      uiOutput("myWT"),
                                                                                      uiOutput("myGeneLoc"),
                                                                                      uiOutput("mysgRNA"),
                                                                                      uiOutput("plotClick")),
                                                                             tabPanel(title="Counts", plotlyOutput("mystudyp1")),
                                                                             tabPanel(title="Difference", plotlyOutput("mystudyp2")),
                                                                             tabPanel(title="Standardized Difference", plotlyOutput("mystudyp3")))),
                                                            column(4,
                                                                   fluidRow(box(uiOutput("myBin"),
                                                                                uiOutput("myNorm"),
                                                                                width=12))))),
                                           tabItem("study1",
                                                   tags$head(tags$style("#study1p1{height:80vh !important;}")),
                                                   tags$head(tags$style("#study1p2{height:80vh !important;}")),
                                                   tags$head(tags$style("#study1p3{height:80vh !important;}")),
                                                   fluidRow(column(8, tabBox(id="study1p", width="100%", height="100%",
                                                                             tabPanel(title="Counts", plotlyOutput("study1p1")),
                                                                             tabPanel(title="Difference", plotlyOutput("study1p2")),
                                                                             tabPanel(title="Standardized Difference", plotlyOutput("study1p3")))),
                                                            column(4,
                                                                   fluidRow(box(title="About the Series", width=12,
                                                                                "GSE117280_EBs", br(), br(),
                                                                                strong("Series Title"), br(),
                                                                                "RNA-seq analysis of Pten's function in regulating embryonic stem cell transition from naive to primed pluripotent state.", br(),
                                                                                strong("Series Summary"), br(),
                                                                                "The goals of this study are to explore the function of Pten in regulating the pluripotency transition of mouse embryonic stem cells through Pten knockout and mutation.", br(),
                                                                                strong("Organism"), br(),
                                                                                "Mus musculus", br(),
                                                                                strong("Tissue"), br(),
                                                                                "Embryoid bodies", br(),
                                                                                strong("Gene"), br(),
                                                                                "PTEN", br(),
                                                                                strong("Number of Samples"), br(),
                                                                                "WT: 3, KO: 3", br(),
                                                                                strong("Date"), br(),
                                                                                "Jul 18 2018")),
                                                                   fluidRow(box(sliderInput("myBin1", "Bin Size", 100, 4000, 2000, step=100, ticks=F),
                                                                                checkboxInput("myNorm1", "Use normalized counts", T),
                                                                                width=12))))),
                                           tabItem("study2",
                                                   tags$head(tags$style("#study2p1{height:80vh !important;}")),
                                                   tags$head(tags$style("#study2p2{height:80vh !important;}")),
                                                   tags$head(tags$style("#study2p3{height:80vh !important;}")),
                                                   fluidRow(column(8, tabBox(id="study2p", width="100%", height="100%",
                                                                             tabPanel(title="Counts", plotlyOutput("study2p1")),
                                                                             tabPanel(title="Difference", plotlyOutput("study2p2")),
                                                                             tabPanel(title="Standardized Difference", plotlyOutput("study2p3")))),
                                                            column(4,
                                                                   fluidRow(box(title="About the Series", width=12,
                                                                                "GSE117280_ESC", br(), br(),
                                                                                strong("Series Title"), br(),
                                                                                "RNA-seq analysis of Pten's function in regulating embryonic stem cell transition from naive to primed pluripotent state.", br(),
                                                                                strong("Series Summary"), br(),
                                                                                "The goals of this study are to explore the function of Pten in regulating the pluripotency transition of mouse embryonic stem cells through Pten knockout and mutation.", br(),
                                                                                strong("Organism"), br(),
                                                                                "Mus musculus", br(),
                                                                                strong("Tissue"), br(),
                                                                                "Embryonic stem cells", br(),
                                                                                strong("Gene"), br(),
                                                                                "PTEN", br(),
                                                                                strong("Number of Samples"), br(),
                                                                                "WT: 5, KO: 3", br(),
                                                                                strong("Date"), br(),
                                                                                "Jul 18 2018")),
                                                                   fluidRow(box(sliderInput("myBin2", "Bin Size", 100, 4000, 2000, step=100, ticks=F),
                                                                                checkboxInput("myNorm2", "Use normalized counts", T),
                                                                                width=12))))),
                                           tabItem("study3",
                                                   tags$head(tags$style("#study3p1{height:80vh !important;}")),
                                                   tags$head(tags$style("#study3p2{height:80vh !important;}")),
                                                   tags$head(tags$style("#study3p3{height:80vh !important;}")),
                                                   fluidRow(column(8, tabBox(id="study3p", width="100%", height="100%",
                                                                             tabPanel(title="Counts", plotlyOutput("study3p1")),
                                                                             tabPanel(title="Difference", plotlyOutput("study3p2")),
                                                                             tabPanel(title="Standardized Difference", plotlyOutput("study3p3")))),
                                                            column(4,
                                                                   fluidRow(box(title="About the Series", width=12,
                                                                                "GSE145653", br(), br(),
                                                                                strong("Series Title"), br(),
                                                                                "Cooperative genetic networks drive a mammalian cell state transition.", br(),
                                                                                strong("Series Summary"), br(),
                                                                                "In the mammalian embryo epiblast cells must exit their naive state and differentiate to acquire formative pluripotency. This cell-state-transition is recapitulated in mouse embryonic stem cells (ESCs), which undergo pluripotency-progression in defined conditions in vitro. Here we combined genetic screens in haploid ESCs with CRISPR/Cas9 gene-disruption, large-scale transcriptomics and computational systems-biology to delineate the regulatory circuits governing naive-state-exit. Transcriptome profiles for 73 knockouts mostly align on the in vivo trajectory from naive to formative epiblast. We identified 496 naive-associated genes tightly connected to the epiblast state and largely conserved primate embryos. Systematic analysis of mutant transcriptomes highlighted discrete regulatory clusters, which are under control of one or more of five signaling pathways. Thus, a pivotal mammalian cell state transition is driven by multiple genes whose actions are funneled through a handful of complementary circuits in vitro and in vivo.", br(),
                                                                                strong("Organism"), br(),
                                                                                "Mus musculus", br(),
                                                                                strong("Tissue"), br(),
                                                                                "Embryonic stem cells", br(),
                                                                                strong("Gene"), br(),
                                                                                "PTEN", br(),
                                                                                strong("Number of Samples"), br(),
                                                                                "WT: 14, KO: 2", br(),
                                                                                strong("Date"), br(),
                                                                                "Feb 20 2020")),
                                                                   fluidRow(box(sliderInput("myBin3", "Bin Size", 100, 4000, 2000, step=100, ticks=F),
                                                                                checkboxInput("myNorm3", "Use normalized counts", T),
                                                                                width=12))))),
                                           tabItem("study4",
                                                   tags$head(tags$style("#study4p1{height:80vh !important;}")),
                                                   tags$head(tags$style("#study4p2{height:80vh !important;}")),
                                                   tags$head(tags$style("#study4p3{height:80vh !important;}")),
                                                   fluidRow(column(8, tabBox(id="study4p", width="100%", height="100%",
                                                                             tabPanel(title="Counts", plotlyOutput("study4p1")),
                                                                             tabPanel(title="Difference", plotlyOutput("study4p2")),
                                                                             tabPanel(title="Standardized Difference", plotlyOutput("study4p3")))),
                                                            column(4,
                                                                   fluidRow(box(title="About the Series", width=12,
                                                                                "NLRC3", br(), br(),
                                                                                strong("Series Title"), br(),
                                                                                "The Innate Immune Sensor NLRC3 Acts as a Rheostat that Fine-Tunes T Cell Responses in Infection and Autoimmunity.", br(),
                                                                                strong("Series Summary"), br(),
                                                                                "", br(),
                                                                                strong("Organism"), br(),
                                                                                "Mus musculus", br(),
                                                                                strong("Tissue"), br(),
                                                                                "Spleen cells", br(),
                                                                                strong("Gene"), br(),
                                                                                "NLRC3", br(),
                                                                                strong("Number of Samples"), br(),
                                                                                "WT: 8, KO: 8", br(),
                                                                                strong("Date"), br(),
                                                                                "2018")),
                                                                   fluidRow(box(sliderInput("myBin4", "Bin Size", 100, 4000, 2000, step=100, ticks=F),
                                                                                checkboxInput("myNorm4", "Use normalized counts", T),
                                                                                width=12))))),
                                           tabItem("info",
                                                   box(width=6,
                                                       strong("Data Source"), br(),
                                                       "All genomic data are obtained from the NCBI GEO database in FASTQ format.",
                                                       "STAR and SAMTOOLS packages are used for sequence alignment processing and to extract the sequence counts.", br(), br(),
                                                       strong("Calculations"), br(),
                                                       "The length of the particular gene of interest is broken up into windows of a specifiable size.", br(),
                                                       "Within each window:", br(),
                                                       "1. In each sample, the number of RNA sequences is summed and normalized by dividing by the total number of reads for that sample, then multiplied by 10E6 to obtain a more reasonably sized number for handling.", br(),
                                                       "2. Across replicated samples of the same cell type (WT or KO), mean and standard deviation of the number of sequence counts is found.", br(),
                                                       "3. Find the standardized difference of counts by taking the difference of mean KO counts and mean WT counts, then dividing over the sum of KO and WT count standard deviations.")))
                      ))

server <- function(input, output, session) {
  
  options(shiny.maxRequestSize=10*1024^2) # max 7 Mb file upload
  
  ## COUNTTABLE FILE UPLOAD ##
  observeEvent(input$myCounttable, {
    counttable = read.csv(input$myCounttable$datapath, header=T, check.names=F)

    output$myMeta <- renderUI({
      fileInput("myMeta", tags$span("Upload total counts (.txt)",
                                      h6("'name' column that matches sample name and 'total' column for # total reads.")))
    })
    
    ## META/TOTAL COUNTS UPLOAD ##
    observeEvent(input$myMeta, {
      output$myExon <- renderUI({
        fileInput("myExon", tags$span("Upload exon locations (.txt)",
                                      h6("Include 'exon', 'id', 'start', and 'end' columns.")))
      })
      
      ## EXON INFO UPLOAD ##
      observeEvent(input$myExon, {
        output$myTitle <- renderUI({
          textInput("myTitle", "Enter a title for this comparison", "My Study")
        })
        
        output$myKO <- renderUI({
          selectInput("myKO",
                      tags$span("Select KO samples", h6("Use mouse to drag or hold down 'Ctrl' to select multiple")),
                      names(counttable), multiple=T, selectize=F, size=10)
        })
        
        output$myWT <- renderUI({
          selectInput("myWT", "Select WT samples", names(counttable), multiple=T, selectize=F, size=10)
        })
        
        output$myGeneLoc <- renderUI({
          textInput("myGeneLoc", tags$span("Enter range of location",
                                           h6("e.g. for gene NLRC3 enter: 3943807-3979017")))
        })
        
        output$mysgRNA <- renderUI({
          numericInput("mysgRNA", "Enter KO/sgRNA location", 1)
        })
        
        output$myBin <- renderUI({
          sliderInput("myBin", "Bin Size", 100, 4000, 2000, step=100, ticks=F)
        })
        
        output$myNorm <- renderUI({
          checkboxInput("myNorm", "Use normalized counts", T)
        })
        
        output$plotClick <- renderUI({
          actionButton("plotClick", "Plot", width="20%", style="padding:30px")
        })
      })
      
    })
  })
  
  # rv <- reactiveValues(X=NULL, p_mystudy=NULL)
  ## START PLOT ##
  observe({ # Reacts to plot button click & myBin & myNorm
    req(input$plotClick)
    if (input$plotClick>0) {
      myCounttable = read.csv(input$myCounttable$datapath, header=T, check.names=F)
      myMeta = read.table(input$myMeta$datapath, header=T)
      myExon = read.table(input$myExon$datapath, header=T)
      
      X <- create_X(
        myMeta = myMeta,
        myExon = myExon,
        myBin = input$myBin, # Reacts to myBin
        myNorm = input$myNorm, # Reacts to myNorm
        myGeneLoc = strsplit(isolate(input$myGeneLoc), "-")[[1]],
        
        myTitle = isolate(input$myTitle),
        myKO = isolate(input$myKO),
        myWT = isolate(input$myWT),
        mysgRNA = isolate(input$mysgRNA),
        
        counttable = myCounttable
      )
      
      p_mystudy <- create_p(
          myMeta = myMeta,
          myExon = myExon,
          myBin = isolate(input$myBin),
          myNorm = isolate(input$myNorm),
          myGeneLoc = strsplit(isolate(input$myGeneLoc), "-")[[1]],
          
          myTitle = isolate(input$myTitle),
          myKO = isolate(input$myKO),
          myWT = isolate(input$myWT),
          mysgRNA = isolate(input$mysgRNA),
          
          counttable = myCounttable,
          X = X
        )
      
      output$mystudyp1 <- renderPlotly({
        create_p1(
          myMeta = myMeta,
          myExon = myExon,
          myBin = input$myBin,
          myNorm = input$myNorm,
          myGeneLoc = strsplit(isolate(input$myGeneLoc), "-")[[1]],
          
          myTitle = isolate(input$myTitle),
          myKO = isolate(input$myKO),
          myWT = isolate(input$myWT),
          mysgRNA = isolate(input$mysgRNA),
          
          counttable = myCounttable,
          X = X,
          p = p_mystudy
        )
      })
      
      output$mystudyp2 <- renderPlotly({
        create_p2(
          myMeta = myMeta,
          myExon = myExon,
          myBin = input$myBin,
          myNorm = input$myNorm,
          myGeneLoc = strsplit(isolate(input$myGeneLoc), "-")[[1]],
          
          myTitle = isolate(input$myTitle),
          myKO = isolate(input$myKO),
          myWT = isolate(input$myWT),
          mysgRNA = isolate(input$mysgRNA),
          
          counttable = myCounttable,
          X = X,
          p = p_mystudy
        )
      })
      
      output$mystudyp3 <- renderPlotly({
        create_p3(
          myMeta = myMeta,
          myExon = myExon,
          myBin = input$myBin,
          myNorm = input$myNorm,
          myGeneLoc = strsplit(isolate(input$myGeneLoc), "-")[[1]],
          
          myTitle = isolate(input$myTitle),
          myKO = isolate(input$myKO),
          myWT = isolate(input$myWT),
          mysgRNA = isolate(input$mysgRNA),
          
          counttable = myCounttable,
          X = X,
          p = p_mystudy
        )
      })
      
      updateTabsetPanel(session, "mystudy", selected="Counts")
    }
  })
  
  ## EXAMPLE PLOTS ##
  
  counttable <- reactive({load("counttable2.RData"); counttable})
  counttable_toru <- reactive(read.csv("counttable_toru.csv", header=T, check.names=F))
  myMeta <- reactive(read.table("srr_name_totals.txt", fill=T, header=T))
  myMeta_toru <- reactive(read.table("counts_toru.txt", fill=T, header=T))
  myExon <- reactive(read.table("exons.txt", header=T))
  myExon_toru <- reactive(read.table("exons_toru.txt", header=T))
  
  ##############################################################################
  ## EXAMPLE STUDY 1 ##
  
  X1 <- reactive({
    create_X(
      myMeta = isolate(myMeta()),
      myExon = isolate(myExon()),
      myBin = input$myBin1, # Reacts to myBin1
      myNorm = input$myNorm1, # Reacts to myNorm1
      myGeneLoc = c(32757497, 32826160),
      
      myTitle = "GSE117280_EBs",
      myKO = c("Pten_KO_EBs-1", "Pten_KO_EBs-2", "Pten_KO_EBs-3"),
      myWT = c("WT_EBs-1", "WT_EBs-2", "WT_EBs-3"),
      mysgRNA = 32758481,
      
      counttable = isolate(counttable())
    )
  })
  
  p_study1 <- reactive({
    create_p(
      myMeta = isolate(myMeta()),
      myExon = isolate(myExon()),
      myBin = isolate(input$myBin1),
      myNorm = isolate(input$myNorm1),
      myGeneLoc = c(32757497, 32826160),
      
      myTitle = "GSE117280_EBs",
      myKO = c("Pten_KO_EBs-1", "Pten_KO_EBs-2", "Pten_KO_EBs-3"),
      myWT = c("WT_EBs-1", "WT_EBs-2", "WT_EBs-3"),
      mysgRNA = 32758481,
      
      counttable = isolate(counttable()),
      X = X1() # Reacts to X1
    )
  })
  
  output$study1p1 <- renderPlotly({
    create_p1(
      myMeta = isolate(myMeta()),
      myExon = isolate(myExon()),
      myBin = isolate(input$myBin1),
      myNorm = isolate(input$myNorm1),
      myGeneLoc = c(32757497, 32826160),
      
      myTitle = "GSE117280_EBs",
      myKO = c("Pten_KO_EBs-1", "Pten_KO_EBs-2", "Pten_KO_EBs-3"),
      myWT = c("WT_EBs-1", "WT_EBs-2", "WT_EBs-3"),
      mysgRNA = 32758481,
      
      counttable = isolate(counttable()),
      X = isolate(X1()),
      p = p_study1() # Reacts to p_study1
    )
  })
  
  output$study1p2 <- renderPlotly({
    create_p2(
      myMeta = isolate(myMeta()),
      myExon = isolate(myExon()),
      myBin = isolate(input$myBin1),
      myNorm = isolate(input$myNorm1),
      myGeneLoc = c(32757497, 32826160),
      
      myTitle = "GSE117280_EBs",
      myKO = c("Pten_KO_EBs-1", "Pten_KO_EBs-2", "Pten_KO_EBs-3"),
      myWT = c("WT_EBs-1", "WT_EBs-2", "WT_EBs-3"),
      mysgRNA = 32758481,
      
      counttable = isolate(counttable()),
      X = isolate(X1()),
      p = p_study1() # Reacts to p_study1
    )
  })
  
  output$study1p3 <- renderPlotly({
    create_p3(
      myMeta = isolate(myMeta()),
      myExon = isolate(myExon()),
      myBin = isolate(input$myBin1),
      myNorm = isolate(input$myNorm1),
      myGeneLoc = c(32757497, 32826160),
      
      myTitle = "GSE117280_EBs",
      myKO = c("Pten_KO_EBs-1", "Pten_KO_EBs-2", "Pten_KO_EBs-3"),
      myWT = c("WT_EBs-1", "WT_EBs-2", "WT_EBs-3"),
      mysgRNA = 32758481,
      
      counttable = isolate(counttable()),
      X = isolate(X1()),
      p = p_study1() # Reacts to p_study1
    )
  })
  
  ##############################################################################
  ## EXAMPLE STUDY 2 ##
  
  X2 <- reactive({
    create_X(
      myMeta = isolate(myMeta()),
      myExon = isolate(myExon()),
      myBin = input$myBin2, # Reacts to myBin2
      myNorm = input$myNorm2, # Reacts to myNorm2
      myGeneLoc = c(32757497, 32826160),
      
      myTitle = "GSE117280_ESC",
      myKO = c("Pten_KO_ESC-1", "Pten_KO_ESC-2", "Pten_KO_ESC-3"),
      myWT = c("WT_ESC-1", "WT_ESC-2", "WT_ESC-3", "WT_ESC-4", "WT_ESC-5"),
      mysgRNA = 32758481,
      
      counttable = isolate(counttable())
    )
  })
  
  p_study2 <- reactive({
    create_p(
      myMeta = isolate(myMeta()),
      myExon = isolate(myExon()),
      myBin = isolate(input$myBin2),
      myNorm = isolate(input$myNorm2),
      myGeneLoc = c(32757497, 32826160),
      
      myTitle = "GSE117280_ESC",
      myKO = c("Pten_KO_ESC-1", "Pten_KO_ESC-2", "Pten_KO_ESC-3"),
      myWT = c("WT_ESC-1", "WT_ESC-2", "WT_ESC-3", "WT_ESC-4", "WT_ESC-5"),
      mysgRNA = 32758481,
      
      counttable = isolate(counttable()),
      X = X2() # Reacts to X2
    )
  })
  
  output$study2p1 <- renderPlotly({
    create_p1(
      myMeta = isolate(myMeta()),
      myExon = isolate(myExon()),
      myBin = isolate(input$myBin2),
      myNorm = isolate(input$myNorm2),
      myGeneLoc = c(32757497, 32826160),
      
      myTitle = "GSE117280_ESC",
      myKO = c("Pten_KO_ESC-1", "Pten_KO_ESC-2", "Pten_KO_ESC-3"),
      myWT = c("WT_ESC-1", "WT_ESC-2", "WT_ESC-3", "WT_ESC-4", "WT_ESC-5"),
      mysgRNA = 32758481,
      
      counttable = isolate(counttable()),
      X = isolate(X2()),
      p = p_study2() # Reacts to p_study2
    )
  })
  
  output$study2p2 <- renderPlotly({
    create_p2(
      myMeta = isolate(myMeta()),
      myExon = isolate(myExon()),
      myBin = isolate(input$myBin2),
      myNorm = isolate(input$myNorm2),
      myGeneLoc = c(32757497, 32826160),
      
      myTitle = "GSE117280_ESC",
      myKO = c("Pten_KO_ESC-1", "Pten_KO_ESC-2", "Pten_KO_ESC-3"),
      myWT = c("WT_ESC-1", "WT_ESC-2", "WT_ESC-3", "WT_ESC-4", "WT_ESC-5"),
      mysgRNA = 32758481,
      
      counttable = isolate(counttable()),
      X = isolate(X2()),
      p = p_study2() # Reacts to p_study1
    )
  })
  
  output$study2p3 <- renderPlotly({
    create_p3(
      myMeta = isolate(myMeta()),
      myExon = isolate(myExon()),
      myBin = isolate(input$myBin2),
      myNorm = isolate(input$myNorm2),
      myGeneLoc = c(32757497, 32826160),
      
      myTitle = "GSE117280_ESC",
      myKO = c("Pten_KO_ESC-1", "Pten_KO_ESC-2", "Pten_KO_ESC-3"),
      myWT = c("WT_ESC-1", "WT_ESC-2", "WT_ESC-3", "WT_ESC-4", "WT_ESC-5"),
      mysgRNA = 32758481,
      
      counttable = isolate(counttable()),
      X = isolate(X2()),
      p = p_study2() # Reacts to p_study1
    )
  })
  
  ##############################################################################
  ## EXAMPLE STUDY 3 ##
  
  X3 <- reactive({
    create_X(
      myMeta = isolate(myMeta()),
      myExon = isolate(myExon()),
      myBin = input$myBin3, # Reacts to myBin3
      myNorm = input$myNorm3, # Reacts to myNorm3
      myGeneLoc = c(32757497, 32826160),
      
      myTitle = "GSE145653",
      myKO = c("302_2i_Pten", "303_2i_Pten"),
      myWT = c("157_2i_RC9-1", "16_2i_RC9-1", "190_2i_RC9-2", "23_2i_RC9-2",
             "259_2i_RC9-1", "316_2i_RC9-2", "538_2i_RC9-1", "539_2i_RC9-2",
             "669_2i_RC-9_2a", "683_2i_RC-9_2b", "84_2i_RC9-1", "98_2i_RC9-2",
             "L54674_2i_RC9", "L54679_2i_RC9"),
      mysgRNA = 32776044,
      
      counttable = isolate(counttable())
    )
  })
  
  p_study3 <- reactive({
    create_p(
      myMeta = isolate(myMeta()),
      myExon = isolate(myExon()),
      myBin = isolate(input$myBin3),
      myNorm = isolate(input$myNorm3),
      myGeneLoc = c(32757497, 32826160),
      
      myTitle = "GSE145653",
      myKO = c("302_2i_Pten", "303_2i_Pten"),
      myWT = c("157_2i_RC9-1", "16_2i_RC9-1", "190_2i_RC9-2", "23_2i_RC9-2",
               "259_2i_RC9-1", "316_2i_RC9-2", "538_2i_RC9-1", "539_2i_RC9-2",
               "669_2i_RC-9_2a", "683_2i_RC-9_2b", "84_2i_RC9-1", "98_2i_RC9-2",
               "L54674_2i_RC9", "L54679_2i_RC9"),
      mysgRNA = 32776044,
      
      counttable = isolate(counttable()),
      X = X3() # Reacts to X3
    )
  })
  
  output$study3p1 <- renderPlotly({
    create_p1(
      myMeta = isolate(myMeta()),
      myExon = isolate(myExon()),
      myBin = isolate(input$myBin3),
      myNorm = isolate(input$myNorm3),
      myGeneLoc = c(32757497, 32826160),
      
      myTitle = "GSE145653",
      myKO = c("302_2i_Pten", "303_2i_Pten"),
      myWT = c("157_2i_RC9-1", "16_2i_RC9-1", "190_2i_RC9-2", "23_2i_RC9-2",
               "259_2i_RC9-1", "316_2i_RC9-2", "538_2i_RC9-1", "539_2i_RC9-2",
               "669_2i_RC-9_2a", "683_2i_RC-9_2b", "84_2i_RC9-1", "98_2i_RC9-2",
               "L54674_2i_RC9", "L54679_2i_RC9"),
      mysgRNA = 32776044,
      
      counttable = isolate(counttable()),
      X = isolate(X3()),
      p = p_study3() # Reacts to p_study3
    )
  })
  
  output$study3p2 <- renderPlotly({
    create_p2(
      myMeta = isolate(myMeta()),
      myExon = isolate(myExon()),
      myBin = isolate(input$myBin3),
      myNorm = isolate(input$myNorm3),
      myGeneLoc = c(32757497, 32826160),
      
      myTitle = "GSE145653",
      myKO = c("302_2i_Pten", "303_2i_Pten"),
      myWT = c("157_2i_RC9-1", "16_2i_RC9-1", "190_2i_RC9-2", "23_2i_RC9-2",
               "259_2i_RC9-1", "316_2i_RC9-2", "538_2i_RC9-1", "539_2i_RC9-2",
               "669_2i_RC-9_2a", "683_2i_RC-9_2b", "84_2i_RC9-1", "98_2i_RC9-2",
               "L54674_2i_RC9", "L54679_2i_RC9"),
      mysgRNA = 32776044,
      
      counttable = isolate(counttable()),
      X = isolate(X3()),
      p = p_study3() # Reacts to p_study3
    )
  })
  
  output$study3p3 <- renderPlotly({
    create_p3(
      myMeta = isolate(myMeta()),
      myExon = isolate(myExon()),
      myBin = isolate(input$myBin3),
      myNorm = isolate(input$myNorm3),
      myGeneLoc = c(32757497, 32826160),
      
      myTitle = "GSE145653",
      myKO = c("302_2i_Pten", "303_2i_Pten"),
      myWT = c("157_2i_RC9-1", "16_2i_RC9-1", "190_2i_RC9-2", "23_2i_RC9-2",
               "259_2i_RC9-1", "316_2i_RC9-2", "538_2i_RC9-1", "539_2i_RC9-2",
               "669_2i_RC-9_2a", "683_2i_RC-9_2b", "84_2i_RC9-1", "98_2i_RC9-2",
               "L54674_2i_RC9", "L54679_2i_RC9"),
      mysgRNA = 32776044,
      
      counttable = isolate(counttable()),
      X = isolate(X3()),
      p = p_study3() # Reacts to p_study3
    )
  })
  
  ##############################################################################
  ## EXAMPLE STUDY 4 ##
  
  X4 <- reactive({
    create_X(
      myMeta = isolate(myMeta_toru()),
      myExon = isolate(myExon_toru()),
      myBin = input$myBin4, # Reacts to myBin4
      myNorm = input$myNorm4, # Reacts to myNorm4
      myGeneLoc = c(3943807, 3979017),
      
      myTitle="Toru",
      myKO = as.character(1:8),
      myWT = as.character(9:16),
      mysgRNA = 3943807,
      
      counttable = isolate(counttable_toru())
    )
  })
  
  p_study4 <- reactive({
    create_p(
      myMeta = isolate(myMeta_toru()),
      myExon = isolate(myExon_toru()),
      myBin = isolate(input$myBin4),
      myNorm = isolate(input$myNorm4),
      myGeneLoc = c(3943807, 3979017),
      
      myTitle="Toru",
      myKO = as.character(1:8),
      myWT = as.character(9:16),
      mysgRNA = 3943807,
      
      counttable = isolate(counttable_toru()),
      X = X4() # Reacts to X4
    )
  })
  
  output$study4p1 <- renderPlotly({
    create_p1(
      myMeta = isolate(myMeta_toru()),
      myExon = isolate(myExon_toru()),
      myBin = isolate(input$myBin4),
      myNorm = isolate(input$myNorm4),
      myGeneLoc = c(3943807, 3979017),
      
      myTitle="Toru",
      myKO = as.character(1:8),
      myWT = as.character(9:16),
      mysgRNA = 3943807,
      
      counttable = isolate(counttable_toru()),
      X = isolate(X4()),
      p = p_study4() # Reacts to p_study4
    )
  })
  
  output$study4p2 <- renderPlotly({
    create_p2(
      myMeta = isolate(myMeta_toru()),
      myExon = isolate(myExon_toru()),
      myBin = isolate(input$myBin4),
      myNorm = isolate(input$myNorm4),
      myGeneLoc = c(3943807, 3979017),
      
      myTitle="Toru",
      myKO = as.character(1:8),
      myWT = as.character(9:16),
      mysgRNA = 3943807,
      
      counttable = isolate(counttable_toru()),
      X = isolate(X4()),
      p = p_study4() # Reacts to p_study4
    )
  })
  
  output$study4p3 <- renderPlotly({
    create_p3(
      myMeta = isolate(myMeta_toru()),
      myExon = isolate(myExon_toru()),
      myBin = isolate(input$myBin4),
      myNorm = isolate(input$myNorm4),
      myGeneLoc = c(3943807, 3979017),
      
      myTitle="Toru",
      myKO = as.character(1:8),
      myWT = as.character(9:16),
      mysgRNA = 3943807,
      
      counttable = isolate(counttable_toru()),
      X = isolate(X4()),
      p = p_study4() # Reacts to p_study4
    )
  })
}

## CODE FROM COUNTTBALE-DIFF-PLOT.R BROKEN INTO FUNCTIONS ##

create_X <- function(myMeta, myExon, myBin, myNorm, myGeneLoc, mysgRNA, myTitle, myKO, myWT, counttable) {
  ## CREATE BIN INTERVALS ##
  a = c(min(counttable[, 1]),
        seq(
          round(min(counttable[, 1]) + 10**floor(log10(myBin))/2 - 1, -floor(log10(myBin))),
          max(counttable[, 1]),
          myBin
        ),
        max(counttable[, 1]))
  
  ## CREATE BINNED COUNT DATASET ##
  X = data.frame()
  
  ## ADD COLUMNS FOR LOCATION RANGE ##
  for (i in 1:(length(a)-1)) {
    X = X %>% rbind(data.frame(location=paste0(as.character(a[i]), "-", as.character(a[i+1])),
                               loc=i,
                               loc2=a[i]))
  }
  
  ## ADD COLUMNS FOR BINNED COUNTS ##
  for (sample in c(myKO, myWT)) {
    name = sample#str_split(sample, "_", n=2)[[1]][2]
    
    binned_counts = sapply(2:length(a),
                           function(i) counttable[, sample][counttable[, 1]>=a[i-1] & counttable[, 1]<a[i]] %>% sum(na.rm=T))
    
    if (myNorm==T) {
      count_total = myMeta$total[myMeta$name==name] #>> help here potentially too
      binned_counts = binned_counts %>% `/`(count_total) %>% `*`(10E6) # normalize by total count and scale up
    }
    
    X[, sample] = binned_counts
  }
  
  ## ESTIMATE AND TEST DIFFERENCE IN MEAN COUNTS BETWEEN KO AND WT ##
  for (i in 1:nrow(X)) {
    vartest = var.test(unlist(X[i, myKO]), unlist(X[i, myWT]))$p.value
    if (!is.nan(vartest)) {
      if (vartest < 0.05) { # test equal variance then do using result as assumption in t-test
        test = t.test(X[i, myKO], X[i, myWT], var.equal=F)
      } else {
        test = t.test(X[i, myKO], X[i, myWT], var.equal=T)
      }
      X[i, "meanKO"] = test$estimate[1]
      X[i, "meanWT"] = test$estimate[2]
      X[i, "diff"] = -diff(test$estimate)
      X[i, "t"] = test$statistic
      X[i, "df"] = test$parameter
      X[i, "p"] = test$p.value
      X[i, "cihi"] = test$conf.int[1]
      X[i, "cilo"] = test$conf.int[2]
    }
  }
  
  X
}

create_p <- function(myMeta, myExon, myBin, myNorm, myGeneLoc, mysgRNA, myTitle, myKO, myWT, counttable, X) {
  ## CREATE BIN INTERVALS ##
  a = c(min(counttable[, 1]),
        seq(
          round(min(counttable[, 1]) + 10**floor(log10(myBin))/2 - 1, -floor(log10(myBin))),
          max(counttable[, 1]),
          myBin
        ),
        max(counttable[, 1]))
  
  ## PREPARE PLOTS ##
  p = ggplot(X) +
    theme(text = element_text(colour="#555555"),
          axis.text = element_text(colour="#555555"),
          # axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
          axis.ticks = element_line(colour="#826a50"),
          axis.line = element_line(colour="#826a50"),
          panel.background = element_blank(),
          panel.grid = element_line(colour="#efe5db", linetype="dashed"),
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_line(colour="#efe5db", linetype="dashed")
    ) +
    geom_vline(aes(xintercept = (mysgRNA-a[2])/(tail(a, 2)[1]-a[2])*(nrow(X)-2)+1.5,
                   text=sprintf("<b>%s</b>", mysgRNA),
                   color="Target sgRNA"),
               linetype="dashed") + # mark sgRNA KO location
    scale_color_manual("", values=c("Target sgRNA"="#e84646", "Global"="#826a50", "Exons"="orange")) +
    labs(title=myTitle, x="Location") +
    scale_x_continuous(labels=X$loc2[seq(1, nrow(X), ceiling(10000/myBin))], breaks=seq(0.5, nrow(X)-0.5, ceiling(10000/myBin))) # min 10000 between x axis ticks
  
  p
}

create_p1 <- function(myMeta, myExon, myBin, myNorm, myGeneLoc, mysgRNA, myTitle, myKO, myWT, counttable, X, p) {
  ## CREATE BIN INTERVALS ##
  a = c(min(counttable[, 1]),
        seq(
          round(min(counttable[, 1]) + 10**floor(log10(myBin))/2 - 1, -floor(log10(myBin))),
          max(counttable[, 1]),
          myBin
        ),
        max(counttable[, 1]))
  
  global_test = t.test(X[, myKO], X[, myWT], var.equal=T)
  
  ## 1. PLOT MEAN COUNTS ##
  p1 = p +
    geom_col(aes(loc, meanKO, fill="KO",
                 text=sprintf("<b>%s</b><br>KO Mean count: %.2f", location, meanKO)),
             alpha=0.8) +
    geom_col(aes(loc, meanWT, fill="WT",
                 text=sprintf("<b>%s</b><br>WT Mean count: %.2f", location, meanWT)),
             alpha=0.6) +
    geom_rect(data=myExon, aes(xmin=(start-a[2])/(tail(a, 2)[1]-a[2])*(nrow(X)-2)+1.5,
                               xmax=(end-a[2])/(tail(a, 2)[1]-a[2])*(nrow(X)-2)+1.5,
                               ymin=0, ymax=max(c(X$meanKO, X$meanWT)),
                               color="Exons",
                               text=sprintf("<b>Exon %d<br>%s-%s</b><br>%s", exon, start, end, id)),
              fill="orange", alpha=0.6) + # shade exon locations
    scale_fill_manual("", values=c("Target sgRNA"="#e84646", "KO"="#c29365", "WT"="#7bb8cb")) +
    labs(y="Mean normalized counts *10E6")
  
  p1_plotly = ggplotly(p1, tooltip="text") %>% config(scrollZoom=T) %>% layout(dragmode="pan", hovermode="compare")
  for (i in 1:length(p1_plotly$x$data)){ # getting rid of strange (,1) in legend
    if (!is.null(p1_plotly$x$data[[i]]$name)){
      p1_plotly$x$data[[i]]$name =  gsub("\\(","",str_split(p1_plotly$x$data[[i]]$name,",")[[1]][1])
    }
  }
  for (i in (length(p1_plotly$x$data)-nrow(myExon)+1):length(p1_plotly$x$data)) { # Hide exons
    p1_plotly$x$data[[i]]$visible <- 'legendonly'
  }
  p1_plotly
}

create_p2 <- function(myMeta, myExon, myBin, myNorm, myGeneLoc, mysgRNA, myTitle, myKO, myWT, counttable, X, p) {
  ## CREATE BIN INTERVALS ##
  a = c(min(counttable[, 1]),
        seq(
          round(min(counttable[, 1]) + 10**floor(log10(myBin))/2 - 1, -floor(log10(myBin))),
          max(counttable[, 1]),
          myBin
        ),
        max(counttable[, 1]))
  
  global_test = t.test(X[, myKO], X[, myWT], var.equal=T)
  
  ## 2. PLOT MEAN DIFF WITH 95% CI ##
  p2 = p +
    geom_col(aes(loc, diff,
                 text=sprintf("<b>%s</b><br>%.2f (95%% CI: %.2f, %.2f)", location, diff, cilo, cihi)),
             fill="#7bb8cb", alpha=0.8) +
    geom_errorbar(aes(x=loc, ymin=cilo, ymax=cihi,
                      text=sprintf("<b>%s</b><br>%.2f (95%% CI: %.2f, %.2f)", location, diff, cilo, cihi)),
                  colour="#233b43", alpha=0.8) +
    geom_hline(aes(yintercept=round(-diff(global_test$estimate), 2), color="Global",
                   text=sprintf("<b>Global<br>%s-%s</b><br>%.2f (95%% CI: %.2f, %.2f)",
                                a[1], tail(a, 1), -diff(global_test$estimate), global_test$conf.int[1], global_test$conf.int[2])),
               alpha=0.7) +
    geom_rect(data=myExon, aes(xmin=(start-a[2])/(tail(a, 2)[1]-a[2])*(nrow(X)-2)+1.5,
                               xmax=(end-a[2])/(tail(a, 2)[1]-a[2])*(nrow(X)-2)+1.5,
                               ymin=min(c(X$cilo, X$cihi)), ymax=max(c(X$cilo, X$cihi)),
                               color="Exons",
                               text=sprintf("<b>Exon %d<br>%s-%s</b><br>%s", exon, start, end, id)),
              fill="orange", alpha=0.6) + # shade exon locations
    labs(y="Difference in Mean Counts (KO-WT)")
  
  p2_plotly = ggplotly(p2, tooltip="text") %>% config(scrollZoom=T) %>% layout(dragmode="pan")
  for (i in (length(p2_plotly$x$data)-nrow(myExon)+1):length(p2_plotly$x$data)) { # Hide exons
    p2_plotly$x$data[[i]]$visible <- 'legendonly'
  }
  p2_plotly
}

create_p3 <- function(myMeta, myExon, myBin, myNorm, myGeneLoc, mysgRNA, myTitle, myKO, myWT, counttable, X, p) {
  ## CREATE BIN INTERVALS ##
  a = c(min(counttable[, 1]),
        seq(
          round(min(counttable[, 1]) + 10**floor(log10(myBin))/2 - 1, -floor(log10(myBin))),
          max(counttable[, 1]),
          myBin
        ),
        max(counttable[, 1]))
  
  global_test = t.test(X[, myKO], X[, myWT], var.equal=T)
  
  ## 3. PLOT STANDARDIZED DIFF ##
  p3 = p +
    geom_col(aes(loc, ifelse(p<0.05, 0, t), # non-significant bars
                 text=sprintf("<b>%s</b><br>t = %.2f<br>df = %.2f<br>p = %.3f", location, t, df, p)),
             fill="rgba(35, 59, 67, 0.7)") +
    geom_col(aes(loc, ifelse(p<0.05, t, 0), # significant bars
                 text=sprintf("<b>%s</b><br>t = %.2f<br>df = %.2f<br>p = %.3f", location, t, df, p)),
             fill="rgba(35, 59, 67, 0.7)") +
    geom_hline(aes(yintercept=round(global_test$statistic, 2), color="Global",
                   text=sprintf("<b>Global<br>%s-%s</b><br>t = %.2f<br>df = %.2f<br>p = %.3f",
                                a[1], tail(a, 1), global_test$statistic, global_test$parameter, global_test$p.value)),
               alpha=0.7) + # Global test
    geom_rect(data=myExon, aes(xmin=(start-a[2])/(tail(a, 2)[1]-a[2])*(nrow(X)-2)+1.5,
                               xmax=(end-a[2])/(tail(a, 2)[1]-a[2])*(nrow(X)-2)+1.5,
                               ymin=min(X$t), ymax=max(X$t),
                               color="Exons",
                               text=sprintf("<b>Exon %d<br>%s-%s</b><br>%s", exon, start, end, id)),
              fill="orange", alpha=0.6) + # shade exon locations
    labs(y="Standardized Difference in Mean Counts (KO-WT)")
  
  updatemenus = list(list(type="buttons", buttons=list(
    
    list(method = "restyle",
         args = list("marker.color", c("rgba(194, 147, 101, 0.7)", "rgba(35, 59, 67, 0.7)")),
         label = "p<0.05"),
    list(method = "restyle",
         args = list("marker.color", c("rgba(35, 59, 67, 0.7)", "rgba(35, 59, 67, 0.7)")),
         label = "Reset"))))
  
  p3_plotly = ggplotly(p3, tooltip="text") %>% config(scrollZoom=T) %>% layout(dragmode="pan", updatemenus=updatemenus)
  for (i in (length(p3_plotly$x$data)-nrow(myExon)+1):length(p3_plotly$x$data)) { # Hide exons
    p3_plotly$x$data[[i]]$visible <- 'legendonly'
  }
  p3_plotly
}


shinyApp(ui,server)
