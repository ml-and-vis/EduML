library(shiny)
library(shinydashboard)

source("common.R")
source("modules.R")

ui <- dashboardPage(

  dashboardHeader(title = "EduML",
                  titleWidth = 300),

  dashboardSidebar(
    tags$head(  tags$style(HTML(style.H5)) ),
    width = 300,
    sidebarMenu(
      menuItem(
        "Menu",
        tabName = "modules",
        startExpanded = TRUE,
        menuSubItem(
          "Getting Started",
          tabName = "gettingstarted",
          icon = icon("play")
        ),
        menuSubItem(
          "Input data",
          tabName = "visualisation",
          icon = icon("chart-bar")
        ),
        menuSubItem(
          "Classification",
          tabName = "classification",
          icon = icon("table")
        ),
        menuSubItem(
          "Exploration",
          tabName = "exploration",
          icon = icon("flask")
        ),
        menuSubItem(
          "Load Own Data",
          tabName = "loadown",
          icon = icon("database")
        ),
        menuItem("About", tabName = "about", icon = icon("address-card"))
      ),
      menuItem(
        #width = 400,
        #type = "pills",
        startExpanded = TRUE,
        helpText("Configure your ML pipeline"),
        
                             
        # UI panels of modules
        # <EXTENSION POINT: add your own panel here>
        moduleRegistry$lookup("DA")$createUIPanel(),
        moduleRegistry$lookup("SC")$createUIPanel(),
        moduleRegistry$lookup("DR")$createUIPanel(),
        moduleRegistry$lookup("MS")$createUIPanel()

      )
    )
  ),
  dashboardBody(
    tabItems(tabItem(tabName = "gettingstarted",
            htmlOutput("help.text")),
    tabItem(tabName = "visualisation",
            h3("Input data"),
            tabsetPanel(
              tabPanel("Scatter plot matrix", plotOutput("vis.scatterPlot")),
              tabPanel("Parallel coordinates", plotOutput("vis.parCoordsPlot")),
              tabPanel("Density by class", plotOutput("vis.densityPlot")),
              tabPanel("Box plot by class", plotOutput("vis.boxPlot")),
              tabPanel("Correlation matrix (Pearson)", plotOutput("vis.corrPlot")),
              tabPanel("Feature importance", plotOutput("vis.featImportancePlot")),
              tabPanel("Data properties", verbatimTextOutput("vis.datasetInfo")),
              tabPanel("Raw data", tableOutput("vis.dataAsTable"))
            )
            ),
    tabItem(
      tabName = "classification",
      h3("Classification"),
      tabsetPanel(
        tabPanel("Results on test set", verbatimTextOutput("cls.confusionMatrix")),
        tabPanel("Results on train set", verbatimTextOutput("cls.confusionMatrixTraining")),
        tabPanel("Model", verbatimTextOutput("cls.model")),
        tabPanel("Scatter plot of test set", plotOutput("cls.scatterPlot")),
        tabPanel("Full results", verbatimTextOutput("cls.fullResults")),
        tabPanel("R Source code snippet", downloadButton("downloadSrcR", "Download R source code snippet"), verbatimTextOutput("cls.srcCodeR")),
        tabPanel("Python Source code snippet", downloadButton("downloadSrcPy", "Download python source code snippet"), verbatimTextOutput("cls.srcCodePy"))
      )
    ),
    tabItem(
      tabName = "exploration",
      h4("Exploration"),
            tabsetPanel(
              tabPanel("Adjust hyperparameters", 
                       
                       verticalLayout(uiOutput("exp.sliderParam1"),
                                      uiOutput("exp.sliderParam2"),
                                      uiOutput("exp.sliderParam3"),
                                      plotOutput("exp.explorationPlot"),
                                      verbatimTextOutput("exp.test"))
              ),
              tabPanel("Results on test set", verbatimTextOutput("exp.confusionMatrix")),
              tabPanel("Results on train set", verbatimTextOutput("exp.confusionMatrixTraining")),
              tabPanel("Model", verbatimTextOutput("exp.model"))
            )
    ),
    tabItem(
      tabName = "loadown",
      h4("Load own dataset"),
      
      # maximum upload size by shiny: 5MB
      # see: https://stackoverflow.com/questions/18037737/how-to-change-maximum-upload-size-exceeded-restriction-in-shiny-and-save-user
      textOutput("file.text"),
      
      tags$hr(),
      fileInput("file.CSVFile", "Load CSV file (separator: , / dec. point: .)",width="100%",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      
      checkboxInput("file.header", "Header in first line?", width="100%",TRUE),
      selectInput("file.ClassColumn", width="100%",
                  label = "Select class column",
                  choices = c("")
      ),
      tags$hr(),
      actionButton("file.load", "Load file", width="100%"),
      tags$hr(),
      
      tableOutput("file.dataAsTable")
    ),
    tabItem(
      tabName = "about",
      h4("About"),
      htmlOutput("about.text")
      )
    )
  )
)
    

