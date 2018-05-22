#.libPaths("/home/shiny/libs")
library(openxlsx)
library(visNetwork)
library(igraph)
library(shiny)
library(shinydashboard)
library(shinyBS)
library(shinyjs)
source("./Rsource/SwitchButton.R")

shinyUI(dashboardPage(
  dashboardHeader(title = "PatientNet",
                  tags$li(class = "dropdown notifications-menu",
                          HTML('<a href="#" class="dropdown-toggle" data-toggle="dropdown"> 
                               <i class="fa fa-user"></i>
                               <span class="label label-primary">1</span></a>
                               <ul class="dropdown-menu"> 
                               <li> <ul class="menu"> 
                               <li> <a href="mailto:liye.he@helsinki.fi">
                               <i class="fa fa-envelope-o" aria-hidden="true"></i>
                               Author: Liye He <liye.he@helsinki.fi>
                              </a>'))),
  dashboardSidebar(
    # custom css for buttons and sliderinput               
    tags$head( 
      tags$link(rel = "stylesheet", type = "text/css", href = "allcss.css"),
      tags$link(rel = "stylesheet", type = "text/css", href = "tooltip-curved.css"),
      tags$link(rel = "stylesheet", type = "text/css", href = "drop.css"),
      tags$script(src = "feedback_source.js"),
      tags$script(src = "feedback.js"), #also to top button
      tags$script(src = "tour.js")
    ),
    
    downloadButton(outputId = "loadExData_small", label = "example data", class = "butEx"),
    div(
      id = "filetype",
      switchButton(inputId = "separate", label = "Separate input files", value = FALSE)
    ),
    #,
    div(id = "singleinput",
        tags$form(id = "testing",
          conditionalPanel(
            condition = " input.separate != true",
            
            fileInput('dataAll', 'Upload a single file in xlsx format', accept = c(".xlsx"))
            
            
          )
        )
        
    ),
    conditionalPanel(
      condition = " input.separate == true",
      fileInput('drugTarget', 'Upload drug target data', accept = c(".csv")),
      fileInput('dss', 'Upload drug response data', accept = '.csv'),
      fileInput('mut', 'Upload mutation data', accept = '.csv'),
      fileInput('exp', 'Upload gene expression data', accept = '.csv')
    ),
    
    
    div(id = "net", actionButton("Nets", "Construct Patient Network")),
    
    hr(),
    div(id = "save", valueBox(downloadButton("Save", "Save"), 
                              "Save results", icon = icon("floppy-save", lib = "glyphicon"),
                              width = 13, color = "red"))
  ),
  dashboardBody(
    useShinyjs(),
    
    div(
      br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),
      fluidRow(column(offset = 3, width = 6,
                      div(
                        HTML('<div class="primary_header"><h1>Welcome to PatientNet</h1>
                             <h2>a shiny web application for visualizing patient-specific cancer vulnerability networks</h2></div>'
                        ),  br(),
                        HTML('<button type="button" id="buttonTour" class="btn btn-primary btn-lg">
                             <span class="glyphicon glyphicon-play"></span>
                             Start the tour
                             </button>'), id = "startour")
                        )), id = "wraptour"),
    uiOutput("showInputData"),
    div(id = "networks", uiOutput("showNetwork"))
    
    
  )
  
))


# shinyUI(fluidPage(
#   # titlePanel("Visualize patient-specific network"),
#   # sidebarLayout(
#   #   sidebarPanel(
#   #     fileInput(inputId = "data", "Upload data", accept = "xlsx"),
#   #     checkboxInput("visualiz", "Visualize", value = FALSE)
#   #   ), 
#   #   mainPanel(
#   #     uiOutput('tabs'),
#   #     conditionalPanel(condition = "input.visualiz == true",
#   #                      visNetworkOutput('zaman.sub'))
#   #     
#   #   )
#   # )
#   titlePanel("Visualize patient-specific network"),
#   fluidRow(
#     column(3,
#            fileInput(inputId = "data", "Upload data", accept = "xlsx"),
#            checkboxInput("visualiz", "Visualize", value = FALSE)
#     ),
#     column(9, 
#            uiOutput('tabs'),
#            fluidRow(column(9, 
#                            conditionalPanel(condition = "input.visualiz == true", 
#                                             visNetworkOutput('zaman.sub', height = "900"))),
#                     column(3, conditionalPanel(
#                       condition = "input.visualiz == true", imageOutput("extraLegend")
#                     ))
#                     )
#           
#     )
#   )
# )
# )