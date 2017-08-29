options(shiny.maxRequestSize = 1000*1024^2)
library(shiny)
library(shinydashboard)
library(leaflet)
library(data.table)
library(ggplot2)



title <- "Gráficas de dispersión"


shinyUI(pageWithSidebar(
  
  headerPanel(title),
  
  
  sidebarPanel(
    fileInput('file1', 'Choose CSV File',
              accept=c('text/csv','text/comma-separated-values,text/plain','.csv')),
    tags$hr(),
    checkboxInput('header', 'Header', TRUE),
    fluidRow(
      column(11, selectizeInput("xaxisGrp","Eje X",
                               c("1"="1","2"="2"))),
      column(11, selectizeInput("yaxisGrp","Eje Y",
                               c("1"="1","2"="2")))
    ),
    radioButtons('sep', 'Separator',
                 c(Coma=',', Punto_coma=';',Tab='\t'), ','),
    uiOutput("choose_columns")
  ),
  mainPanel(
    tabsetPanel(
      tabPanel("Gráficas",plotOutput("plot"))
    )
  )
))