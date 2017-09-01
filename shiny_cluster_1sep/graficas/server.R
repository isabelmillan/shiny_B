### Gráficas

options(shiny.maxRequestSize = 1000*1024^2)
library(shiny)
library(shinydashboard)
library(leaflet)
library(data.table)
library(ggplot2)

shinyServer(function(input,output,session) {
  
  dsnames <- c()
  
  data_set <- reactive({
    inFile <- input$file1
    
    if (is.null(inFile))
      return(mtcars)
    
    data_set<-read.csv(inFile$datapath, header=input$header, 
                       sep=input$sep, quote=input$quote)
  })
  
  #output$contents <- renderTable({data_set()})
  
  observe({
    dsnames <- names(data_set())
    cb_options <- list()
    cb_options[ dsnames] <- dsnames
    updateSelectizeInput(session, "xaxisGrp",
                       label = "Eje X",
                       choices = cb_options,
                       selected = "")
    updateSelectizeInput(session, "yaxisGrp",
                             label = "Eje Y",
                             choices = cb_options,
                             selected = "")
  })
  output$choose_dataset <- renderUI({
    selectInput("dataset", "Data set", as.list(data_sets))
  })
  output$plot = renderPlot(
    {
      df <- data_set()
      gp <- NULL
      if (!is.null(df)){
        xv <- input$xaxisGrp
        yv <- input$yaxisGrp
        if (!is.null(xv) & !is.null(yv)){
          if (sum(xv %in% names(df))>0){ # eliminar error al cambiar archivo
            mdf <- melt(df,id.vars=xv,measure.vars=yv)
            gp <- ggplot(data=mdf) + 
              geom_point(aes_string(x=xv,y="value",color="variable"))
          }
        }
      }
      return(gp)
    }
  )
 # output$choose_columns <- renderUI({
    
  #  if(is.null(input$dataset))
  #    return()
  #  colnames <- names(contents)
  #  checkboxGroupInput("columns", "Choose columns", 
  #                     choices  = colnames,
   #                    selected = colnames)
  #}) 
})