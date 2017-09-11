library(shiny)
library(ggplot2)
library(ggvis)
options(shiny.maxRequestSize = 1000*1024^2)



shinyServer(function(input, output,session) 
{
  InputData <- reactive({inFile <- input$data
  if (is.null(inFile))
    return(NULL)
  read.csv(inFile$datapath, header=input$header, sep=input$sep, quote=input$quote,stringsAsFactors=FALSE)
  })
  
  output$Summary<-renderTable({
    if (is.null(InputData()))
      return(NULL)
    
    summary(InputData())
    
  })
  output$headname <- renderText({
    paste("First",input$obs, "observations" )
  })
  
  output$head<-renderTable({
    if (is.null(InputData()))
      return(NULL)
    
    
    head(InputData(),n=input$obs)
    
    
  })
  output$str<-renderPrint({
    if (is.null(InputData()))
      return(NULL)
    str(InputData())
    
    
  })
  
  
  observe({
    data1 <- InputData()
    updateSelectInput(session,"xvar",choices=names(data1))
  })
  observe({
    data2 <- InputData()
    updateSelectInput(session,"yvar",choices=names(data2))
  })
  observe({
    data3 <- InputData()
    updateSelectInput(session,"xvar1",choices=names(data3))
  })
  observe({
    data4 <- InputData()
    updateSelectInput(session,"yvar1",choices=names(data4))
  })
  observe({
    data5 <- InputData()
    updateSelectInput(session,"fill1",choices=names(data5))
  })
  
  
  
  output$barplot<-renderPlot({ 
    if (is.null(InputData()))
    {return(NULL)}
    else {
      if(input$yvar == "None")
      {ggplot(InputData(), aes_string(x=input$xvar)) +
          geom_bar(width=0.5,colour="green")}
      else{
        ggplot(InputData(), aes_string(x=input$xvar,fill=input$yvar)) +
          geom_bar(width=0.5,colour="green")
        
      }}}) 
  
  
  output$hist<-renderPlot({
    if (is.null(InputData()))
      return(NULL)
    ggplot(InputData(), aes_string(x=input$xvar)) +
      geom_histogram(width=0.5,fill="yellow",colour="darkred")
    
  }) 
  
  output$ScatterPlot <- renderPlot({
    if (is.null(InputData()))
      return(NULL)
    ggplot(InputData(), aes_string(x=input$xvar, y=input$yvar)) + geom_point(colour="darkblue")
    
  })
  output$beanplot <- renderPlot({
    if (is.null(InputData()))
      return(NULL)
    ggplot(InputData(), aes_string(x=input$xvar, y=input$yvar)) + geom_boxplot(colour="black")
    
  })
  
  
  #abc<-reactive({if (is.null(InputData()))
  #  return(NULL)
  #  else {                   
  #    B<-subset( InputData(), select=c(input$xvar1,input$yvar1,input$fill1))
  #    names(B)<-c("x","y","fill")
  #    return(B)
  #  }})
  
  
  #observe({
  #  if (is.null(InputData()))
  #    return(NULL) else {
        
   #     abc() %>% ggvis(x=~x,y=~y,fill=~fill) %>%
  #        layer_points(size := input_slider(25, 300, label = "Bubble Size")) %>%
  #        add_axis("x", title = input$xvar1) %>%
   #       add_axis("y", title = input$yvar1) %>%
  #        add_legend("fill", title = input$fill1) %>% bind_shiny("plot_scatter", "plot_scatter_ui")
  #    }})
  
  #observe({
  #  if (is.null(InputData()))
  #    return(NULL) else {
  #      abc() %>% ggvis(x=~x,y=~y,fill=~fill) %>%
  #        layer_points() %>%
  #        layer_smooths(span = input_slider(0.1, 1))%>%
  #        add_axis("x", title = input$xvar1) %>%
  #        add_axis("y", title = input$yvar1) %>%
  #        add_legend("fill", title = input$fill1) %>% bind_shiny("plot_smooth", "plot_smooth_ui")
   #   }})
  
})