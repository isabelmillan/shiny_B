
#options(shiny.maxRequestSize = 6000*1024^2) # 6000 MB/ 6 GB

stat_sum_df <- function(fun, geom="point", ...) {
  stat_summary(fun.data=fun,  geom=geom,  ...)
}
stat_sum_single <- function(fun, geom="point", ...) {
  stat_summary(fun.y=fun,  geom=geom,  ...)
}

shinyServer(function(input, output, session) {
  filedata <- reactive({
    infile <- input$datafile
    if (is.null(infile)) {
      return(NULL)
    }
    read.csv(infile$datapath,na.strings = c("NA","."))
  })
  
  
  
  myData <- reactive({
    df=filedata()
    if (is.null(df)) return(NULL)
  })
  
  output$optionsmenu <-  renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    
    fluidRow(
      
      hr(),
      
      column(4,checkboxInput('showplottypes', 'Tipo de gráfica: puntos/líneas', value = TRUE)),
      column(4,checkboxInput('showfacets', 'Opciones de agrupación', value = TRUE) ),
      column(4,checkboxInput('showrqss', 'Opciones regresión intercuartil', value = TRUE))
      
      #column(4,checkboxInput('showLoess', 'Loess SE (Options ?)', value = TRUE)),
      #column(4,checkboxInput('showMean' , 'Media CI', value = FALSE)),
      
      #column(4,checkboxInput('showMedian','Mediana PIs', value = FALSE))
    )
  })
  
  output$xcol <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    
    items=names(df)
    names(items)=items
    selectInput("x", "Variable X:",items)
    
  })
  
  output$ycol <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    items=names(df)
    names(items)=items
    selectInput("y", "Variable Y:",items)
    
  })
  
  output$slider <- renderUI({
    df <-filedata()
    xvariable<- input$x
    if (is.null(df)) return(NULL)
    if (!is.numeric(df[,xvariable]) ) return(NULL)
    
    sliderInput("inSlider", paste(xvariable,"Rango"),
                min=min(df[,xvariable],na.rm=T),
                max=max(df[,xvariable],na.rm=T),
                value=c(min(df[,xvariable],na.rm=T),max(df[,xvariable],na.rm=T)) 
    )
    
  })
  
  
  output$slider2 <- renderUI({
    df <-filedata()
    yvariable<- input$y
    if (is.null(df)) return(NULL)
    if (!is.numeric(df[,yvariable]) ) return(NULL)
    sliderInput("inSlider2", paste(yvariable,"Rango"),
                min=min(df[,yvariable],na.rm=T),
                max=max(df[,yvariable],na.rm=T),
                value=c(min(df[,yvariable],na.rm=T),max(df[,yvariable],na.rm=T)) 
    )
  })
  

  
  
  output$filtervar1 <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    valores <- sapply(df, function(x) (unique(x)))
    valores<- names(valores)
    #NAMESTOKEEP<- names(df)  [ NUNIQUEDF  < input$inmaxlevels ]
    selectInput("infiltervar1" , "Filtro variable1",c('Ninguno', valores ) )
  })
  
  output$filtervar1values <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    if(input$infiltervar1=="Ninguno") {
      return(NULL)  
    }
    if(input$infiltervar1!="Ninguno" )  {
      choices <- levels(as.factor(df[,input$infiltervar1]))
      selectInput('infiltervar1valuesnotnull',
                  label = paste("Selecciona valores", input$infiltervar1),
                  choices = choices,
                  selected = "",
                  multiple=TRUE, selectize=TRUE)   
    }
  })  
  
  subsetdata  <- reactive({
    df <- filedata()
    if (is.null(df)) return(NULL)
    if(!is.numeric( input$inSlider[1]) ) {
      df 
    }
    
    if(!is.numeric( input$inSlider2[1]) ) {
      df 
    }
    
    
    if(is.numeric( input$inSlider[1]) & is.numeric(df[,input$x])) {
      df<- df [!is.na(df[,input$x]),]
      
      df <-  df [df[,input$x] >= input$inSlider[1]&df[,input$x] <= input$inSlider[2],]
      
    }
    
    if(is.numeric( input$inSlider2[1])& is.numeric(df[,input$y]) ) {
      df<- df [!is.na(df[,input$y]),]
      
      df <-  df [df[,input$y] >= input$inSlider2[1]&df[,input$y] <= input$inSlider2[2],]
      
    }  
    
    if(is.null(input$infiltervar1)) {
      
      df <-  df 
    }
    
    
    if(!is.null(input$infiltervar1)&input$infiltervar1!="Ninguno") {
      
      df <-  df [ is.element(df[,input$infiltervar1],input$infiltervar1valuesnotnull),]
    }
    df
  })
  
  output$filtervar2 <- renderUI({
    df <- filedata()
    if (is.null(df)) return(NULL)
    valores <- sapply(df, function(x) (unique(x)))
    valores<- names(valores)
    #NAMESTOKEEP<- names(df)  [ NUNIQUEDF  < input$inmaxlevels ]
    valores<-  valores[ valores!=input$infiltervar1 ]
    selectInput("infiltervar2" , "Filtro variable 2:",c('Ninguno', valores ) )
  })
  
  output$filtervar2values <- renderUI({
    df <- subsetdata()
    
    if (is.null(df)) return(NULL)
    if(input$infiltervar2=="Ninguno") {
      selectInput('infiltervar2valuesnull',
                  label ='No se especificó ningún filtro para la variable 2', 
                  choices = list(""),multiple=TRUE, selectize=FALSE)   
    }
    if(input$infiltervar2!="Ninguno"&!is.null(input$infiltervar2) )  {
      choices <- levels(as.factor(as.character(df[,input$infiltervar2])))
      selectInput('infiltervar2valuesnotnull',
                  label = paste("Selecciona valores", input$infiltervar2),
                  choices = choices,
                  selected = "",
                  multiple=TRUE, selectize=TRUE)   
    }
  })   
  
  subsetdata2  <- reactive({
    df <- subsetdata()
    if (is.null(df)) return(NULL)
    if(!is.null(input$infiltervar2)&input$infiltervar2!="Ninguno") {
      df <-  df [ is.element(df[,input$infiltervar2],input$infiltervar2valuesnotnull),]
    }
    if(input$infiltervar2=="Ninguno") {
      df 
    }
    df
  })
  
  
  output$colour <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    items=names(df)
    names(items)=items
    selectInput("colorin", "Colorear por:",c("Ninguno",items ) )
    
  })
  
  output$group <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    items=names(df)
    names(items)=items
    selectInput("groupin", "Agrupar por:",c("Ninguno",items ) )
    
  })
  
  output$facet_col <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    items=names(df)
    names(items)=items
    selectInput("facetcolin", "Split por columna",c(Ninguno='.',items)
    )
    
    
    
  })
  output$facet_row <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    items=names(df)
    names(items)=items
    selectInput("facetrowin", "Split por renglón",    c(Ninguno=".",items)
    )
    
  })
  
  output$pointsize <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    items=names(df)
    names(items)=items
    selectInput("pointsizein", "Tamaño por:",    c("Ninguno",items)
    )
    
  })
  
  
  output$fill <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    items=names(df)
    names(items)=items
    selectInput("fillin", "Relleno por:",    c("Ninguno",items)
    )
    
  })
  
  output$weight <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    items=names(df)
    names(items)=items
    selectInput("weightin", "Peso por:",    c("Ninguno",items)
    )
    
  })
  
  
  plotObject <- reactive({
    plotdata <- subsetdata2()
    
    if(!is.null(plotdata)) {
      
      p <- ggplot(plotdata, aes_string(x=input$x, y=input$y)) 
      
      if (input$Points=="Points"&input$pointsizein == 'Ninguno')
        p <- p + geom_point(,alpha=input$pointstransparency,shape=input$pointtypes,size=input$pointsizes)  
      if (input$Points=="Points"&input$pointsizein != 'Ninguno')
        p <- p + geom_point(,alpha=input$pointstransparency,shape=input$pointtypes) 
      if (input$Points=="Points"&input$colorpunto != 'Ninguno')
        p <- p + geom_point(,alpha=input$pointstransparency,shape=input$pointtypes, color=input$colorpunto) 
      if (input$line=="Lines"&input$pointsizein == 'Ninguno')
        p <- p + geom_line(,size=input$linesize,alpha=input$linestransparency,linetype=input$linetypes)
      
      if (input$line=="Lines"&input$pointsizein != 'Ninguno')
        p <- p + geom_line(,alpha=input$linestransparency,linetype=input$linetypes)
      
      if (input$pointsizein != 'Ninguno')
        p <- p  + aes_string(size=input$pointsizein)
      
      if (input$Points=="Jitter")
        p <- p + geom_jitter()
      if (input$colorin != 'Ninguno')
        p <- p + aes_string(color=input$colorin)
      if (input$fillin != 'Ninguno')
        p <- p + aes_string(fill=input$fillin)
      
      if (input$groupin != 'Ninguno' & !is.factor(plotdata[,input$x]))
        p <- p + aes_string(group=input$groupin)
      
      if (input$groupin != 'Ninguno' & is.factor(plotdata[,input$x]))
        p <- p + aes(group=1)
      
      
      ###### Regresión cuantilica
      #if (!input$ignoregroupqr) {
        if (!input$ignorecolqr) {
          if (input$Tauvalue) {
            if(!input$hidedynamic){
              p <- p +  stat_quantile(method = "rqss",quantiles =input$Tau,size=1.5,
                                      linetype="solid", 
                                      formula=y ~ qss(x))#, constraint= input$Constraints,
              #lambda=input$Penalty))   
            }
            
            if (input$mitad)
              p <- p +  stat_quantile(method = "rqss",quantiles = 0.5,size=1.5,
                                      linetype="solid",
                                      formula=y ~ qss(x))#, constraint= input$Constraints,
            #lambda=input$Penalty))
            if (input$noventa)
              p <- p +  stat_quantile(method = "rqss",quantiles = 0.90,size=1,
                                      linetype="dashed",
                                      formula=y ~ qss(x))#, constraint= input$Constraints,
            #lambda=input$Penalty))
            if (input$diez)
              p <- p +  stat_quantile(method = "rqss",quantiles = 0.1,size=1,
                                      linetype="dashed",
                                      formula=y ~ qss(x))#, constraint= input$Constraints,
            #lambda=input$Penalty))
            
            if (input$superior)
              p <- p +  stat_quantile(method = "rqss",quantiles = 0.95,size=1,
                                      linetype="dashed",
                                      formula=y ~ qss(x))#, constraint= input$Constraints,
            #lambda=input$Penalty))
            
            if (input$inferior) 
              p <- p +  stat_quantile(method = "rqss",quantiles = 0.05,size=1,
                                      linetype="dashed",
                                      formula=y ~ qss(x))#, constraint= input$Constraints,
            #lambda=input$Penalty))
            
            
            
          }
        }
        if (input$ignorecolqr) {
          colqr <- input$colqr
          if (input$Tauvalue) {
            if(!input$hidedynamic){
              p <- p +  stat_quantile(method = "rqss",quantiles =input$Tau,size=1.5,
                                      linetype="solid", col=colqr,
                                      formula=y ~ qss(x))#, constraint= input$Constraints,
                                                      #lambda=input$Penalty)) 
            }
            
            
            if (input$mitad)
              p <- p +  stat_quantile(method = "rqss",quantiles = 0.5,size=1.5,
                                      linetype="solid", col=colqr,
                                      formula=y ~ qss(x))#, constraint= input$Constraints,
            #lambda=input$Penalty))
            if (input$noventa)
              p <- p +  stat_quantile(method = "rqss",quantiles = 0.90,size=1,
                                      linetype="dashed", col=colqr,
                                      formula=y ~ qss(x))#, constraint= input$Constraints,
            #lambda=input$Penalty))
            if (input$diez)
              p <- p +  stat_quantile(method = "rqss",quantiles = 0.1,size=1,
                                      linetype="dashed", col=colqr,
                                      formula=y ~ qss(x))#, constraint= input$Constraints,
            #lambda=input$Penalty))
            
            if (input$superior)
              p <- p +  stat_quantile(method = "rqss",quantiles = 0.95,size=1,
                                      linetype="dashed", col=colqr,
                                      formula=y ~ qss(x))#, constraint= input$Constraints,
            #lambda=input$Penalty))
            
            if (input$inferior) 
              p <- p +  stat_quantile(method = "rqss",quantiles = 0.05,size=1,
                                      linetype="dashed", col=colqr,
                                      formula=y ~ qss(x))#, constraint= input$Constraints,
            #lambda=input$Penalty))
            
            
            
          }
        }
      #}
      
      
      #if (input$ignoregroupqr) {
      #  if (!input$ignorecolqr) {
      #    if (input$Tauvalue) {
      #      if(!input$hidedynamic){
      #        p <- p +  stat_quantile(aes(group=NULL),method = "rqss",quantiles =input$Tau,size=1.5,
      #                                linetype="solid",
      #                                formula=y ~ qss(x))#, constraint= input$Constraints,
              #lambda=input$Penalty))
      #      }
            
      #      if (input$mitad)
      #        p <- p +  stat_quantile(aes(group=NULL),method = "rqss",quantiles = 0.5,size=1.5,
      #                                linetype="solid",
      #                                formula=y ~ qss(x))#, constraint= input$Constraints,
            #lambda=input$Penalty))
      #      if (input$noventa)
      #        p <- p +  stat_quantile(aes(group=NULL),method = "rqss",quantiles = 0.90,size=1,
      #                                linetype="dashed", 
      #                                formula=y ~ qss(x))#, constraint= input$Constraints,
            #lambda=input$Penalty))
      #      if (input$diez)
      #        p <- p +  stat_quantile(aes(group=NULL),method = "rqss",quantiles = 0.1,size=1,
      #                                linetype="dashed",
      #                                formula=y ~ qss(x))#, constraint= input$Constraints,
            #lambda=input$Penalty))
            
      #      if (input$superior)
      #        p <- p +  stat_quantile(aes(group=NULL),method = "rqss",quantiles = 0.95,size=1,
      #                                linetype="dashed", 
      #                                formula=y ~ qss(x))#, constraint= input$Constraints,
            #lambda=input$Penalty))
            
      #      if (input$inferior) 
      #        p <- p +  stat_quantile(aes(group=NULL),method = "rqss",quantiles = 0.05,size=1,
      #                                linetype="dashed", 
      #                                formula=y ~ qss(x))#, constraint= input$Constraints,
            #lambda=input$Penalty))
      #    }
      #  }
      #  if (input$ignorecolqr) {
      #    colqr <- input$colqr
      #    if (input$Tauvalue) {
      #      if(!input$hidedynamic){
      #        p <- p +  stat_quantile(aes(group=NULL),method = "rqss",quantiles =input$Tau,size=1.5,
      #                                linetype="solid",col=colqr,
      #                                formula=y ~ qss(x))#, constraint= input$Constraints,
              #lambda=input$Penalty))
      #      }
            
            
      #      if (input$mid)
      #        p <- p +  stat_quantile(aes(group=NULL),method = "rqss",quantiles = 0.5,size=1.5,
      #                                linetype="solid", col=colqr,
      #                                formula=y ~ qss(x))#, constraint= input$Constraints,
            #lambda=input$Penalty))
      #      if (input$noventa)
      #        p <- p +  stat_quantile(aes(group=NULL),method = "rqss",quantiles = 0.90,size=1,
      #                                linetype="dashed", col=colqr,
      #                                formula=y ~ qss(x))#, constraint= input$Constraints,
            #lambda=input$Penalty))
      #      if (input$diez)
      #        p <- p +  stat_quantile(aes(group=NULL),method = "rqss",quantiles = 0.1,size=1,
      #                                linetype="dashed", col=colqr,
      #                                formula=y ~ qss(x))#, constraint= input$Constraints,
            #lambda=input$Penalty))
            
      #      if (input$superior)
      #        p <- p +  stat_quantile(aes(group=NULL),method = "rqss",quantiles = 0.95,size=1,
      #                                linetype="dashed", col=colqr,
      #                                formula=y ~ qss(x))#, constraint= input$Constraints,
            #lambda=input$Penalty))
            
      #      if (input$inferior) 
      #        p <- p +  stat_quantile(aes(group=NULL),method = "rqss",quantiles = 0.05,size=1,
      #                                linetype="dashed", col=colqr,
      #                                formula=y ~qss(x))#, constraint= input$Constraints,
            #lambda=input$Penalty))
            
      #    }
      #  }
     # }
      
      
      ###### RQSS SECTION END
      
      
      if (input$ylab!="")
        p <- p + ylab(input$ylab)
      
      if (input$xlab!="")
        p <- p + xlab(input$xlab)
      
      if (input$horizontalzero)
        p <-    p+
        geom_hline(aes(yintercept=0))
      
      if (input$customline1)
        p <-    p+
        geom_vline(xintercept=input$vline)
      
      
      if (input$customline2)
        p <-    p+
        geom_hline(yintercept=input$hline)
      
      
      
      if (input$identityline)
        p <-    p+ geom_abline(intercept = 0, slope = 1)
      
      p
    }
  })
  
  output$plot <- renderPlot({
    plotObject()
  })
  
  
  
  ##### Clicks interactivos
  output$plotinfo <- renderPrint({
    df<- subsetdata2()  
    if (is.null(df)) return(NULL)
    nearPoints( subsettdata2(), input$plot_click, threshold = 5, maxpoints = 5,
                addDist = TRUE,xvar=input$x, yvar=input$y)
    
  })
  
  
  output$clickheader <-  renderUI({
    df <-subsetdata2()
    if (is.null(df)) return(NULL)
    h4("Puntos seleccionados con click")
  })
  
  output$brushheader <-  renderUI({
    df <-subsetdata2()
    if (is.null(df)) return(NULL)
    h4("Puntos seleccionados al mismo tiempo")
    
  })
  
  output$plot_clickedpoints <- renderTable({

    df<- subsetdata2() 
    if (is.null(df)) return(NULL)
    
    res <- nearPoints(subsetdata2(), input$plot_click, input$x, input$y)
    colnames(res)<- colnames(subsetdata2())
    if (nrow(res) == 0|is.null(res))
      return(NULL)
    
    trans<- t(res)
    rownames(trans)<- colnames(res)
    trans
  
  }, include.rownames= TRUE, include.colnames = FALSE)
  
  output$plot_brushedpoints <- renderTable({
    df<- subsetdata2()  
    if (is.null(df)) return(NULL)
    res <- brushedPoints(subsetdata2(), input$plot_brush, input$x, input$y)
    if (nrow(res) == 0|is.null(res))
      return(NULL)
    trans<- t(res)
    rownames(trans)<- colnames(res)
    trans
    
  }, include.rownames= TRUE, include.colnames = FALSE)
  

  downloadPlotType <- reactive({
    input$downloadPlotType  
  })
  
  observe({
    plotType    <- input$downloadPlotType
    plotTypePDF <- plotType == "pdf"
    plotUnit    <- ifelse(plotTypePDF, "pulgadas", "pixeles")
    plotUnitDef <- ifelse(plotTypePDF, 7, 480)
    
    updateNumericInput(
      session,
      inputId = "downloadPlotHeight",
      label = sprintf("Altura (%s)", plotUnit),
      value = plotUnitDef)
    
    updateNumericInput(
      session,
      inputId = "downloadPlotWidth",
      label = sprintf("Ancho (%s)", plotUnit),
      value = plotUnitDef)
    
  })
  
  
  # Get the download dimensions.
  downloadPlotHeight <- reactive({
    input$downloadPlotHeight
  })
  
  downloadPlotWidth <- reactive({
    input$downloadPlotWidth
  })
  
  # Get the download file name.
  downloadPlotFileName <- reactive({
    input$downloadPlotFileName
  })
  
  # Include a downloadable file of the plot in the output list.
  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste(downloadPlotFileName(), downloadPlotType(), sep=".")   
    },
    content = function(con) {
      plotFunction <- match.fun(downloadPlotType())
      plotFunction(con, width = downloadPlotWidth(), height = downloadPlotHeight())
      print(plotObject())
      dev.off(which=dev.cur())
    }
  )
  
  
}
)