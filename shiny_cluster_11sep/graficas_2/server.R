###### Análisis gráfico

source("utiles.R")
options(shiny.maxRequestSize = 6000*1024^2) # 6000 MB/ 6 GB

shinyServer(function(input, output, session) {
  filedata1 <- reactive({
    infile <- input$datafile
    if (is.null(infile)) {
      return(NULL)
    }
    read.csv(infile$datapath,na.strings = c("NA","."))
  })

##########################################################################################################  
#################################### Filtros automáticos##################################################
##########################################################################################################
# Se llama a la función de filtrado en utiles.R
  
  filedata <- reactive({
    filtrar(filedata1())
  })
  
  myData <- reactive({
    df=filedata()
    if (is.null(df)) return(NULL)
  })
  
##########################################################################################################  
###################### Menú de opciones: puntos, líneas y regresión cuantílica############################
##########################################################################################################
  
  output$optionsmenu <-  renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    
    fluidRow(
      
      hr(),
      
      column(4,checkboxInput('showplottypes', 'Tipo de gráfica: puntos/líneas', value = TRUE)),
      column(4,checkboxInput('showfacets', 'Opciones de agrupación', value = TRUE) ),
      column(4,checkboxInput('showrqss', 'Opciones regresión cuantílica', value = TRUE))
      
    )
  })

###########################################################################################################  
######################################### Selección  de variables##########################################
###########################################################################################################
  
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
 
##########################################################################################################  
###################################### Filtros de variables seleccionadas#################################
##########################################################################################################
  
#Rango del filtro 1  (eje x)
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
  
# Rango del filtro 2  (eje y)
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
  
# Filtro variable extra 1
  output$filtervar1 <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    valores <- sapply(df, function(x) (unique(x)))
    valores<- names(valores)
    selectInput("infiltervar1" , "Filtro variable1",c('Ninguno', valores ) )
  })
  
 # Opciones filtro variable extra 1
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

  # Filtro variable extra 2  
  output$filtervar2 <- renderUI({
    df <- filedata()
    if (is.null(df)) return(NULL)
    valores <- sapply(df, function(x) (unique(x)))
    valores<- names(valores)
    valores<-  valores[ valores!=input$infiltervar1 ]
    selectInput("infiltervar2" , "Filtro variable 2:",c('Ninguno', valores ) )
  })
  
# Valores variable extra 2  
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
 
############################################################################################################
##################################### Filtros gráficos #######################################################
############################################################################################################
  
# Colores en la gráfica por variable  
  output$colour <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    items=names(df)
    names(items)=items
    selectInput("colorin", "Colorear por:",c("Ninguno",items ) )
    
  })

# Agrupaciones por variable  
  output$group <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    items=names(df)
    names(items)=items
    selectInput("groupin", "Agrupar por:",c("Ninguno",items ) )
    
  })
  
# Split por columna en la gráfica  
  output$facet_col <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    items=names(df)
    names(items)=items
    selectInput("facetcolin", "Split por columna",c(Ninguno='.',items)
    )
  })
  
  #Split por renglón en la gráfica
  output$facet_row <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    items=names(df)
    names(items)=items
    selectInput("facetrowin", "Split por renglón",    c(Ninguno=".",items)
    )
  })
  
# Tamaño del punto por variable  
  output$pointsize <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    items=names(df)
    names(items)=items
    selectInput("pointsizein", "Tamaño por:",    c("Ninguno",items)
    )
  })
  
# Relleno de los puntos por variable 
  output$fill <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    items=names(df)
    names(items)=items
    selectInput("fillin", "Relleno por:",    c("Ninguno",items)
    )
    
  })
  
 # output$weight <- renderUI({
#    df <-filedata()
#    if (is.null(df)) return(NULL)
#    items=names(df)
#    names(items)=items
#    selectInput("weightin", "Peso por:",    c("Ninguno",items)
#    )
    
#  })
  
############################################################################################################
############################################### Gráfica de dispersión ######################################
############################################################################################################
  
  plotObject <- reactive({
    plotdata <- subsetdata2()
    
    if(!is.null(plotdata)) {
      
      p <- ggplot(plotdata, aes_string(x=input$x, y=input$y)) 
  ##### Inputs te puntos y líneas
      
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
      
      
############################################# Regresión cuantilica#########################################
      
      #if (!input$ignoregroupqr) {
        if (!input$ignorecolqr) {
          if (input$Tauvalue) {
            if(!input$hidedynamic){
              p <- p +  stat_quantile(method = "rqss",quantiles =input$Tau,size=1.5,
                                      linetype="solid", 
                                      formula=y ~ qss(x))   
            }
            
            if (input$mitad)
              p <- p +  stat_quantile(method = "rqss",quantiles = 0.5,size=1.5,
                                      linetype="solid",
                                      formula=y ~ qss(x))
            if (input$noventa)
              p <- p +  stat_quantile(method = "rqss",quantiles = 0.90,size=1,
                                      linetype="dashed",
                                      formula=y ~ qss(x))
            if (input$diez)
              p <- p +  stat_quantile(method = "rqss",quantiles = 0.1,size=1,
                                      linetype="dashed",
                                      formula=y ~ qss(x))
            
            if (input$superior)
              p <- p +  stat_quantile(method = "rqss",quantiles = 0.95,size=1,
                                      linetype="dashed",
                                      formula=y ~ qss(x))
            
            if (input$inferior) 
              p <- p +  stat_quantile(method = "rqss",quantiles = 0.05,size=1,
                                      linetype="dashed",
                                      formula=y ~ qss(x))
          }
        }
        if (input$ignorecolqr) {
          colqr <- input$colqr
          if (input$Tauvalue) {
            if(!input$hidedynamic){
              p <- p +  stat_quantile(method = "rqss",quantiles =input$Tau,size=1.5,
                                      linetype="solid", col=colqr,
                                      formula=y ~ qss(x))
            }
            if (input$mitad)
              p <- p +  stat_quantile(method = "rqss",quantiles = 0.5,size=1.5,
                                      linetype="solid", col=colqr,
                                      formula=y ~ qss(x))
            if (input$noventa)
              p <- p +  stat_quantile(method = "rqss",quantiles = 0.90,size=1,
                                      linetype="dashed", col=colqr,
                                      formula=y ~ qss(x))
            if (input$diez)
              p <- p +  stat_quantile(method = "rqss",quantiles = 0.1,size=1,
                                      linetype="dashed", col=colqr,
                                      formula=y ~ qss(x))
            
            if (input$superior)
              p <- p +  stat_quantile(method = "rqss",quantiles = 0.95,size=1,
                                      linetype="dashed", col=colqr,
                                      formula=y ~ qss(x))
            
            if (input$inferior) 
              p <- p +  stat_quantile(method = "rqss",quantiles = 0.05,size=1,
                                      linetype="dashed", col=colqr,
                                      formula=y ~ qss(x))
          }
        }

############################################################################################################
##################################### Líneas interactivas ##################################################
############################################################################################################      
  
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
 
## imprime gráfica   
  output$plot <- renderPlot({
    plotObject()
  })
  
  
##############################################################################################################  
########################################### Clicks interactivos###############################################
##############################################################################################################
  
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
  
################################################################################################################
################################################## Gráficas #####################################################
################################################################################################################  
  
# Histograma de la variable x
  
  histograma <- reactive({
    
    if(is.null(subsetdata2())) return()
    tabla <- data.frame(table(subsetdata2()[,input$x]))
    ggplot(data=tabla, aes(x=tabla[,1], y=tabla[,2], fill=factor(tabla[,1]))) +
      geom_bar(stat="identity", position=position_dodge())+
      labs(x = input$x, y = 'Frecuencia', title = 'Histograma') +
      geom_text(aes(label=Freq), vjust=-0.3, size=3.5) +
      theme_bw() +
      theme(legend.position="none", plot.title = element_text(family = "Trebuchet MS", color="Black",
                                                              face="bold", size=15))
    
  })
  
  
  output$histplot <- renderPlot({
    histograma()
  })
  

  # Boxplot de variable x 
  
  diagrama_caja <- reactive({
    
    if(is.null(subsetdata2())) return()
    
    tabla1 <- data.frame(subsetdata2()[,c(input$x)])
    boxplot(tabla1, las = 2, col = 'steelblue')
    
  })
  
  output$boxplot <- renderPlot({
    diagrama_caja()
  })
  
  
    
  
  
#################################################################################################################
######################################### Descarga de la gráfica ################################################
#################################################################################################################
  
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
  
  
  # Obtener dimensiones de la imagen
  downloadPlotHeight <- reactive({
    input$downloadPlotHeight
  })
  
  downloadPlotWidth <- reactive({
    input$downloadPlotWidth
  })
  
  # Nombre del archivo
  downloadPlotFileName <- reactive({
    input$downloadPlotFileName
  })
  
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