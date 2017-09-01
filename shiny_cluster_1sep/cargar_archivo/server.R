##### K-means

options(shiny.maxRequestSize = 1000*1024^2)
library(cluster)
library(fpc)
library(rpart)
library(factoextra)
library(shinythemes)
library(psych)
library(data.table)
library(crosstalk)
library(leaflet)
library(DT)
library(reshape)
library(reshape2)
library(plyr)
source("utiles.R")


shinyServer(function(input, output, session) {
  
  # Función que importa el dataset
  dInput1 = reactive({
    in.file = input$file1
    
    if (is.null(in.file))
      return(NULL)
    
    data <- read.csv(in.file$datapath, na.strings=c(".", "NA", "", "?"))
    
    datGlobal <<- data
    return(data)
  })
  
  
  # Filtro inicial de las variables: todas las que tienen COD, Fechas, 
  
  dInput3<- reactive({
    filtrar(dInput1())
  })
    
  
  # Check boxes
  output$choose_columns <- renderUI({
    # Si falta el imput, regresa error después
    d.input = dInput3()
    if(is.null(d.input))
      return()
    
    # Obtener datos con nombres adecuados
    #dat <- get(input$file1)
    colnames <- names(d.input)
    
    # Crear checkboxes y seleccionarlas por default
    checkboxGroupInput("columns", "Escoge las columnas", 
                       choices  = colnames,
                       selected = colnames)
  })
  
  # Función que renderea el archivo
  output$view = renderTable({
    d.input = dInput3()
    if (is.null(d.input)) return(NULL)
    if (ncol(d.input > 10)) d.input = d.input[] 
    head(dInput3(), n=50)  
  }) 
  
  # Muestra random
  output$muestra_random<- renderUI({
    cuantos<- nrow(dInput3())
    textInput('muestra', 'Selecciona el número de contratos aleatorios de la base', 
              cuantos)
  })
  
  dInput2<- reactive({
    filas<- nrow(dInput3())
    aleatorio<- dInput3()[sample(filas, input$muestra), ]
  })

  
  
  ###########    Filtros
  
  
  
  # Filtros que pone el usuario
  

  output$choose_familia <- renderUI({
    choices<- as.vector(unique(dInput2()$CMCO_COD_FAMILIA))
    selectInput ('fam', 'Escoge la familia', choices, selected="", 
                 multiple = TRUE)
  })  

  output$choose_herramienta <- renderUI({
    choices<- as.vector(unique(dInput2()$CMCO_COD_HERRAM_SCO))
    selectInput ('herr', 'Escoge la herramienta', choices , selected="", 
                 multiple = TRUE)
  })  
  
  #output$choose_stage <- renderUI({
  #  choices<- as.vector(unique(dInput2()$CMCO_IND_STAGE_FINAL))
  #  selectInput ('stage', 'Escoge el stage', choices, selected="", 
  #               multiple = TRUE)
  #})  
  
  output$choose_codpd <- renderUI({
    choices<- as.vector(unique(dInput2()$CMCO_COD_CLAVE_PD))
    selectInput ('codpd', 'Escoge la clave de pd', choices, selected="", 
                 multiple = TRUE)
  }) 
  
  output$choose_codlgd <- renderUI({
    choices<- as.vector(unique(dInput2()$CMCO_COD_CLAVE_LGD))
    selectInput ('codlgd', 'Escoge la clave de lgd', choices, selected="", 
                 multiple = TRUE)
  }) 
  
  output$choose_mora <- renderUI({
    choices<- as.vector(unique(dInput2()$CMCO_NUM_CUOTIMP))
    selectInput ('mora', 'Escoge la mora', choices, selected="", 
                 multiple = TRUE)
  }) 
  
  
  filtro_fam <- reactive({
    if (is.null(input$fam)) {
      return(dInput2())}
    subset(dInput2(), CMCO_COD_FAMILIA %in% input$fam)
  })
  
  filtro_herr <- reactive({
    if (is.null(input$herr)) {
      return(dInput2())}
    subset(dInput2(), CMCO_COD_HERRAM_SCO %in% input$herr)
  })
  
 # filtro_stage <- reactive({
#    if (is.null(input$stage)) {
#      return(dInput2())}
#    subset(dInput2(), CMCO_IND_STAGE_FINAL %in% input$stage)
    
 # })
  
  filtro_codpd <- reactive({
    if (is.null(input$codpd)) {
      return(dInput2())}
    subset(dInput2(), CMCO_COD_CLAVE_PD %in% input$codpd)
    
  })
  
  filtro_codlgd <- reactive({
    if (is.null(input$codlgd)) {
      return(dInput2())}
    subset(dInput2(), CMCO_COD_CLAVE_LGD %in% input$codlgd)
    
  })
  
  filtro_mora <- reactive({
    if (is.null(input$mora)) {
      return(dInput2())}
    subset(dInput2(), CMCO_NUM_CUOTIMP %in% input$mora)
    
  })
  
  #Juntamos todos los filtros
    dInput <- reactive({
      finales_1<- merge(filtro_fam(),filtro_herr()) 
      finales_2<- merge(finales_1, filtro_codpd())
      finales_3<- merge(finales_2, filtro_codlgd())
      finales<- merge(finales_3, filtro_mora())
      #finales<- merge(finales_4, filtro_stage())
      finales<- unique(finales)
      finales<- as.data.frame(finales)
  })
  
  
  
 #############################k-means cluster
  
  selectedData <- reactive({
    d.input = dInput()
    dat_cluster <- d.input
    dat_cluster <- dat_cluster[, input$columns, drop = FALSE]
    dat_cluster<- dat_cluster[sapply(dat_cluster, function(x) length(unique(x))) > 1] #quitar constantes
    for (x in 1:length(dat_cluster))      # Transformar variables
    { 
      # Para las variables que no son numéricas
      if(!is.numeric(dat_cluster[,x])) {
        column <- paste(colnames(dat_cluster)[x],"transformed",sep="_")
        temp_data <- as.numeric(dat_cluster[,x])
        temp_mat <- matrix(c(temp_data),ncol=1,byrow=TRUE)
        colnames(temp_mat) <- column
        data_clust_trans <- cbind(dat_cluster,temp_mat)
      }
      
    }
    nums <- sapply(dat_cluster, is.numeric)  
    dat_cluster_numeric <- dat_cluster[,nums]  #Consideramos solo variables numericas para K-Means
    dat_cluster_numeric[is.na(dat_cluster_numeric)]=FALSE #quitamos NA's
    dat_cluster_numeric_norm<- normalize_df(dat_cluster_numeric) #Normalizamos
    
    dat_cluster_numeric_norm
    
  })	
  

  clusters <- reactive({
    
    kmeans(selectedData(), input$clusters)
  })
  
  output$kmeans_plot <- renderPlot({
    
    #clusplot(selectedData(), clusters()$cluster, color=clusters()$cluster, labels=clusters()$cluster, cex=1.0, shade=TRUE,lines=0)
    fviz_cluster(clusters(), selectedData())
  })
  
  #dat_cluster_numeric$cluster<- reactive({clusters()$cluster
    
  #})
  
  output$clust_table <- renderTable({
    clust_table <- cbind(selectedData(), by=list(cluster=clusters()$cluster))
    clust_table
  })
  
  output$agg_table <- renderTable({
    agg_table <- aggregate(selectedData(), by=list(cluster=clusters()$cluster),mean)
    #agg_table_t <- t(agg_table[,2:ncol(agg_table)])
    #colnames(agg_table_t) <- agg_table[,1] 
    agg_table
  })

  
  #### para variables no normalizadas y agregar el cluster al que pertenecen
  
  selectedData2 <- reactive({
    d.input = dInput()
    dat_cluster <- d.input
    dat_cluster <- dat_cluster[, input$columns, drop = FALSE]
    for (x in 1:length(dat_cluster))      # Transformar las variables
    { 
      # Para las variables que no son numéricas
      if(!is.numeric(dat_cluster[,x])) {
        column <- paste(colnames(dat_cluster)[x],"transformed",sep="_")
        temp_data <- as.numeric(dat_cluster[,x])
        temp_mat <- matrix(c(temp_data),ncol=1,byrow=TRUE)
        colnames(temp_mat) <- column
        data_clust_trans <- cbind(dat_cluster,temp_mat)
      }
      
    }
    nums <- sapply(dat_cluster, is.numeric)  
    dat_cluster_numeric <- dat_cluster[,nums]  
    dat_cluster_numeric[is.na(dat_cluster_numeric)]=FALSE #quitamos NA's
    
    dat_cluster_numeric
    
  })
    #agregamos el cluster al que pertenecen
  specifiedCluster <- reactive({
    tabla_cluster <- cbind(selectedData2(), by=list(cluster=clusters()$cluster))
    describeBy(tabla_cluster, tabla_cluster$cluster)
    #tabla_cluster
  })

 
  
  output$tabla_cluster <- renderPrint({
    specifiedCluster()
  })   
 
################################# Importancia de variables  
  
  specifiedCluster2 <- reactive({
    tabla_cluster <- cbind(selectedData2(), by=list(cluster=clusters()$cluster))
    tabla_cluster
  })
  
  
  subset_cluster<- reactive({ for (i in unique(specifiedCluster2()$cluster)){
    num_clus<- i
    nam <- paste("cluster", i, sep = "_")
    cluster_i<- assign(nam, specifiedCluster2()[specifiedCluster2()$cluster==i,])
    
    for (cluster in num_clus){
      constantes<- cluster_i[sapply(cluster_i, function(x) length(unique(x))) == 1]
      #constantes<- cbind(cluster_i, constantes)
      #print(summary(constantes))
      #for(colu in constantes){
      #  val<- constantes$colu
      #  valor<- unique(constantes$val)
      #}
      valor<- sapply(constantes, function(x) unique(x))
      importantes<- cbind(cluster, valor)
      colnames(importantes)<- c('cluster', 'valor')
      print(importantes)
    }
    
  }
  })
  

  output$variables_imp <- renderPrint({
    subset_cluster()
  })
  
})
