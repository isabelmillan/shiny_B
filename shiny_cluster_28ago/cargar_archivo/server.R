options(shiny.maxRequestSize = 1000*1024^2)
library(cluster)
library(fpc)
library(rpart)
library(factoextra)
library(shinythemes)
library(psych)
library(data.table)
source("utiles.R")

shinyServer(function(input, output) {
  
  # Función que importa el dataset
  dInput = reactive({
    in.file = input$file1
    
    if (is.null(in.file))
      return(NULL)
    
    data <- read.csv(in.file$datapath, na.strings=c(".", "NA", "", "?"))
    
    datGlobal <<- data
    return(data)
  })
  
  
  # Función que renderea el archivo
  output$view = renderTable({
    d.input = dInput()
    if (is.null(d.input)) return(NULL)
    if (ncol(d.input>10)) d.input = d.input[] 
    head(dInput(), n=50)  
  })
  
  # Check boxes
  output$choose_columns <- renderUI({
    # Si falta el imput, regresa error después
    d.input = dInput()
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
  
  # Target, seleccion de variables
  output$choose_target <- renderUI({
    # Si falta el imput, regresa error después
    d.input = dInput()
    if(is.null(d.input))
      return()
    
    # Obtener datos con nombres adecuados
    
    colnames <- names(d.input)
    
    # Crear checkboxes y seleccionarlas por default
    selectInput("target", "Choose Target Attribute", 
                choices  = colnames)#,
    # selected = colnames)
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
    dat_cluster_numeric <- dat_cluster[,nums]  #Consideramos solo numéricas K-Means
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
      importantes<- cbind(cluster, colnames(constantes))
      colnames(importantes)<- c('cluster', 'variables constantes')
      print(importantes)
    }
    
  }
  })
  

  output$variables_imp <- renderPrint({
    subset_cluster()
  })
  
})
