options(shiny.maxRequestSize = 1000*1024^2)
library(cluster)
library(fpc)
library(rpart)
library(factoextra)
#source("utiles.R")

shinyServer(function(input, output) {
  
  
  # Importar archivo
  dInput = reactive({
    in.file = input$file1
    
    if (is.null(in.file))
      return(NULL)
    
    data <- read.csv(in.file$datapath, na.strings=c(".", "NA", "", "?"))
    
  })
  
  # Renderear la tabla y pasarla a UI.R
  output$view = renderTable({
    d.input = dInput()
    if (is.null(d.input)) return(NULL)
    if (ncol(d.input>10)) d.input = d.input[] 
    head(dInput(), n=50)  
  })
  
  # Check boxes
  output$choose_columns <- renderUI({
    d.input = dInput()
    if(is.null(d.input))
      return()
    
    # Con nombres adecuados
    #dat <- get(input$file1)
    colnames <- names(d.input)
    
    # Crear checkboxes y seleccionarlas todas por default
    checkboxGroupInput("columns", "Choose columns", 
                       choices  = colnames,
                       selected = colnames)
  })
  
  # Target 
  output$choose_target <- renderUI({
    d.input = dInput()
    if(is.null(d.input))
      return()
    
    colnames <- names(d.input)
    
 
    selectInput("target", "Choose Target Attribute", 
                choices  = colnames)#,
    # selected = colnames)
  })
  ##########  #k means cluster
  
  selectedData <- reactive({
    d.input = dInput()
    dat_cluster <- d.input
    dat_cluster <- dat_cluster[, input$columns, drop = FALSE]
    for (x in 1:length(dat_cluster))      # Transformar  variables
    { 
      if(!is.numeric(dat_cluster[,x])) {
        column <- paste(colnames(dat_cluster)[x],"transformed",sep="_")
        temp_data <- as.numeric(dat_cluster[,x])
        temp_mat <- matrix(c(temp_data),ncol=1,byrow=TRUE)
        colnames(temp_mat) <- column
        data_clust_trans <- cbind(dat_cluster,temp_mat)
      }
      
    }
    nums <- sapply(dat_cluster, is.numeric)  
    dat_cluster_numeric <- dat_cluster[,nums]  #Considerar solo numericas
    dat_cluster_numeric[is.na(dat_cluster_numeric)]=FALSE
    
    dat_cluster_numeric
    
  })	
  clusters <- reactive({
    
    kmeans(selectedData(), input$clusters)
  })
  
  output$kmeans_plot <- renderPlot({
    
    clusplot(selectedData(), clusters()$cluster, color=clusters()$cluster, labels=clusters()$cluster, cex=1.0, shade=TRUE,lines=0)
  })
  
  output$clust_table <- renderTable({
    clust_table <- cbind(selectedData(), by=list(cluster=clusters()$cluster))
    clust_table
  })
  
  output$agg_table <- renderTable({
    agg_table <-  aggregate(selectedData(), by=list(cluster=clusters()$cluster),mean)
    agg_table
  })
  
})