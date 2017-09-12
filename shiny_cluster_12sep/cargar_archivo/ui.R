#### K means

library(shiny)
library(factoextra)
library(shinythemes)
options(shiny.maxRequestSize = 1000*1024^2)

#shinyUI(bootstrapPage(theme = shinytheme("cerulean"),
 #                     navbarPage("Análisis Clusters IFRS9",
shinyUI(pageWithSidebar(

#shinyUI(fluidPage(theme = shinytheme("cerulean"),
                 
  list(tags$head(tags$style("body {background-color: #F3F1F1; }")), HTML('<h2 style="color:#05B967"><center> Análisis de Clusters </center> </p>' )),
  #headerPanel("Predict Insurance Claim Risk "),

  # Left hand side panel
  sidebarPanel(
    #h2("Datos"),
    
    # Botón para importar datos
    fileInput('file1', 'Cargar archivo csv',
              accept=c('text/csv', 'text/comma-separated-values,text/plain')),
    conditionalPanel(condition="input.conditionedPanels==1",h5("Seleccionar las columnas que se utilizarán para el análisis")),
    conditionalPanel(condition="input.conditionedPanels==1",
                     tabPanel("Columns",uiOutput("choose_columns")))
                    # tabPanel("target",uiOutput("choose_target")))
    #wellPanel(img(src = "logo.png", height = 89, width = 200,align="center"))
    
  ),
  
  
  
  # Main panel (on the right hand side)
  mainPanel(
    tabsetPanel(
      tabPanel("Datos",
               h4("En caso de querer usar una muestra random de la base:"),
               uiOutput("muestra_random"),
               tableOutput("view"),
               value=1
      ),
      tabPanel("Filtros",
               h4("Selecciona los filtros necesarios"),
               uiOutput("choose_familia"),
               uiOutput("choose_herramienta"),
               #uiOutput("choose_stage"),
               uiOutput("choose_codpd"),
               uiOutput("choose_codlgd"),
               uiOutput("choose_mora")
               
      ),
      tabPanel("Clustering k-means",    
               numericInput('clusters', 'Número de clusters', 2, min = 1, max = 9),
               plotOutput('kmeans_plot'),
               h3("Medias/centros de los clusters"),
               tableOutput('agg_table'),
               value=1),
      tabPanel("Variables clave",
               h5("Las variables que tienen varianza cero, que son iguales para todos los elementos para cada cluster son"),
               verbatimTextOutput('variables_imp')),
      #tabPanel("Prediction Analysis",tableOutput("prediction"),value=1), 
      tabPanel("Variables por cluster",
               h3("Resumen estadísico de las variables por cluster"),
               verbatimTextOutput('tabla_cluster')),
      
      id = "conditionedPanels") 
                
    
    ))
)

