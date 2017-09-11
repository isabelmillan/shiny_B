library(shiny)
library(ggplot2)
library(scales)
library(DT)
library(quantreg)
library(Hmisc)
library(quantreg)
source("utiles.R")
options(shiny.maxRequestSize = 6000*1024^2) # 6000 MB/ 6 GB


#stat_sum_df <- function(fun, geom="point", ...) {
#  stat_summary(fun.data=fun,  geom=geom,  ...)
#}
#stat_sum_single <- function(fun, geom="point", ...) {
#  stat_summary(fun.y=fun,  geom=geom,  ...)
#}




shinyUI(fluidPage(
  titlePanel("Gráficas"),
  sidebarLayout(
    
    sidebarPanel(
      tabsetPanel(
        tabPanel("Inputs", 
                 #tagList(
                 # singleton(tags$head(tags$script(src='//cdn.datatables.net/1.10.7/js/jquery.dataTables.min.js',
                 #                                 type='text/javascript'))),
                 # singleton(tags$head(tags$script(src='//cdn.datatables.net/tabletools/2.2.4/js/dataTables.tableTools.min.js',
                 #                                 type='text/javascript'))),
                 #singleton(tags$head(tags$script(src='//cdn.datatables.net/colreorder/1.1.3/js/dataTables.colReorder.min.js',type='text/javascript'))),
                 #singleton(tags$head(tags$script(src='colvis.js',type='text/javascript'))),
                 #singleton(tags$head(tags$script(src='//cdn.datatables.net/tabletools/2.2.4/js/ZeroClipboard.min.js',type='text/javascript'))),
                 #singleton(tags$head(tags$link(href='//cdn.datatables.net/tabletools/2.2.4/css/dataTables.tableTools.css',rel='stylesheet',type='text/css'))),
                 #singleton(tags$script(HTML("if (window.innerHeight < 400) alert('Screen too small');"))),
                 #tags$head( tags$style(HTML("
                 #                            .cvclear{
                 #                            text-align:right}"))) 
                 #),
                 
                 
                 fileInput("datafile", "Escoge un archivo csv", multiple = FALSE, accept = c("csv")),
                 
                 uiOutput("ycol"),
                 uiOutput("slider2"),
                 uiOutput("xcol"),
                 uiOutput("slider"),
                 # uiOutput("maxlevels"),
                 
                 tabPanel("Filtros", 
                          #h4("Filtros"),
                          uiOutput("filtervar1"),
                          uiOutput("filtervar1values"),
                          uiOutput("filtervar2"),
                          uiOutput("filtervar2values")
                 )
                 
                 #hr()
        ), # tabpanel
        
        tabPanel("Ajustes de la gráfica",
                 #checkboxInput('logy', 'Log Y axis', value = FALSE) ,
                 #checkboxInput('logx', 'Log X axis', value = FALSE) ,
                 textInput('ylab', 'Nombre del eje Y', value = "") ,
                 textInput('xlab', 'Nombre del eje X', value = "") ,
                 
                 
                 
                 selectInput('backgroundcol', label ='Color del fondo',
                             choices=c("Gris" ="gray97","Blanco"="white","Gris Oscuro"="grey90"),
                             multiple=FALSE, selectize=TRUE,selected="gray97"),
                 h4("Líneas de referencia"),
                 checkboxInput('identityline', 'Línea idenidad')    ,   
                 checkboxInput('horizontalzero', 'Línea horizontal en cero'),
                 checkboxInput('customline1', 'Línea vertical interactiva'),
                 conditionalPanel(
                   condition = "input.customline1" , 
                   numericInput("vline",label = "",value = 1) )
                 ,
                 checkboxInput('customline2', 'Línea horizontal interactiva'),
                 conditionalPanel(
                   condition = "input.customline2" , 
                   numericInput("hline",label = "",value = 1) )
        )
      )
    ), #sidebarPanel
    mainPanel(
      tabsetPanel(
        tabPanel("Gráfica"  , 
                 plotOutput('plot',  width = "100%" ,click = "plot_click",
                            hover = hoverOpts(id = "plot_hover", delayType = "throttle"),
                            brush = brushOpts(id = "plot_brush")),
                 
                 #verbatimTextOutput("plotinfo"),
                 hr(),
                 uiOutput("clickheader"),
                 tableOutput("plot_clickedpoints"),
                 uiOutput("brushheader"),
                 tableOutput("plot_brushedpoints"),
                 
                 
                 #actionButton("plotButton", "Update Plot"),
                 
                 uiOutput("optionsmenu") ,
                 
                 conditionalPanel(
                   condition = "input.showplottypes" , 
                   
                   fluidRow(
                     
                     column (12, hr()),
                     column (4,
                             radioButtons("Points", "Puntos/Jitter:",
                                          c("Puntos" = "Points",
                                            "Jitter" = "Jitter",
                                            "Ninguno" = "None")),
                             conditionalPanel( " input.Points== 'Points' ",
                                               sliderInput("pointstransparency", "Transparencia de puntos", min=0, max=1, value=c(0.5),step=0.01))
                     ),
                     column(4,
                            conditionalPanel( " input.Points== 'Points' ",
                                              sliderInput("pointsizes", "Tamaño del punto", min=0, max=4, value=c(1),step=0.1),
                                              numericInput('pointtypes','Tipo de punto',16, min = 1, max = 25),
                                              selectInput('colorpunto', 'Color', choices=colors(),multiple=FALSE, selectize=TRUE,selected="black")
                            )),
                     
                     column (4,
                             
                             radioButtons("line", "Líneas",
                                          c("Lineas" = "Lines",
                                            "Ninguno" = "None"),selected="None"),
                             conditionalPanel( " input.line== 'Lines' ",
                                               sliderInput("linestransparency", "Transparencia de líneas", min=0, max=1, value=c(0.5),step=0.01)
                                               
                                               
                             )
                             
                     ),
                     column(4, conditionalPanel( " input.line== 'Lines' ",
                                                 sliderInput("linesize", "Tamaño de línea", min=0, max=4, value=c(1),step=0.1),
                                                 selectInput('linetypes','Tipo de línea',c("solid","dotted"))
                                                 
                     ) )
                     
                   )#fluidrow
                 ) ,
                 conditionalPanel(
                   condition = "input.showfacets" , 
                   fluidRow(
                     column (12, hr()),
                     column (4, uiOutput("colour"),uiOutput("group")),
                     column(4, uiOutput("facet_col"),uiOutput("facet_row")),
                     column (4, uiOutput("pointsize"),uiOutput("fill"))
                   )
                 ),
                 
                 
                 
                 
                 #rqss quantile regression
                 conditionalPanel(
                   condition = "input.showrqss" , 
                   
                   fluidRow(
                     column(12,hr()),
                     column(4,
                            checkboxInput('Tauvalue', 'Cuantiles dinámicos', value = FALSE),
                            h5("Preset Quantiles"),
                            checkboxInput('superior', '95%'),
                            checkboxInput('noventa', '90%'),
                            checkboxInput('mitad', '50%', value = FALSE),
                            checkboxInput('diez', '10%'),
                            checkboxInput('inferior', '5%')
                     ),
                     column(4,
                            sliderInput("Tau", label = "Valor cuantil dinámico",
                                        min = 0, max = 1, value = 0.5, step = 0.01) 
                            #sliderInput("Penalty", label = "Sensibilidad spline",
                            #            min = 0, max = 10, value = 1, step = 0.1)  ,
                            #selectInput("Constraints", label = "Spline constraints:",
                            #            choices = c("N","I","D","V","C","VI","VD","CI","CD"), selected = "N")
                     ),
                     column(4,
                            checkboxInput('ignorecolqr', 'Ignorar color'),
                          #  checkboxInput('ignoregroupqr', 'Ignorar grupo',value = TRUE),
                            checkboxInput('hidedynamic', 'Esconder cuantil dinamico'),
                            selectInput('colqr', label ='Color', choices=colors(),multiple=FALSE, selectize=TRUE,selected="black")
                     )
                     
                   )#fluidrow
                 )
        ),#tabPanel1
        tabPanel("Descarga", 
                 selectInput(
                   inputId = "downloadPlotType",
                   label   = h5("Selecciona el tipo de archivo"),
                   choices = list("PDF"  = "pdf","BMP"  = "bmp","JPEG" = "jpeg","PNG"  = "png")),
                 
                 # Dimensiones de la imagen
                 h5(HTML("Selecciona las dimensiones de la imagen<br>(unidades son pulgadas para PDF, pixeles para otrosformatos)")),
                 numericInput(
                   inputId = "downloadPlotHeight",label = "Altura (pulgadas)",value = 7,min = 1,max = 100),
                 numericInput(
                   inputId = "downloadPlotWidth",label = "Ancho (pulgadas)",value = 7,min = 1,max = 100),
                 # Nombre del archivo
                 textInput(
                   inputId = "downloadPlotFileName",
                   label = h5("Escribe el nombre del archivo")),
                 
                 # Descargar con botón
                 downloadButton(
                   outputId = "downloadPlot", 
                   label    = "Descarga gráfica")
                 
        )#tabPanel2
      )#tabsetPanel
    )#mainPanel
  )#sidebarLayout
)#fluidPage
)