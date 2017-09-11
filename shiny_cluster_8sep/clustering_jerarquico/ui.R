##### Clustering jerárquico

library(shinythemes)

shinyUI(fluidPage(theme = shinytheme("cerulean"),
  headerPanel(""),
  sidebarPanel(
    checkboxInput(inputId = "update",
                  label = strong("Actualizar después de cargar (Requerido siempre)"),
                  value = FALSE),
    selectInput('method', 'Método de correlación',
                c("pearson", "kendall", "spearman"), selected="pearson"),
    selectInput('distMethod', 'Método de distancia para Clustering',
                c("euclidean", "maximum", "manhattan", "canberra",
                  "binary", "minkowski"), selected="manhattan"),
    selectInput('clustMethod', 'Método de aglomeración for Clustering',
                c("complete", "ward", "single", "average",
                  "mcquitty", "median", "centroid"), selected="complete"),
    tags$hr(),
    checkboxInput(inputId = "rem",
                  label = strong("Escoge variables a excluir"),
                  value = FALSE),
    uiOutput("condPanel1"),
    ## conditionalPanel(
    ##   condition = "input.rem == true",
    ##   selectizeInput('toRm', "Exclude", choices=sort(rownames(datGlobal)), multiple=TRUE)
    ## ),
    checkboxInput(inputId = "incl",
                  label = strong("Escoge variables a incluir"),
                  value = FALSE),
    uiOutput("condPanel2")
    ## conditionalPanel(
    ##   condition = "input.incl == true",
    ##   selectizeInput('toIncl', "Include Only", choices=sort(rownames(datGlobal)), multiple=TRUE)
    ## )
  ),
  mainPanel(
    tabsetPanel(type = "tabs",
                tabPanel("Plot", plotOutput('plot1', height = "500px", width = "500px")),
                tabPanel("Summary", verbatimTextOutput("summary") )
      )
    )
  )
)
