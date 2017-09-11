library(shiny)
library(ggplot2)
library(xtable)
library(plotly)

shinyUI(pageWithSidebar(
  
  # Header:
  headerPanel("Data explorer"),
  
  # Input in sidepanel:
  sidebarPanel(
    
    # Upload data:
    fileInput("file", "Upload any CSV format file:"),
    htmlOutput("Y"),
    htmlOutput("X"),
    htmlOutput("group"),
    htmlOutput("group2")
    
  ),
  
  # Main:
  mainPanel(
    navbarPage("",
               tabPanel("Data", dataTableOutput("table")),
               navbarMenu("Uni",
                          tabPanel("Histogram",sliderInput("breaks","Breaks:",min=5,max=30,value=10),plotlyOutput("hist")),
                          tabPanel("Boxplot with mean shown in red", plotlyOutput("box2"))),
               navbarMenu("Regression",
                          tabPanel("Linear plot",plotlyOutput("lplot")),
                          tabPanel("Quadratic plot", plotlyOutput("qplot")),
                          tabPanel("Regression summary",htmlOutput("sumtable"),htmlOutput("sumtext"),verbatimTextOutput("sum")), 
                          tabPanel("Regression anova", htmlOutput("antable"),verbatimTextOutput("anova")),
                          #tabPanel("Description", htmlOutput("txt3")),
                          tabPanel("Regression Diagnostics", plotOutput("dplot")),
                          tabPanel("Quadratic summary", verbatimTextOutput("quadsum")),
                          tabPanel("Quadratic anova",verbatimTextOutput("quadanova"))),
               navbarMenu("Non parametric",
                          tabPanel("Spline plot", plotlyOutput("splot")),           
                          tabPanel("Non parametric (rank) plot", plotlyOutput("rplot")),
                          tabPanel("Spearman's rank correlation",verbatimTextOutput("cor"))),
               navbarMenu("GLM",
                          tabPanel("Poisson plot for counts", plotlyOutput("poisplot")), 
                          tabPanel("Negbin plot for counts", plotlyOutput("nbplot")),
                          tabPanel("Binomial plot for ones and zeros", plotlyOutput("binplot")),
                          tabPanel("Poisson summary", verbatimTextOutput("poissum")),
                          tabPanel("Negbin summary", verbatimTextOutput("nbsum")),
                          tabPanel("Poisson anova",verbatimTextOutput("poisanova")),
                          tabPanel("Negbin anova",verbatimTextOutput("nbanova"))),
               navbarMenu("ANOVA",
                          tabPanel("Boxplot", plotlyOutput("boxplot")), 
                          tabPanel("Confidence intervals", plotlyOutput("ciplot")),
                          tabPanel("ANOVA", verbatimTextOutput("ANOVA")),
                          tabPanel("Tukey pairwise", verbatimTextOutput("tukey"),plotOutput("tukeyplot"))),
               navbarMenu("Grouped",
                          tabPanel("Linear plots", plotlyOutput("lplot2")), 
                          tabPanel("Spline plots", plotlyOutput("splot2")),
                          tabPanel("Box plots", plotlyOutput("boxplot2")),
                          tabPanel("Confidence intervals", plotlyOutput("ciplot2")))
               
               
    )
  )
))