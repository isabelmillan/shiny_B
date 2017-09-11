library(shiny)
library(ggplot2)
library(xtable)
library(plotly)
library(MASS)
library(Hmisc)

#load("marinverts.rob")
#wd <- getwd()
#setwd("..")
#datadir <- getwd()
#datadir<-paste(datadir,"/data/",sep="")
#setwd(wd)
#datadir

shinyServer(function(input, output) {
  
  
  # load data -------------------------------------------------------------------
  
  Dataload <- reactive({
    if (is.null(input$file)) {
      # User has not uploaded a file yet
      return(NULL)
    }
    Dataset <- as.data.frame(read.csv(input$file$datapath))
    #out<-paste("data/",input$file$name,sep="")
    #write.csv(Dataset,file=out,row.names = FALSE)
    return(Dataset)
  })
  
  Dataset<-reactive(Dataload())
  
  # Show data table ---------------------------------------------------------  
  output$table <- renderDataTable(Dataset())  
  # setup variables -----------------------------------------------------------------------
  
  varnames<-reactive({
    d<-Dataset()
    names(d)[sapply(d,is.numeric)]
  })
  
  factornames<-reactive({
    d<-Dataset()
    names(d)[sapply(d,is.factor)]
  })
  
  
  output$X <- renderUI({ 
    selectInput("X", "Independent numerical variable (X axis)", varnames())
  })
  output$Y <- renderUI({ 
    selectInput("Y", "Dependent numerical variable (Y axis)", varnames(),selected=varnames()[2])
  })
  output$group <- renderUI({ 
    selectInput("group", "Grouping variable (factor)", factornames(),selected=factornames()[1])
  })
  
  output$group2 <- renderUI({ 
    selectInput("group2", "Second grouping variable (for conditional boxplots)", factornames(),selected=factornames()[2])
  })
  
  # initiate plots ----------------------------------------------------------
  ggx<-reactive({
    x<-input$Y
    d<-Dataset()
    tx<-sprintf("g0<-ggplot(data=d,aes(x=%s))",x)
    eval(parse(text=tx))
    return(g0)
  })
  
  ggy<-reactive({
    x<-input$Y
    d<-Dataset()
    tx<-sprintf("g0<-ggplot(data=d,aes(y=%s,x=''))",x)
    eval(parse(text=tx))
    return(g0)
  })
  
  ggstart<-reactive({
    x<-input$X
    y<-input$Y
    d<-Dataset()
    tx<-sprintf("g0<-ggplot(data=d,aes(x=%s,y=%s))",x,y)
    eval(parse(text=tx))
    return(g0)
  })
  
  ggbstart<-reactive({
    x<-input$group
    y<-input$Y
    d<-Dataset()
    tx<-sprintf("g0<-ggplot(data=d,aes(x=%s,y=%s))",x,y)
    eval(parse(text=tx))
    return(g0)
  })
  
  ggcstart<-reactive({
    x<-input$X
    y<-input$Y
    group<-input$group
    d<-Dataset()
    tx<-sprintf("g0<-ggplot(data=d,aes(x=%s,y=%s)) + facet_wrap(~%s)",x,y,group)
    eval(parse(text=tx))
    return(g0)
  })
  
  ggcbstart<-reactive({
    x<-input$group
    y<-input$Y
    group<-input$group2
    d<-Dataset()
    tx<-sprintf("g0<-ggplot(data=d,aes(x=%s,y=%s)) + facet_wrap(~%s)",x,y,group)
    eval(parse(text=tx))
    return(g0)
  })
  
  
  # Form the plots --------------------------------------------------------------
  
  output$hist <- renderPlotly({
    g0<-ggx()
    g1<-g0+geom_histogram(bins=input$breaks,fill="grey",col="black") +theme_classic()
    ggplotly(g1)
  })
  
  output$box1 <- renderPlotly({
    g0<-ggy()
    g1<-g0+geom_boxplot() +theme_bw() + coord_flip()
    ggplotly(g1)
  })
  
  output$box2 <- renderPlotly({
    g0<-ggy()
    g0<-g0 + stat_summary(fun.y=mean,geom="point",col="red") 
    g1<-g0+geom_boxplot() +theme_bw() + coord_flip()
    ggplotly(g1)
  })
  
  # linearplots --------------------------------------------------------------
  
  
  output$lplot <- renderPlotly({
    g0<-ggstart()
    g1<-g0+geom_point() + geom_smooth(method=lm)+theme_bw()
    ggplotly(g1)
  })
  
  
  
  # quadratic plot ------------------------------------------------------------
  output$qplot <- renderPlotly({
    g0<-ggstart()
    g1<-g0+geom_point() + geom_smooth(method="lm",formula=y~x+I(x^2), se=TRUE) +theme_bw()
    ggplotly(g1)
  })
  # spline plot -------------------------------------------------------------
  output$splot <- renderPlotly({
    g0<-ggstart()
    g1<-g0+geom_point() + geom_smooth() +theme_bw()
    ggplotly(g1)
  })
  
  
  
  # glmplots -------------------------------------------------------------
  
  output$nbplot <- renderPlotly({
    g0<-ggstart()
    g1<-g0+geom_point() +geom_smooth(method="glm.nb", se=TRUE) +theme_bw()
    ggplotly(g1)
  })
  
  output$binplot <- renderPlotly({
    g0<-ggstart()
    g1<-g0+geom_point()+stat_smooth(method="glm",method.args=list(family="binomial"))
    g1<-g1 +theme_bw()
    ggplotly(g1)
  })
  
  
  
  output$poisplot <- renderPlotly({
    g0<-ggstart()
    g1<-g0+geom_point() +theme_bw()
    g1<-g1+ stat_smooth(method="glm",method.args=list( family="poisson"), se=TRUE)
    ggplotly(g1)
  })
  
  
  # boxplot -------------------------------------------------------------
  output$boxplot <- renderPlotly({
    g0<- ggbstart()
    g1<-g0+geom_boxplot() +theme_bw()
    ggplotly(g1)
  })
  # confidence interval plot -------------------------------------------------------------
  output$ciplot <- renderPlotly({
    g0<- ggbstart()
    g1<-g0+stat_summary(fun.data=mean_cl_normal,geom="errorbar")
    g1<-g1+theme_bw()
    g1<-g1+stat_summary(fun.y=mean,geom="point")
    ggplotly(g1)
  })
  # conditional plots --------------------------------------------------------------
  output$lplot2 <- renderPlotly({
    g0<-ggcstart()
    g1<-g0+geom_point() + geom_smooth(method=lm)+theme_bw()
    ggplotly(g1)
  })
  
  output$splot2 <- renderPlotly({
    g0<-ggcstart()
    g1<-g0+geom_point() + geom_smooth()+theme_bw()
    ggplotly(g1)
  })
  output$boxplot2 <- renderPlotly({
    g0<- ggcbstart()
    g1<-g0+geom_boxplot() +theme_bw()
    ggplotly(g1)
  })
  output$ciplot2 <- renderPlotly({
    g0<- ggcbstart()
    g1<-g0+stat_summary(fun.data=mean_cl_normal,geom="errorbar")
    g1<-g1+theme_bw()
    g1<-g1+stat_summary(fun.y=mean,geom="point")
    ggplotly(g1)
  })
  
  # Regression diagnostics  -------------------------------------------------------------
  output$dplot <- renderPlot({
    mod<-linearmodel()
    par(mfcol=c(2,2))
    plot(mod)
  })
  # rank regression ---------------------------------------------------------
  output$rplot <- renderPlotly({
    x<-input$X
    y<-input$Y
    d<-Dataset()
    tx<-sprintf("d$rank_%s<-rank(d$%s)",x,x)
    eval(parse(text=tx))
    tx<-sprintf("d$rank_%s<-rank(d$%s)",y,y)
    eval(parse(text=tx))
    
    tx<-sprintf("outp<-cor.test(d$%s,d$%s,method='spearman')",y,x)
    eval(parse(text=tx))
    output$cor <- renderPrint(outp)
    
    tx<-sprintf("g0<-ggplot(data=d,aes(x=rank_%s,y=rank_%s))",x,y)
    eval(parse(text=tx))
    
    g1<-g0+geom_point() +theme_bw() + geom_smooth(method=lm)
    ggplotly(g1)
  })
  
  
  
  # build models ------------------------------------------------------------
  
  # linear model ------------------------------------------------------------
  linearmodel<-reactive({
    x<-input$X
    y<-input$Y
    d<-Dataset()
    tx<-sprintf("mod<-lm(data=d,%s~%s)",y,x)
    eval(parse(text=tx))
    return(mod)
  })
  
  # aov model ---------------------------------------------------------------
  
  aovmodel<-reactive({
    x<-input$group
    y<-input$Y
    d<-Dataset()
    tx<-sprintf("mod<-aov(data=d,%s~%s)",y,x)
    eval(parse(text=tx))
    return(mod)
  })
  
  # quadratic model ---------------------------------------------------------
  quadmodel<-reactive({
    x<-input$X
    y<-input$Y
    d<-Dataset()
    tx<-sprintf("mod<-lm(data=d,%s~%s+I(%s^2))",y,x,x)
    eval(parse(text=tx))
    return(mod)
  })
  
  # poisson model -----------------------------------------------------------------
  poissonmodel<-reactive({
    x<-input$X
    y<-input$Y
    d<-Dataset()
    tx<-sprintf("mod<-glm(data=d,%s~%s,family=poisson)",y,x)
    eval(parse(text=tx))
    return(mod)
  })
  
  # neg bin model-----------------------------------------------------------------
  nbmodel<-reactive({
    x<-input$X
    y<-input$Y
    d<-Dataset()
    tx<-sprintf("mod<-glm.nb(data=d,%s~%s)",y,x)
    eval(parse(text=tx))
    return(mod)
  })
  
  
  
  
  # Rendering R output directly ----------------------------------------------------------------- 
  output$anova<-renderPrint(anova(linearmodel()))
  
  output$ANOVA<-renderPrint(summary(aovmodel()))
  
  output$tukey<-renderPrint(TukeyHSD(aovmodel()))
  
  output$tukeyplot<-renderPlot(plot(TukeyHSD(aovmodel())))
  
  output$quadsum<-renderPrint(
    {
      mod<-quadmodel()
      summary(mod)
    })
  output$quadanova<-renderPrint(
    {
      mod<-quadmodel()
      anova(mod)
    })
  
  output$nbsum<-renderPrint(
    {
      mod<-nbmodel()
      summary(mod)
    })
  output$nbanova<-renderPrint(
    {
      mod<-nbmodel()
      anova(mod)
    })
  
  output$poissum<-renderPrint(
    {
      mod<-poissonmodel()
      summary(mod)
    })
  output$poisanova<-renderPrint(
    {
      mod<-poissonmodel()
      anova(mod)
    })
  
  # R output as HTML  -----------------------------------------------------------------
  
  output$sumtable <- renderText({
    mod<-linearmodel()
    a<-summary(mod)
    output$sum<-renderPrint(a)
    print(xtable(a),type="html")
    
  } )
  
  output$antable <- renderText({
    mod<-linearmodel()
    a<-anova(mod)
    print(xtable(a),type="html")
    
  } )
  
  output$sumtext <- renderText({
    x<-input$X
    y<-input$Y
    mod<-linearmodel()
    a<-summary(mod)
    int<-round(coef(mod)[1],4)
    slope<-round(coef(mod)[2],4)
    rsq<-round(a$r.squared,3)
    sprintf("The regression line is %s = %s + %s %s with an r squared value of %s",y,int,slope,x,rsq)
    
  } )
  
  output$load_file <- renderUI({ 
    selectInput("load_file", "Load saved file", dir(path="data/"))
  })
  
  
})