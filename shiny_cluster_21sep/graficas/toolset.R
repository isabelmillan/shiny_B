gggentipo <- function(var1, var2, colnames.class){
    # var1 = "a1"; var2 = 'po4'
    tipo <- c(colnames.class[var1], colnames.class[var2])
    tipocol <- tipo %>%
        sort() %>% paste0(collapse = "")
}

gggenerator <- function(var1, var2, dataf, colnames.class){
    tipo <- c(colnames.class[var1], colnames.class[var2])
    tipocol <- tipo %>%
        sort() %>% paste0(collapse = "")

    if( tipocol %in% c("factorfactor") ){
        datfacfac <- dataf[, c(var1, var2)]
        datfacfac[,"var2"] <- datfacfac[, var2]
        gg <- ggplot(datfacfac, aes_string(x = var1, fill = var2)) +
            geom_bar(position = 'stack', alpha = .5) +
            facet_wrap(~var2) +
            theme(axis.text.x = element_text(angle = 90, hjust = 1))
    }

    if( tipocol %in% c("factornumeric", "factorinteger") ){
        varcat <- names(tipo[tipo == "factor"])
        varnum <- names(tipo[tipo %in% c("numeric", "integer")])
        gg <- ggplot(dataf, aes_string(x = varcat, y = varnum,
                                       fill = varcat)) +
            geom_boxplot(alpha = .5) +
            geom_rug(color = 'gray30') +
            theme(legend.position = 'none',
                  axis.text.x = element_text(angle = 90, hjust = 1))
    }

    if( tipocol %in% c("numericnumeric", "integerinteger") ){
        datacors <- dataf[, c(var1, var2)] %>% na.omit()
        corcols <- cor(x = as.vector(datacors[, var1]),
                       y = as.vector(datacors[, var2])) %>%
            round(3)
        gg <- ggplot(dataf, aes_string(x = var1, y = var2)) +
            geom_point(color = 'gray20', alpha = .4) +
            geom_rug(color = 'gray30') +
            geom_smooth(method = 'lm', se= F, color = 'red', alpha = .7) +
            ggtitle(paste("Correlación:", corcols)) +
            theme(plot.title = element_text(hjust=1, size=10))
    }
    return(gg)
}


graphs_dataset <- function(dataf, list.vars = "all", print = F){
    list.vars <- unlist(list.vars)

    if( length(list.vars) == 1 ){
        if(list.vars == 'all'){
            combs.vars <- combn(names(dataf), 2)
        }
    }
    if( length(list.vars) > 1 ){
        combs.vars <- combn(list.vars, 2)
    }


    colnames.class <- sapply(dataf, class)
    gglist <- t(combs.vars) %>%
        tbl_df() %>%
        group_by(V1,V2) %>%
        do(restipo = gggentipo(.$V1, .$V2, colnames.class),
           resgg = gggenerator(.$V1, .$V2, dataf, colnames.class)) %>%
        ungroup() %>%
        mutate(tipocomb = unlist(.$restipo)) %>%
        dplyr::select(-restipo)

    if(print){
        a1 <- do.call(gridExtra::grid.arrange,
                      filter(gglist, tipocomb == 'factorfactor')$resgg)

        a2 <- do.call(gridExtra::grid.arrange,
                      filter(gglist, tipocomb %in% c('factornumeric', 'factorinteger') )$resgg)

        a3 <- do.call(gridExtra::grid.arrange,
                      filter(gglist, tipocomb %in% c("numericnumeric", "integerinteger"))$resgg)
        # pdf("algas/03-eda_graphs.pdf", width=25,height=30)
        # gridExtra::grid.arrange(a1,a2,a3)
        # dev.off()
        # warning("Se creó un pdf con las gráficas en la carpeta algas")
    }

    return(gglist)
}



nasreport <- function(dataf){
    # dataf = algas_data
    porccol <- apply(is.na(dataf), 1, sum)/ncol(dataf)
    porcrow <- apply(is.na(dataf), 2, sum)/nrow(dataf)
    compporc <- sum(complete.cases(dataf))/nrow(dataf)

    if(compporc!=1){
        colsmasfalta <- paste(sort(porcrow[porcrow>0], decreasing = T) %>%
                                  names(), collapse = ", ")
        titulo <- paste(
            "Resúmen:\n",
            paste0(round(100*sum(complete.cases(dataf))/nrow(dataf)),"%"),
            "observaciones completas",
            "\n",
            sum(porccol> 0.02),
            "Observaciones con más de 2% faltante\n",
            "Columnas de mayor a menor faltantes:","\n",
            colsmasfalta,
            "\n","\n",
            "Matriz de correlación de faltantes:")

        # Matriz de correlación
        cor.mat <- cor(is.na(dataf)[,porcrow>0] )
        tab.corr <- cor.mat %>%
            tbl_df() %>%
            mutate(rows = colnames(cor.mat)) %>%
            gather(cols, corr, -rows)
        gg <- ggplot(tab.corr, aes(x = rows, y = cols, fill = corr)) +
            geom_tile(alpha =.7) +
            geom_text(aes(label = round(corr, 2), color = corr)) +
            scale_fill_continuous(low = 'white', high = "#132B43") +
            scale_color_continuous(high = 'white', low = "#132B43") +
            xlab(NULL) +
            ylab(NULL) +
            ggtitle(titulo) +
            theme(legend.position = 'none',
                  plot.title = element_text(hjust=0, size=9),
                  axis.text.x = element_text(angle = 90, hjust = 1)) +
            coord_fixed(ratio = .65)
        return(gg)
    }
    else{
        warning("No hay faltantes en la base")
    }
}

imputacion_cent <- function(colu){
    if(class(colu) %in% c("factor", "character")){
        colu <- as.character(colu)
        colu[is.na(colu)] <- names(sort(table(colu), decreasing = T)[1])
        colu <- factor(colu)
    }

    if(class(colu) %in% c("numeric", "integer", "double")){
        colu[is.na(colu)] <- median(colu, na.rm = T)
    }
    return(colu)
}

imputar_valor_central <- function(dataf, colnames) {
    # dataf = algas_data
    limnas <- apply(is.na(dataf[, colnames] ), 1, sum)/length(colnames) < 0.02
    dataimp <- dataf[, colnames] %>%
        mutate(selecobs = limnas) %>%
        filter(selecobs) %>%
        mutate_each(funs(imputacion_cent)) %>%
        dplyr::select(-selecobs)

    warning( paste("Se descartan",
                   sum(!limnas),
                   "observaciones con mas de 2% de faltantes\n") )
    return(dataimp)
}

imputar_valor_lm <- function(var_independiente, modelo) {
    coefs.vec <- coef(modelo)
    pred <- coefs.vec[1] + coefs.vec[2]*var_independiente
    pred
    }
