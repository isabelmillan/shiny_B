#La que sirve pa tener valores entre 0 y 1 es range
normalize_df = function(x, method = "range", range = c(0, 1), margin = 1L, on.constant = "quiet") {
  isnum = sapply(x, is.numeric)
  if (any(isnum))
    x[, isnum] = as.data.frame(lapply(x[, isnum, drop = FALSE], normalize2, method = method,
                                      range = range, on.constant = on.constant))
  return(x)
}

normalize2 = function(x, method, range, on.constant) {
  # si es constante
  if (length(unique(x[!is.na(x)])) == 1L) {
    switch(on.constant,
           warn = warning("Constant vector in normalization."),
           stop = stop("Constant vector in normalization."))
    switch(method,
           center = scale(x, center = TRUE, scale = FALSE),
           range = ifelse(is.na(x), NA, mean(range)),
           standardize = scale(x, center = TRUE, scale = FALSE),
           scale = x
    )
  } else {
    switch(method,
           range = (x - min(x, na.rm = TRUE)) / diff(range(x, na.rm = TRUE)) * diff(range) + range[1L],
           standardize = scale(x, center = TRUE, scale = TRUE),
           center = scale(x, center = TRUE, scale = FALSE),
           scale = scale(x, center = FALSE, scale = sd(x, na.rm = TRUE))
    )
  }
}


## Funcion que hace un primer filtro de las variables
filtrar<- function(x){
  x<- x[, grep("^CMCO", colnames(x))]  # solo variables ifrs9
  x<- x[, -grep("CLI", colnames(x))]                   # solo por contrato (quitamos por cliente)
  x
}


### Quitar constantes y NA's en general
#df<- df[sapply(df, function(x) length(unique(x))) > 1]
#df<- df[complete.cases(df),]

### graficar todas las variables

#for(i in 1:(ncol(fam1)-1)) {plot(fam1$CMCO_FEC_APERTURA, fam1[,i+1], ylab=colnames(fam2)[i+1], xlab="Fecha_apertura",pch=20,col='blue',main=paste('Plot',i))}


