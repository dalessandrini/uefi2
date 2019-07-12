############################################################
#### TESIS DA [Reunion 65]: script con funciones utiles ####
############################################################

### Función 1 "gedn" OK -- INTERNA
#-- Que hace: genero datos normales separados x clase
#-- Entra: n,prop,mus,sigmas,(nombre_clases)
#-- Sale: df [Datos,Clase]

#' Titulo
#'
#' (parrafo1: va debajo del titulo, que hace en 1 oracion)
#'
#' (parrafos2-: van en Details, explayarse)
#'
#' @keywords internal
gedn <- function(n,prop,mu1,mu2,sigma1,sigma2,cl1="C1",cl2="C2",resu=FALSE){
  df <- data.frame("Datos"=mezcla(n,prop,mu1,sigma1,mu2,sigma2),
    "Clase"=c(rep(cl1,n*prop),rep(cl2,n-n*prop)))
  if(resu)
    list(df=df,resu=summary(df))
  else
    df
}

# Prueba:
#a1 <- gedn(n=2000,prop=0.5,mu1=0,mu2=10,sigma1=1,sigma2=1,cl1="C1",cl2="C2",resu=FALSE)

############################################################

### Función 2 "mute" OK -- INTERNA
#-- Que hace: separa {MEnt,MTest}
#-- Entra:
#-- Sale:
#-- Otros: controlar errores (salta c/mensajes R; personalizarlos)

#' Titulo
#'
#' (parrafo1: va debajo del titulo, que hace en 1 oracion)
#'
#' (parrafos2-: van en Details, explayarse)
#'
#' @keywords internal
mute <- function(df,nroperm=1,pme){
  if(pme>=1 | pme<=0)
    cat("Perdoname:", pme,"debe ser un valor entre 0 y 1")
  if(class(nroperm)!="numeric" | nroperm <= 0)
    cat("Fijate bien:", nroperm,"debe ser un valor NATURAL!!!")
  dne <- dnt <- indE <- indT <- rnME <- rnMT <- vector("list",nroperm)
  for(j in 1:nroperm){
    me <- sort(sample(1:nrow(df),round((nrow(df))*pme)))
    dne[[j]] <- df[me,]
    dnt[[j]] <- df[-me,]
    indE[[j]] <- me
	indT[[j]] <- setdiff(1:nrow(df),me)
	rnME[[j]] <- row.names(dne[[j]])
	rnMT[[j]] <- row.names(dnt[[j]])
  }
  dn <- list("dat_me"=dne,"dat_mt"=dnt,"indME"=indE,"indMT"=indT,rnME=rnME,rnMT=rnMT)
  dn
}

# Prueba:
# b1 <- mute(df=a1,nroperm=1,pme=.8); str(b1) #revisar xq da lo que da!!
# mute(df=a1,nroperm=-2,pme=.75)
# mute(df=a1,nroperm=3,pme=1.3)

############################################################

### Función 3 "mezcla" -- INTERNA
#-- Que hace: genera mezcla 2 normales, devuelve 1 solo vector
#-- Usada en: generar f_X(x)=af_{X_1}+(1-a)f_{X_2}, F~Normal
#-- Otros: cedida por Mathias Bourel
#(implementada solo para k=2 densidades mezcla; se puede generalizar con procedimiento analogo)

#' Titulo
#'
#' (parrafo1: va debajo del titulo, que hace en 1 oracion)
#'
#' (parrafos2-: van en Details, explayarse)
#'
#' @keywords internal
mezcla <- function(n,prop=0.5,m1=-1,s1=1,m2=1,s2=1,semilla=123){
  set.seed(semilla)
  x1 <- rnorm(n*prop,m1,s1) #a*f_{X_1} TODO JUNTO relacionando f(x2)
  x2 <- rnorm(n-n*prop,m2,s2) #(1-a)*f_{X_2} TODO JUNTO relacionando f(x1)
  c(x1,x2) #f_X(x)=c(af_{X_1},(1-a)f_{X_2}) #juntamos todo en un mismo vector
} #con esto se genera el VECTOR x

############################################################

### Función 4 "nif" -- INTERNA
#-- Que hace: auxiliar para generar 'f__k' variables (no fijos como hasta 14/04/17)
#-- Entra: factor
#-- Sale: #niveles, tipo(Nom,Ord), niveles, frecuencias (agreg 23/06/17)

#' Titulo
#'
#' (parrafo1: va debajo del titulo, que hace en 1 oracion)
#'
#' (parrafos2-: van en Details, explayarse)
#'
#' @keywords internal
nif <- function(factor){
  cl <- class(factor)
  stopifnot(cl=="factor")
  niv <- levels(factor)
  cniv <- length(niv)
  ord <- is.ordered(factor)
  tipoF <- if(ord) "Ordinal" else "Nominal"
  fa <- table(factor)
  fr <- round(table(factor)/sum(table(factor)),digits=3)
  tdfreq <- data.frame(cbind(fa,fr),row.names=niv)
  obsxn <- vector(mode="list",length=cniv)
  for(i in 1:cniv){
    obsxn[[i]] <- which(factor==levels(factor)[i])
    names(obsxn)[i] <- paste("niv_",i,"_",levels(factor)[i],sep="")
  }
  res <- list(niv=niv,cniv=cniv,tipoF=tipoF,orig=factor,obsxn=obsxn,tdfreq=tdfreq)
  class(res) <- c("cdb_nif",class(res))
  return(res)
}

### Función 4a "predict.nif" Predictor "histograma" categorico: version univariada
#-- Que hace: metodo predict para nif -- INTERNA
#-- Entra: 1 solo factor, newdata=dim(1) #escalar ejs newdata="R", "MontPubl"
#-- Sale: valores predict() para escalar (agreg 02/07/17)

#' Titulo
#'
#' (parrafo1: va debajo del titulo, que hace en 1 oracion)
#'
#' (parrafos2-: van en Details, explayarse)
#'
#' @keywords internal
predict.nif <- function(object,newdata,...){
  clo <- class(object)
  clom <- match(x=clo,table="cdb_nif")
  if(any(clom>0,na.rm=TRUE)==FALSE)
    stop(cat("Error: el objeto", object,"no es de clase 'cdb_nif' sino de clase", class(object)))
  lev <- object$niv
  nlev <- object$cniv
  df_fr <- object$tdfreq #verificar que concuerde con tdfreq actual
  a <- list()
  for(i in 1:nlev){
    a[[i]] <- ifelse(newdata %in% row.names(df_fr)[i],df_fr[i,2],NA)
  }
  a1 <- a[which(!is.na(unlist(a)))]
  if(!is.null(a1) & is.list(a1)) return(unlist(a1)) else return(NA)
}
# Pruebas:
#prpr <- uefi2:::nif(factor=X3) #OK
#predict.nif(object=prpr,newdata="M") #"R"

### Función 4b "predict.nif" Predictor "histograma" categorico: version multivariada
#-- Que hace: predice "histograma" para 1 variable categorica entera (vector [nx1])
#-- Entra: objeto clase nif, variable categorica entera (vector [nx1]) -- INTERNA
#-- Sale: prediccion para variable categorica en base a objeto 'nif' (agreg 02/07/17)

#' Titulo
#'
#' (parrafo1: va debajo del titulo, que hace en 1 oracion)
#'
#' (parrafos2-: van en Details, explayarse)
#'
#' @keywords internal
predict.nifM <- function(object,newdataM,...){
  a <- lapply(newdataM, function(x){
    predict.nif(object=object,newdata=x)
  })
  return(unlist(a))
}
# Prueba
#ooo <- sample(x=prpr$niv,size=500,replace=TRUE)
#predict.nifM(object=prpr,newdataM=ooo)

############################################################

### Función 5 "predict.hist.x" Predictor "histograma" numérico: version univariada
#-- Que hace: recibe objetos de clase {hist,density} (version 21/07/17)
#-- Entra: hh=histograma/densidad, x1 es UN SOLO punto -- INTERNA
#-- Sale: prediccion para un solo punto (agreg 02/07/17, en base a codigo gentileza MBo)

#' Titulo
#'
#' (parrafo1: va debajo del titulo, que hace en 1 oracion)
#'
#' (parrafos2-: van en Details, explayarse)
#'
#' @keywords internal
predict.hist.x <- function(hh,x1){
  if(class(hh)=="histogram"){
    breaks <- hh$breaks
    intens <- hh$density
  } else if(class(hh)=="density"){
    breaks <- hh$x
    intens <- hh$y
  }
  lims <- breaks[length(breaks)] #no sabes max, si min=breaks[1]
  if(x1<breaks[1] || x1>lims)
    res<- 0 #dens=0 si <min o >max
  else {
    pos1 <- which(x1<breaks)[1] - 1
    pos2 <- which(x1<=breaks)[1] - 1
    pos <- max(pos1,pos2,na.rm=TRUE) #asegura q pos1<=pos2
    res <- intens[pos] #devuelve hh$density "adecuado"
  }
  structure(list(x=x1,dens=res),class="cdb_dens")
}
#predict.hist.x(hist(precip),x1=c(2,4))

### Función 5a "predict.hist" Predictor "histograma" numérico: version multivariada
#-- Que hace: -- INTERNA
#-- Entra: hh=histograma/densidad, x=grilla de puntos (vector numerico)
#-- Sale: prediccion para una variable numerica (vector [nx1]) (agreg 02/07/17, en base a codigo gentileza MBo)

#' Titulo
#'
#' (parrafo1: va debajo del titulo, que hace en 1 oracion)
#'
#' (parrafos2-: van en Details, explayarse)
#'
#' @keywords internal
predict.hist <- function(hh,x){
  xl <- length(x)
  densm <- xx <- vector(length=xl)
  for(i in 1:xl){
    xx[i] <- predict.hist.x(hh,x[i])$x
    densm[i] <- predict.hist.x(hh,x[i])$dens
  }
  structure(list(x=xx,dens=densm),class="cdb_dens")
}

### Función 5b "plot.cdb_dens" Predictor "histograma" numérico: grafico
#-- Que hace: -- INTERNA
#-- Entra: objeto "cdb_dens"
#-- Sale: grafico de densidad estimada (para mejorar!)

#' Titulo -- DOCUMENTACION roxygen
#'
#' (parrafo1: va debajo del titulo, que hace en 1 oracion)
#'
#' (parrafos2-: van en Details, explayarse)
##'
#' #@export
#' @keywords internal
plot.cdb_dens <- function(x,main=NULL,xlab="x",ylab="Funcion de densidad",type="p",...){
  plot.default(x=x$x,y=x$dens,main=main,xlab=xlab,ylab=ylab,type=type,...)
  #agregarle grafico con ggplot2 que queda mas lindo que esta bazofia
}


### Función 6 "predict.hm" Predictor "histograma" mixto: version conjunta multivariada
#-- Que hace: -- INTERNA
#-- Entra:
#-- Sale:  (agreg 02/07/17)

#' Titulo
#'
#' (parrafo1: va debajo del titulo, que hace en 1 oracion)
#'
#' (parrafos2-: van en Details, explayarse)
#'
#' @keywords internal
predict.hm <- function(datE,datT,tp="hist",...){
  if(any(ncol(datE)!=ncol(datT)))
    stop(cat("El numero de columnas de datos de entrenamiento (",ncol(datE),") y
      de datos de prueba (",ncol(datT),") no coincide"))
  if(any(names(datE)!=names(datT)))
    stop(cat("Las variables de datos de entrenamiento y de prueba NO coinciden"))
  #[A]Separar cuaLi de cuanTi OK
  vdftE <- which(unlist(lapply(datE,class) %in% c("numeric","double","integer")))
  dfnumE <- data.frame(datE[,vdftE])
  names(dfnumE) <- names(datE)[vdftE]
  vdflE <- which(unlist(lapply(datE,class) %in% c("factor","character")))
  dfcatE <- data.frame(datE[,vdflE])
  names(dfcatE) <- names(datE)[vdflE]
  vdftT <- which(unlist(lapply(datT,class) %in% c("numeric","double","integer")))
  dfnumT <- data.frame(datT[,vdftT])
  names(dfnumT) <- names(datT)[vdftT]
  vdflT <- which(unlist(lapply(datT,class) %in% c("factor","character")))
  dfcatT <- data.frame(datT[,vdflT])
  names(dfcatT) <- names(datT)[vdflT]
  #[B]Aplicar hist.multiv x separado
  #-> a c/variable aplicarle "multiv" (lapply,apply o simil), juntar
  phn <- vector(mode="list",length=ncol(datT))
  phc <- vector(mode="list",length=ncol(datT))
  if(tp=="hist"){
    for(j in 1:ncol(dfnumT))
      phn[[j]] <- predict.hist(hist(dfnumE[,j],plot=FALSE),x=dfnumT[,j])
    } else if(tp=="dens"){
    for(j in 1:ncol(dfnumT))
      phn[[j]] <- predict.hist(hh=density(dfnumE[,j]),x=dfnumT[,j])
    }
  for(j in 1:ncol(dfcatT))
    phc[[j]] <- predict.nifM(object=uefi2:::nif(dfcatE[,j]),newdataM=dfcatT[,j])
  phndf <- data.frame(matrix(unlist(phn),ncol=ncol(dfnumT)))
  phcdf <- data.frame(matrix(unlist(phc),ncol=ncol(dfcatT)))
  #[C]Producto X FILA de [B]
  dfful <- cbind(phndf,phcdf)
  dfprd <- Reduce(f='*',x=dfful,accumulate=FALSE)
  return(list(dfprd=dfprd,dfnum=names(dfnumT),dfcat=names(dfcatT))) #mejorar SALIDA!!!
}
# Pruebas:
#debugonce(predict.hm)
#(ay <- predict.hm(datE=dfsE1,datT=dfsT1))

##############################################################
