############################################################
#### TESIS DA [Reunion 59]: script con funciones utiles ####
############################################################

### Función 4 "pape"
#-- Que hace: devuelve probabilidades a posteriori x clase y etiquetas (binario)
#-- Entra:
#-- Sale:
#-- Otros: modificada 30/04/17 para calcular >2 alfas a priori x defecto

#' @export
pape <- function(alfas=NULL,f_dk,yr,y_nombr="yr",datfalt=NULL,nomb.obs=NULL,...){
  stopifnot("list" %in% class(f_dk)) #necesario para loop
  fnum <- f_dk$f_num
  fcat <- f_dk$f_cat
  fmix <- f_dk$f_mix
  tdens <- f_dk$dens
  cantalfas <- if(is.null(alfas)) nlevels(yr) else length(alfas)
  clases <- seq(1:cantalfas)
  if(!is.null(datfalt))
    yr <- yr[-datfalt]
  minfil <- length(yr) #min(nrow(fiX$f1k_dfe$eval)
  yr_etiq <- levels(yr)
  if(is.null(alfas)){
    alfas_ap <- round(table(yr)/length(yr),digits=3)
    alfas <- alfas_ap
  }
  vs <- pap <- data.frame(matrix(nrow=minfil,ncol=cantalfas),row.names=nomb.obs)
  for(j in clases){
    assign(paste0("alfa",j,collapse=""),as.numeric(paste0(get("alfas")[j],collapse=","))) #OK
    #Modificar ejecucion segun clase de objeto de entrada
    #ej
    if(!any((class(f_dk) %in% "cdb_dk_f"))){
      if(tdens %in% c("density","ks"))
        assign(paste0("vs",j,collapse=""),get(paste0("alfa",j,collapse=""))*get("fnum_Xte_prod",get(paste0("fy",j,"_Xte",collapse=""),fnum)))
      if(tdens=="np")
        assign(paste0("vs",j,collapse=""),get(paste0("alfa",j,collapse=""))*get("dens",get(paste0("fy",j,"_Xte",collapse=""),fnum)))
    } else if(any(class(f_dk) %in% "cdb_dk_f")){
      #sacar ADECUADAMENTE valores "dens", luego multiplicar alfas*dens
      assign(paste0("vs",j,collapse=""),get(paste0("alfa",j,collapse=""))*get("pf_ctcl",get(paste0("fmix_",j,collapse=""),fmix)))
    } else {
      stop("No sirve lo que me tas pasando munianio!")
    }
    vs <- cbind(vs,get(paste("vs",j,collapse="",sep="")))
  }
  vs <- vs[,-(1:cantalfas)]
  names(vs) <- paste("vs",clases,sep="")
  for(i in 1:minfil){
    pap[i,] <- vs[i,]/rowSums(vs)[i]
  }
  names(pap) <- yr_etiq #paste(y_nombr,"_",yr_etiq,sep="")
  datprobl <- which(is.na(pap[,1])) #si es uno, son todos
  if(length(datprobl)!=0) pap[datprobl,] <- rep(0,cantalfas)
  pap$ypred <- factor(colnames(pap)[apply(pap,1,which.max)]) #si empate designa al 1ro
  res <- list("f_dk"=f_dk,"PaP"=pap,"alfas"=alfas,"YObs"=yr,"datprobl"=datprobl,
    dens=tdens)
  class(res) <- c("cdb_pape",class(res)) #agregado para despachar metodos
  return(res)
}

############################################################

#**metodos print, plot**
"print.cdb_pape" <- function(x,...)
  stop(paste("No implementamos metodo print para la clase",sQuote(x=cdb_pape)))

"plot.cdb_pape" <- function(x,datT,graf=FALSE,grr=FALSE,...){
  stopifnot("cdb_pape" %in% class(x)) #necesario para loop
  f_dk <- x$f_dk
  pap <- x$PaP
  alf <- x$alfas
  yobs <- x$YObs
  ypred <- x$PaP$ypred
  dens <- x$dens
  yneto <- ifelse(yobs==ypred,"BienPred","MalPred")
  lamp <- which(yneto=="MalPred")
  if(graf){
    plot.new()
    if(grr & graf){
      par(mfrow=c(1,2))
    } else {
      par(mfrow=c(1,1))
    }
    if(dens %in% c("density","ks")){
      plot(datT,pap[,1],ylim=c(0,1),col="blue",xlab="x",lty=2,
        ylab="Probabilidad a posteriori",
        main="Grafico P(j|x), j=1,2",...)
      points(datT,pap[,2],col="red")
      if(length(lamp)>0)
        text(datT[lamp],rep(.5,length(datT[lamp])),paste0("Mal\n",lamp,"\n",
          round(datT[lamp],2)),col="orange",cex=0.7)
      abline(h=0.5,col="green")
      legend("top",legend=c("P(j=1|x)","P(j=2|x)"),lty=c(2,1),
        col=c("blue","red"),bty="n",cex=0.7)
    }
  }
  if(grr){
    plot(x,f1,col="red")
    points(x,f2,col="blue")
    #points(df1[,1],df1[,6],pch=c(1,8),col=c("green","orange",cex=1.2)) #as.numeric(as.character(df1$BienCl))
    # -- falta filtrar x,df1$BienCl x "bien","mal" clasif pensarlo biennn!!!
    text(x,rep(0,length(x)),labels=df1$BienCl[1],col="green",cex=0.7)
    text(x,rep(0,length(x)),labels=df1$BienCl[2],col="orange",cex=0.7)
  }
  # else {
  #   stop(paste("No implementamos metodo plot para la clase",sQuote(x=cdb_pape)))
  # }
}
