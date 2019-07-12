#############################################################
##### TESIS DA [Reunion 68]: funciones auxiliares a ASM #####
#############################################################

## Version 8: desde 23:59 19/05/2018 (sin 'etm', agregamos bestinfo/resblq, (au|fu)roc, ord_df)

### Funcion 1 "errp"
#-- Que hace: devuelve error promedio
#-- Entra: lista con MdConf (provenientes p.ej. de 'mdcmed')
#-- Sale: MdConf, err(prom) simple = suma(mdc)/length(list(mdc))
#-- Otros: mejor version S-A-C que pude desarrollar (mapply casi casi, pero no)

#' Resumen de datos de matriz de confusion
#'
#' Dada una lista de matrices de confusion, devuelve promedios de indicadores de interes
#'
#' Entrada: lista con matrices de confusion (provenientes p.ej. de 'mdcmed'); salida: promedios de indicadores de matrices de confusion (ver [mdcmed()]).
#'
#' @param ldf Lista formada por tablas que en realidad son matrices de confusion
#' @export
errp <- function(ldf){
  nuevmdc <- lapply(ldf,mdcmed)
  diml <- length(nuevmdc)
  nomb <- names(nuevmdc[[1]])
  a <- data.frame(matrix(cbind(unlist(nuevmdc)),nrow=diml,byrow=TRUE))
  names(a) <- nomb
  am <- round(colMeans(a,na.rm=TRUE),3)
  av <- round(rapply(a,sd,na.rm=TRUE,how="unlist"),2)
  list(erp=am,sderp=av,todos.erp=a)
}

#############################################################

### Funcion 2: "mdcmed" (matriz de confusion con medidas x modelo ajustado)
#-- Que hace: dada matriz de confusion (valores ordenados "0,1") devuelve medidas de prediccion
#-- Entra: objeto 'table'
#-- Salen: medidas de comparacion para matrices de confusion
#-- Otros: usada para ver performance de prediccion por modelo
#-- (obj: generalizar a MdConf con >2 categ, funcionalizar c/indicador!)

#' Genera medidas de resumen de matriz de confusion
#'
#' Dada matriz de confusion (valores ordenados "0,1") devuelve medidas de prediccion
#'
#' De un objeto 'table' (2 x 2) se extraen medidas de comparacion (ej: sensibilidad, especificidad) para matrices de confusion.
#'  Definiciones disponibles en documento de tesis "Aprendizaje Estadistico en Educacion:
#'  una propuesta de modelizacion para carreras de grado en Ingenieria", seccion 3.2.2
#'
#' @param tablamdc Lista formada por tablas que en realidad son matrices de confusion
#' @param dig Cantidad de digitos de redondeo
#' @export
mdcmed <- function(tablamdc,dig=3){#Parametro sin usar Fbeta=1
  ta <- tablamdc
  if(class(ta)!="table")
    stop(paste(ta,"no es una tabla hermano, que me estas dando!!?"))
  #Previo: cantidades utiles
  aa <- ta[1,1] # Fila 1: "tienen condicion"
  bb <- ta[1,2]
  cc <- ta[2,1] # Col 1: "expuestos"
  dd <- ta[2,2]
  ab <- aa + bb #Total x fila 1
  cd <- cc + dd #Total x fila 2
  ac <- aa + cc #Total x col 1
  bd <- bb + dd #Total x col 2
  vesp <- vespe(tablamdc,mostrar=TRUE)
  ## Indicadores extraidos de MdC:
  #Positivos:
  sen <- dd/sum(ta[2,]) #tpr=VP
  esp <- aa/sum(ta[1,]) #tnr=VN
  ppv <- dd/sum(ta[,2]) #
  npv <- aa/sum(ta[,1]) #
  acc <- sum(diag(ta))/sum(ta) # (VP+VN)/N "bien predichos" == AccO
  #Negativos:
  fpr <- bb/sum(ta[1,]) #ErrT.I=FP
  fdr <- bb/sum(ta[,2]) #
  fnr <- cc/sum(ta[2,]) #ErrT.II=FN
  fom <- cc/sum(ta[,1]) #
  #Generales
  erg <- 1-acc #error global 1-acc
  prv <- sum(ta[2,])/sum(ta) #prevalencia Cond.Posit/N
  #F_b <- ((1+Fbeta^2)*sen)/((Fbeta^2*sen)+fpr+fnr) #Fscore >1?! No ponerlo x interpret
  #kappa Cohen: kappa <- (AccO-AccE)/(1-AccE)
  AccE <- (vesp$`V.Esp`[1,1]+vesp$`V.Esp`[2,2])/vesp$Total #AccE
  kpc <- (acc-AccE)/(1-AccE)
  #orden segun tabla Wiki
  dfmed <- cbind(sen,esp,ppv,npv,acc,fpr,fdr,fnr,fom,erg,kpc,prv) #F_b
  dfmed <- apply(X=dfmed,MARGIN=2,FUN=round,digits=dig)
  return(dfmed)
}

#############################################################

### Funcion 3 "bestinfo" version v3 (07/05/18)
#-- Que hace: extrae "mejores promedios" de algun indicador seleccionado x bloque tiradas
#-- Entra: lista salida ASM, indices para loops, cuantilas necesarias...
#-- Sale: df con "mejores datos" segun indic, cuantiles, df con todos los datos,
#-- Otros: uso *exclusivo* para sacar info de listas ASM mas rapido; falta mejorar y generalizar
#-- (indicadores: todos los que surgen de una matriz de confusion)

#' Resumen de tiradas ASM por bloque (definidos en listas)
#'
#' Dadas salida ASM (en formato lista), indices para loops, cuantilas necesarias, esta funcion extrae "mejores promedios"
#' de algun indicador seleccionado x bloque tiradas
#'
#' Es una funcion util para extraer rapidamete informacion de listas ASM (falta mejorar y generalizar)
#'
#' @param lista Lista formada por tablas que en realidad son matrices de confusion
#' @param nloop Lista
#' @param nombcols_dfl Lista
#' @param qbest Lista
#' @param indic Lista
#' @param ... No implementado
bestinfo <- function(lista,nloop,nombcols_dfl,qbest,indic,...){
  nest <- length(nloop)
  mod <- c(nloop[[1]],nloop[[3]]) #mimp+moc
  mod1 <- c(1:9)[c("CART","Logit","SVM","RF","Boosting","CDB","vm","mp","waauc") %in% mod]
  dfl_nuev <- data.frame(matrix(ncol=12*3)) #nrow=20*6 NO asi no le tenemo que sacar 120 filas vacias!
  dfl_nuev_nombr <- vector()
  if(nest==3){
    for(k in nloop[[2]]){ #Se guarda: promedio, DEst, CV x estadistico MdConf
      for(j in mod1){ #1:CART, 2:Logit, 3:SVM, 4:RF, 5:Boost, 6:CDB, 7:MC_VM, 8:MC_MP, 9:MC_WA (sigue hasta el 13)
        assign(paste0("nuev_",j,"_",k,"_",collapse=""),
          list(cbind(t(get("lista")[[k]][[2]][[j]][[2]][[1]]),
            t(get("lista")[[k]][[2]][[j]][[2]][[2]]),
            round(t(get("lista")[[k]][[2]][[j]][[2]][[2]])/abs(t(get("lista")[[k]][[2]][[j]][[2]][[1]])),2))))
        assign("dfl_nuev", rbind(unlist(get(paste0("nuev_",j,"_",k,"_",collapse=""))),dfl_nuev))
        assign("dfl_nuev_nombr", c(paste0("nuev_",j,"_",k,"_",collapse=""),dfl_nuev_nombr))
      }
    }
  }
  if(nest>3){
    stop("Con paciencia y esmero... esperar a implementar esto!")
  }
  dfl_nuev <- dfl_nuev[which(!is.na(dfl_nuev[,indic])),]
  row.names(dfl_nuev) <- dfl_nuev_nombr #rev() NO necesario! (xq los juntas de atra p'alante!)
  colnames(dfl_nuev) <- c('sen','esp','ppv','npv','acc','fpr','fdr','fnr','fom','erg','kpc','prv',
    'sd.sen','sd.esp','sd.ppv','sd.npv','sd.acc','sd.fpr','sd.fdr','sd.fnr','sd.fom','sd.erg','sd.kpc','sd.prv',
    'cv.sen','cv.esp','cv.ppv','cv.npv','cv.acc','cv.fpr','cv.fdr','cv.fnr','cv.fom','cv.erg','cv.kpc','cv.prv')
  # Identificar salidas segun modelos ML: ver 1er subindice (cambio respecto a bestinfo() previa)
  rnb <- rownames(dfl_nuev)
  cart_bi <- dfl_nuev[grep("^nuev_1",rnb),]
  rownames(cart_bi) <- gsub("^nuev_1","CART_",rownames(cart_bi))
  logit_bi <- dfl_nuev[grep("^nuev_2",rnb),]
  rownames(logit_bi) <- gsub("^nuev_2","Logit_",rownames(logit_bi))
  svm_bi <- dfl_nuev[grep("^nuev_3",rnb),]
  rownames(svm_bi) <- gsub("^nuev_3","SVM_",rownames(svm_bi))
  rf_bi <- dfl_nuev[grep("^nuev_4",rnb),]
  rownames(rf_bi) <- gsub("^nuev_4","RF_",rownames(rf_bi))
  boost_bi <- dfl_nuev[grep("^nuev_5",rnb),]
  rownames(boost_bi) <- gsub("^nuev_5","Boosting_",rownames(boost_bi))
  cdb_bi <- dfl_nuev[grep("^nuev_6",rnb),]
  rownames(cdb_bi) <- gsub("^nuev_6","CDB_",rownames(cdb_bi))
  mc_vm_bi <- dfl_nuev[grep("^nuev_7",rnb),]
  rownames(mc_vm_bi) <- gsub("^nuev_7","mc_vm_",rownames(mc_vm_bi))
  mc_mp_bi <- dfl_nuev[grep("^nuev_8",rnb),]
  rownames(mc_mp_bi) <- gsub("^nuev_8","mc_mp_",rownames(mc_mp_bi))
  mc_wa_bi <- dfl_nuev[grep("^nuev_9",rnb),]
  rownames(mc_wa_bi) <- gsub("^nuev_9","mc_wa_",rownames(mc_wa_bi))
  # Mirando con lupa: 1) descript, 2) subconj
  Qs <- quantile(dfl_nuev[,indic],probs=qbest)
  dfl_nuev_reduc <- dfl_nuev[with(dfl_nuev,which(dfl_nuev[,indic]<Qs[1])),]
  list("dfBqlcompl"=dfl_nuev,"MejoresResultBlq"=dfl_nuev_reduc,"quant"=Qs,"indicador"=names(dfl_nuev)[indic],
    "cart"= if(nrow(cart_bi)!=0) cart_bi,
    "logit"=if(nrow(logit_bi)!=0) logit_bi,
    "svm"=if(nrow(svm_bi)!=0) svm_bi,
    "rf"=if(nrow(rf_bi)!=0) rf_bi,
    "boost"=if(nrow(boost_bi)!=0) boost_bi,
    "cdb"=if(nrow(cdb_bi)!=0) cdb_bi,
    "mc_vm"=if(nrow(mc_vm_bi)!=0) mc_vm_bi,
    "mc_mp"=if(nrow(mc_mp_bi)!=0) mc_mp_bi,
    "mc_wa"=if(nrow(mc_wa_bi)!=0) mc_wa_bi)
}

### Funcion 'ord_df_' v2 (06/05/18, cambiada por existir otra ord_df en fu_usoGral_vR78!!)

#' Funcion interna
#'
#' Ranking de formulas segun medidas de matriz de confusion especificas
#'
#' Dada lista de matrices de confusion, se ordenan resultados de ajuste de modelos segun valores obtenidos
#' en indicadores de matrices de confusion. Se separan en dos: positivos (se busca maximizarlos, ej: especificidad)
#'  y negativos (se busca minimizarlos, ej: falsos positivos, coeficientes de variacion de esas medidas (cv.xxx)).
#'  El ranking final es obtenido al ordenar las medianas resultantes de manera decreciente (mayor mediana, mejor formula).
#'
#' @param ldf Lista formada por tablas que en realidad son matrices de confusion
#' @param mdcmed mdcmed
#' @param modML modML
#' @param compl compl
#' @param ... No implementado
#' @keywords internal
ord_df_ <- function(df,mdcmed=c("sen","fpr","kpc"),modML="XXX",compl=FALSE,...){
  mdcmed.cv <- c(mdcmed,paste("cv",mdcmed,sep="."))
  df <- df[,unique(grep(paste(mdcmed.cv,collapse='|'),colnames(df),value=TRUE))]
  nof <- row.names(df)
  noc <- colnames(df)
  nf <- nrow(df)
  nc <- ncol(df)
  #MePos: medidas 'positivas', MeNeg: medidas 'negativas'
  #28/11/18: sacamos "sd.xxx" de meneg, agregamos 3er nivel de "medidas" (para los "cv.xxx")
  mepos <- c("sen","esp","ppv","npv","acc","kpc")
  mepos <- df[noc %in% mepos]
  ejordp <- data.frame(matrix(nrow=nrow(mepos),ncol=ncol(mepos)))
  meneg <- c("fpr","fdr","fnr","fom","erg")
  meneg <- df[noc %in% meneg]
  ejordn <- data.frame(matrix(nrow=nrow(meneg),ncol=ncol(meneg)))
  mexxx <- c("cv.sen","cv.esp", "cv.ppv","cv.npv","cv.acc","cv.fpr","cv.fdr","cv.fnr","cv.fom","cv.erg","cv.kpc")
  mexxx <- df[noc %in% mexxx]
  ejordx <- data.frame(matrix(nrow=nrow(mexxx),ncol=ncol(mexxx)))
  medi <- data.frame("mediana"=matrix(nrow=nrow(df),ncol=1),row.names=nof)
  #Ranking: menor valor, PEOR posicion (para ambas medidas; pone 1 al mas chico, 2 al siguiente...)
  if(ncol(mepos)>0){
    for(j in 1:ncol(mepos)){
      ejordp[,j] <- rank(mepos[,j],ties.method="average") #Orden OK!
    }
    colnames(ejordp) <- paste0(colnames(mepos),"_rkg")
  }
  if(ncol(meneg)>0){
    for(j in 1:ncol(meneg)){
      ejordn[,j] <- rank(-meneg[,j],ties.method="average") #Orden OK!
    }
    colnames(ejordn) <- paste0(colnames(meneg),"_rkg")
  }
  for(j in 1:ncol(mexxx)){
    ejordx[,j] <- rank(-mexxx[,j],ties.method="average") #mayor cv peor es la medida!
  }
  colnames(ejordx) <- paste0(colnames(mexxx),"_rkg")
  ejord <- cbind(ejordp,ejordn,ejordx)
  for(i in 1:nf){
    medi[i,] <- median(as.numeric(ejord[i,],row.names=nof))
  }
  emo <- cbind(df,ejord,medi)
  #Ranking final: ordeno por mediana DECRECIENTE (mayor mediana, mejor formula)
  rk <- emo[order(rank(emo$medi),decreasing=TRUE),ncol(emo),drop=FALSE] #OK
  colnames(rk) <- paste0("MdnDesc_",modML)
  rk$fxm <- rownames(rk) <- gsub(".*_([0-9]*)_","\\1",rownames(rk))
  names(rk)[2] <- paste0("Form",modML)
  rk$Orden <- seq(1:nrow(rk))
  if(compl)
    list(rk=rk,"SalidaCompl"=emo,"Advertencia"="A mayor valor de mediana, mas importancia en ranking")
  else rk[,2:3]
}

### 1.Nueva funcion: resblq() ##############################

#' Resultados por bloque
#'
#' Resume informacion por modelo y formula para generar ranking final de ordenamiento
#'
#' Se puede escoger cuantas formulas sirven para ordenar resultados
#'
#' @param obj Objeto
#' @param formulas Formulas
#' @param mxr Maximo de filas para armar el ranking (3 por defecto), debe ser >1
#' @param binf.qbest Cuantilas deseadas
#' @param binf.indic Sobre que indicador se requiere resumen (ver codigos en?)
#' @param ord.mdcmed Resultado de [ord_df()], es decir un ranking de?
#' @param vdm.formula Numero de formula (si objeto es una lista de formulas)
resblq <- function(obj,formulas,mxr=3,binf.qbest=c(0.05,0.1,0.15,0.2,0.25,0.5,0.75,0.95),binf.indic=10,
    ord.mdcmed,vdm.formula=NULL){
  mimp <- obj[[min(formulas)]]$Metadatos$mimp
  moc <- obj[[min(formulas)]]$Metadatos$moc
  #1:Extraigo mejor informacion por bloque de corridas ASM
  dfl <- bestinfo(lista=obj,nloop=list(mimp,formulas,moc),qbest=binf.qbest,indic=binf.indic)
  #2:ord_df_ por modelo ajustado
  if("CART" %in% mimp)
    rkg_binf_cart <- ord_df_(dfl$cart,ord.mdcmed,modML="CART")
  if("Logit" %in% mimp)
    rkg_binf_lgt <- ord_df_(dfl$logit,ord.mdcmed,modML="Logit")
  if("SVM" %in% mimp)
    rkg_binf_svm <- ord_df_(dfl$svm,ord.mdcmed,modML="SVM")
  if("RF" %in% mimp)
    rkg_binf_rf <- ord_df_(dfl$rf,ord.mdcmed,modML="RF")
  if("Boosting" %in% mimp)
    rkg_binf_boost <- ord_df_(dfl$boost,ord.mdcmed,modML="Boost")
  if("CDB" %in% mimp)
    rkg_binf_cdb <- ord_df_(dfl$cdb,ord.mdcmed,modML="CDB")
  if("vm" %in% moc)
    rkg_binf_mc_vm <- ord_df_(dfl$mc_vm,ord.mdcmed,modML="MC_VM")
  if("mp" %in% moc)
    rkg_binf_mc_mp <- ord_df_(dfl$mc_mp,ord.mdcmed,modML="MC_MP")
  if("waauc" %in% moc)
    rkg_binf_mc_wa <- ord_df_(dfl$mc_wa,ord.mdcmed,modML="MC_WA")
  #junto en lista, ordeno: RANKING x BLOQUE!
  #https://stackoverflow.com/questions/8016636/using-get-inside-lapply-inside-a-function
  li_rkg <- lapply(ls(pattern="rkg_binf_"),get,envir=sys.frame(sys.parent(0))) #lapply(ls(pattern="^rkg_binf_"),get) NO funca
  rk_gral <- Reduce(function(...) merge(...,by="Orden",all=TRUE),li_rkg)
  #Primeras formulas en salida: toma los 3 primeros, cuenta #formula en esas 3 filas xa todo modelo ML, luego frecuencia total en esas 3 filas
  fbl <- do.call("rbind",apply(rk_gral[1:mxr,-1],1,function(x) data.frame(table(x)))) #pa ver primeras formulas del bloque
  fbl2 <- aggregate(Freq~x,data=fbl,FUN=sum) #PENSAR EN ponderacion diferente x ranking (+pts si tas 1ro, menos si 2do etc)
  fbl2 <- fbl2[order(fbl2$Freq,decreasing=TRUE),]
  rownames(fbl2) <- NULL #pa poder ordenar correctamente la salida y las formulas!!!
  fbl2$x <- as.numeric(levels(fbl2$x))[fbl2$x]
  if(!is.null(vdm.formula)){
    tablita <- vector()
    cat("5 primeras formulas segun ranking:\n\n")
    for(i in 1:5)
      tablita[i] <- paste("Nro formula::",fbl2[i,1]," --- ",vdm.formula[fbl2[i,1]],"\n\n",sep="",collapse=",")
  }
  writeLines(formatOL(tablita,offset=0.3),sep='\n\n')
  list("rk_gral"=rk_gral,"Formulas_Blq"=fbl2[1:10,])
}

#############################################################

#' Funcion furoc
#'
#' Genera curva ROC
#'
#' Es un wrapper a las funciones [prediction()] y [performance()] del paquete ROCR
#'
#' @param pred Pred
#' @param patroro Patroro
#' @param med Tipo de medida en eje vertical (OY), opciones "auc" (para area
#'  debajo de curva ROC), "tpr" (sensibilidad o tasa de verdaderos positivos) o
#'  "sens" (para graficar Especificidad=TPR vs Sensibilidad=TNR)
#' @param tpr.adic Parametro adicional para med={tpr,sens}; puede ser "plot" (grafico ROC)
#'  o "data" (valores de umbrales para construir una curva ROC por arafue)
#' @param tpr.nmb Vector de caracteres adicional (todas las filas recibiran mismo nombre;
#'  pensado para juntar con otros data.frame similares)
#' @param ... Argumentos adicionales pasados a funciones [ROCR::prediction()] o
#' [ROCR::performance()]
# Ponemos codigo furoc() aca (no esta cargado en {uefi})
furoc <- function(pred,patroro,med=NULL,tpr.adic="data",tpr.nmb=NULL,...){
  loadNamespace("ROCR")
  predob <- ROCR::prediction(predictions=pred,labels=patroro)
  if(med=="auc"){
    perf <- ROCR::performance(prediction.obj=predob,measure=med)
    return(round(as.numeric(perf@y.values),4))
  }
  if(med=="tpr"){
    perf <- ROCR::performance(prediction.obj=predob,measure=med,x.measure="fpr")
    if(tpr.adic=="plot"){
      plot(x=unlist(perf@x.values),y=unlist(perf@y.values),type="l",xlab="1-Especificidad::FPR",ylab="Sensibilidad::TPR",...) #cambiado 23/11/18
    }
    if(tpr.adic=="data"){
      df <- data.frame(x=unlist(perf@x.values),y=unlist(perf@y.values)) #cambiado 24/11/18
      if(!is.null(tpr.nmb))
        df$nmb <- tpr.nmb
      return(df)
    }
  }
  if(med=="sens"){
    perf <- ROCR::performance(prediction.obj=predob,measure=med,x.measure="spec")
    if(tpr.adic=="plot"){
      plot(x=unlist(perf@x.values),y=unlist(perf@y.values),type="l",xlab="Especificidad::TNR",ylab="Sensibilidad::TPR",...) #cambiado 23/11/18
    }
    if(tpr.adic=="data"){
      df <- data.frame(x=unlist(perf@x.values),y=unlist(perf@y.values)) #cambiado 24/11/18
      if(!is.null(tpr.nmb))
        df$nmb <- tpr.nmb
      return(df)
    }
  }
}

## Funcion xa extraer AUROC de una pa todas las iteraciones -- desde R62 (211117)
#(esto sirve solo AFUERA de asm2(); p'hacer algo q funque ADENTRO cambiar el enfoque)
#(modificada 29/11/18 pa cumplir 'nuevo formato' ASM)

#' Funcion para extraer AUROC
#'
#' Util para listas, sirve solo AFUERA de asm2()
#'
#'
#'
#' @param obj Objeto
#' @param ... No implementado
auroc <- function(obj,...){
  clo <- class(obj)
  if(clo!="asm")
    stop("Funcion solo valida para objetos de clase 'asm'")
  lg_salida <- obj$Metadatos$parti
  lg_mi <- length(obj$Metadatos$mimp)
  a <- data.frame(matrix(nrow=lg_salida,ncol=lg_mi))
  vec <- seq(from=5,to=(lg_mi*2)+3,by=2)
  for(i in 1:lg_salida){
    for(j in seq_along(vec)){
      a[i,j] <- furoc(pred=obj$ASM$YprVsPaP[[i]][,vec[j]],patroro=obj$ASM$YprVsPaP[[i]][,3],med="auc")
    }
  }
  names(a) <- paste("AUROC_",obj$mimp,sep="")
  a
}



### 2.Resultados ###########################################

#library(uefi)
#load("C:/Users/Daniel/Documents/Estudio/Maestria/Tesis/Reuniones/R66-120418/Pedidos/1.Corridas/Resultados/CorrG08_Y2-50p6pr_Blq07-10.RData")
#load("//UNIENS07/Unidad/12. Usuarios/Daniel/UEFI/Tesis-DA/Reuniones/R66-120418/Pedidos/1.Corridas/Resultados/CorrG08_Y1-50pCu_Blq16-20.RData")
#a <- resblq(obj=g08_y250p6pr_X1,formulas=130:200,binf.qbest,binf.indic,ord.mdcmed=c("esp","fpr","acc"),vdm.formula=formulas)
#debugonce(resblq)
#a <- resblq(obj=g08_y150pCu_X1,formulas=301:400,ord.mdcmed=c("esp","fpr","acc"),vdm.formula=formulas)
