#############################################################
###### TESIS DA [Reunion 65]: funciones de USO GENERAL ######
#############################################################

## Version 1: desde 14:24 05/07/2017

### Funcion 2 "vdm"
#-- Que hace: devuelve formulas para variables en un data.frame
#-- Entra: dataframe c/datos, vi,vn_vd vectores numericos indicando variables (indep,dep)
#-- Sale: vector de caracteres c/formulas del estilo 'Y~X'
#-- Otros: mejor version S-A-C que pude desarrollar (mapply casi casi, pero no)

#' @export
vdm <- function(dat,vn_vi,vd){
  vi <- names(dat[,vn_vi])
  vd <- names(dat)[vd]
  ldm <- lapply(seq_along(vi),function(m){
    ye <- vd
    equises <- apply(X=combn(vi,m),MARGIN=2,paste,collapse="+")
    paste(ye,equises,sep="~")
  })
  vdmfinal <- unlist(ldm)
  vdmfinal
}

#############################################################

### Funciones 5: funciones internas (clasificar como 'internal')

#' Funcion interna [vespe()]
#'
#' Calcula frecuencias esperadas en TDEs
#'
#' Entra: tabla de contingencia/doble entrada; Sale: frecuencias esperadas
#'
#' @keywords internal
#' @export
vespe <- function(tdc,mostrar=TRUE,...){
  nf <- dim(tdc)[1]
  nc <- dim(tdc)[2]
  sufi <- as.numeric(rowSums(tdc))
  suco <- as.numeric(colSums(tdc))
  tab <- matrix(0,nrow=nf,ncol=nc,byrow=TRUE)
  for(i in 1:nf){
    for(j in 1:nc){
      tab[i,j] <- sufi[i]*suco[j]/sum(sufi)
    }
  }
  if(mostrar) return(list("V.Esp"=tab,"SumaxFila"=sufi,"SumaxCol"=suco,"Total"=sum(sufi)))
  else invisible(list("V.Esp"=tab,"SumaxFila"=sufi,"SumaxCol"=suco,"Total"=sum(sufi)))
}

#' Funcion interna [ldcmeta()]
#'
#' Crea LdC "x arriba" con metadatos extraibles de R
#'
#' Entra: dataframe, nombres de columnas (ej: ("NroVar","NombreVar","Class_R","NivelesFactor"));
#' Sale: version 'raw' de Libro de Codigos para datos en dataframe proporcionado
#'
#' @keywords internal
#' @export
ldcmeta <- function(dat,nombrescol=c("NroVar","NombreVar","Class_R","NivelesFactor"),exportar=TRUE){
  nf <- nrow(dat)
  nc <- ncol(dat)
  if(!is.null(nombrescol))
    ln <- length(nombrescol)
  else
    ln <- max(4,length(nombrescol))
  ldc <- data.frame(matrix(nrow=nc,ncol=ln))
  for(i in 1:nc)
    ldc[i,] <- c(i,names(dat)[i],class(dat[,i]),ifelse(class(dat[,i])=='factor',paste(levels(dat[,i]),collapse=", "),NA),
      strsplit(noquote(paste(rep(NA,length(nombrescol)-4),collapse=', ')),split=",")[[1]])
  ldc <- data.frame(ldc)
  if(is.null(nombrescol))
    names(ldc) <- c("NroVar","NombreVar","Class_R","NivelesFactor")
  else names(ldc) <- nombrescol
  if(exportar){
    write.table(x=ldc,file="LibroCodigos_vXX.txt",sep='\t',dec='.')
    return(ldc)
  }
  else ldc
}


#' Funcion interna [jyo_df()]
#'
#' Junta y ordena data.frame x ranking de primer df usado
#'
#' Sirve para sacar informacion rankeable mas facilmente de objetos 'asm'.
#' Modificacion de https://www.r-statistics.com/2012/01/merging-two-data-frame-objects-while-preserving-the-rows-order/
#'
#' @keywords internal
#' #@export
jyo_df <- function(dfa,dfb,...,ordensegun=1,ordenado=TRUE){
  agr_id <- function(datfr){
    dfid <- data.frame(datfr,"id"=seq_len(nrow(datfr)))
    dfid
  }
  aoysd_id <- function(datfr){
    if(!any(colnames(datfr)=="id"))
      stop("aoysd_id solo sfunciona con DFs con ID")
    orden <- order(datfr$id)
    restocols <- colnames(datfr)!="id"
    dfn <- datfr[orden,restocols]
    dfn
  }
  if(ordensegun==1){
    a1 <- aoysd_id(merge(x=agr_id(dfa),y=dfb,...,sort=FALSE))
    a1
  }
  if(ordensegun==2){
    a2 <- aoysd_id(merge(x=dfa,y=agr_id(dfb),...,sort=FALSE))
    a2
  } else {
    return(merge(x=dfa,y=dfb,...,sort=ordenado))
  }
}


#' Funcion interna [ord_df()]
#'
#' Funcion ord_df: ordena data.frame x ranking (>valor mas alto en el ranking)
#'
#' Sirve para sacar informacion rankeable mas facilmente de objetos 'asm'.
#'
#' @keywords internal
ord_df <- function(df,compl=FALSE,rn=NULL,...) {
  if(is.null(rn))
    nof <- row.names(df)
  else
    nof <- rn
  nf <- nrow(df)
  nc <- ncol(df)
  ejord <- data.frame(matrix(nrow=nrow(df),ncol=ncol(df)))
  medi <- data.frame("mediana"=matrix(nrow=nrow(df),ncol=1),row.names=nof)
  for(j in 1:nc){
    ejord[,j] <- rank(df[,j],ties.method="average") #Orden OK!
  }
  for(i in 1:nf){
    medi[i,] <- median(as.numeric(ejord[i,],row.names=nof))
  }
  emo <- cbind(df,ejord,medi)
  rk <- emo[order(rank(emo$mediana),decreasing=TRUE),ncol(emo),drop=FALSE] #OK
  if(compl)
    list(rk=rk,"SalidaCompl"=emo,"Advertencia"="A mayor valor de mediana, mas importancia en ranking")
  else rk
}


#' Funcion interna [ajp()]
#'
#' Ajuste Parametros para modelos de ML utilizados
#'
#' Utilizada para ajustar simultanteamente parametros de funciones dentro de loop
#'  con los mismos datos de entrenamiento (SVM,RF,GLM)
#'
#' @keywords internal
#' #@export
ajp <- function(fo,dat,svm.g,svm.c,rf.step,lg.family="binomial",lg.step=TRUE,
  cart.meth="class",rf.doBest=FALSE,rf.trace=FALSE,rf.plot=FALSE,
  salida="completa",...){
  fo <- as.formula(fo)
  mof <- model.frame(fo,data=dat) #controlar formatos y otros (TODO)
  dat.y <- model.response(mof)
  Xs <- attr(mof,"terms")
  attr(mof,"intercept") <- 0
  datx <- model.matrix(Xs,mof)
  dat.x <- datx[,-1] #sino le paso directo matriz de diseño
  aj_svm <- e1071::tune.svm(x=dat.x,y=dat.y,gamma=svm.g,cost=svm.c,...)
  aj_rf <- e1071::tuneRF(x=dat.x,y=dat.y,stepFactor=rf.step,doBest=rf.doBest,
    trace=rf.trace,plot=rf.plot)
  cart <- rpart::rpart(fo,data=dat,method=cart.meth)
  cart_cpopt <- cart$cptable[which.min(cart$cptable[,"xerror"]),"CP"]
  lgt <- glm(fo,family=lg.family,data=dat,na.action=na.omit)
  if(lg.step){
    #m.logit.sa <- vector(mode="list")#,length=max(parti))
    lgt.sa <- MASS::stepAIC(lgt,scope=list(upper=fo,lower=~1),direction="ba",trace=0)
  }
  if(salida=="completa"){
    list("SVM_perf"=aj_svm$performances,"RF_mtry"=aj_rf,"CART_cp"=cart$cptable,
      "Logit_SA"=lgt.sa)
  } else {
    list("SVM_gamma"=aj_svm$best.parameters$gamma,"SVM_costo"=aj_svm$best.parameters$cost,
      "RF_mtry"=aj_rf[order(aj_rf[,2])][1],"CART_cp"=cart_cpopt,"Logit_SA"=lgt.sa)
  }
}


#############################################################


#' Funcion interna [reductable()]
#'
#' Generalizacion de matrices de cofusion (MdConf) y calculos asociados
#'
#' (En fase experimental; falta mejorar y generalizar para ampliar matrices
#' de cofusion)
#'
#' @keywords internal
#' @export
reductable <- function(tab){
  nf <- nrow(tab)
  nc <- ncol(tab)
  s0 <- tab[1,1]
  s1 <- sum(tab[2:nf,2:nc])
  s2 <- sum(tab[-1,1])
  s3 <- sum(tab[1,-1])
  nut <- matrix(c(s0,s3,s2,s1),ncol=2,byrow=TRUE,dimnames=list(c("ClasexFila","NoClasexFila"),c("ClasexCol","NoClasexCol")))
  list("TablaOrig"=tab,"TablaNueva"=nut)
}
#Ejemplo de prueba (R47 OK)
#reductable(tt)
#mt <- matrix(1:25,nrow=5,byrow=TRUE)
#mdcmed(reductable(mt)$TablaNueva)

#############################################################

#' Funcion interna [infoxcol()]
#'
#' Extrae informacion util por columna de data.frame
#'
#' Utilizada en generacion de Libro de Codiigos (LdC) de un data.frame cualquiera
#'
#' @keywords internal
infoxcol <- function(dat,...){
  nf <- nrow(dat)
  nc <- ncol(dat)
  cla <- unlist(lapply(dat,function(x) class(x)))
  typ <- unlist(lapply(dat,function(x) typeof(x)))
  ixc <- data.frame("NroCol"=seq(1,nc),"Clase"=cla,"TipoAlm"=typ)
  ixc
}

#############################################################

#' Funcion interna [acm_err()]
#'
#' Extrae informacion util para realizar ACM de errores/aciertos en ASM
#'
#' Utilizada con objetos ...obj[[form]]$ASM$MC_Todos$Pred
#'
#' @keywords internal
acm_err <- function(lst,dfy,dfy_vars,acm=TRUE,acm_quanti.sup,acm_quali.sup,...){
  loadNamespace("factoextra")
  loadNamespace("FactoMineR")
  if(class(lst)!="list") stop("Entrada: solo listas")
  if(class(dfy)!="data.frame") stop("Datos solo en formato data.frame")
  if(is.null(dfy_vars)) stop("Pasar que variables del DF quedan para el ACM")
  if(is.null(acm_quanti.sup | acm_quali.sup)) stop("Pasar variables suplementarias (cuaLi,cuanTi) para el ACM")
  df_lst <- do.call(rbind,lst)
  #Nuevas columnas para DF: uso en ACM
  df_lst$Ypred_CART <- factor(with(df_lst,ifelse(Yp_cart==Yobs.x,"CART_OK","CART_Erra")))
  df_lst$Ypred_SVM <- factor(with(df_lst,ifelse(Yp_svm==Yobs.x,"SVM_OK","SVM_Erra")))
  df_lst$Ypred_RF <- factor(with(df_lst,ifelse(Yp_rf==Yobs.x,"RF_OK","RF_Erra")))
  df_lst$Ypred_Lgt <- factor(with(df_lst,ifelse(Yp_logit==Yobs.x,"Lgt_OK","Lgt_Erra")))
  df_lst$Ypred_Bst <- factor(with(df_lst,ifelse(Yp_boost==Yobs.x,"Bst_OK","Bst_Erra")))
  df_lst$Ypred_CDB <- factor(with(df_lst,ifelse(Yp_CDB==Yobs.x,"CDB_OK","CDB_Erra")))
  df_lst$Ypred_VM <- factor(with(df_lst,ifelse(VM==Yobs.x,"VM_OK","VM_Erra")))
  df_lst$Ypred_MP <- factor(with(df_lst,ifelse(MP==Yobs.x,"MP_OK","MP_Erra")))
  df_lst$Ypred_WA <- factor(with(df_lst,ifelse(WAU==Yobs.x,"WA_OK","WA_Erra")))
  df_lst$`Erra>1mod` <- factor(with(df_lst,ifelse(Yobs.x!=Yp_cart | Yobs.x!=Yp_svm | Yobs.x!=Yp_rf | Yobs.x!=Yp_logit | Yobs.x!=Yp_boost | Yobs.x!=Yp_CDB | Yobs.x!=VM | Yobs.x!=MP | Yobs.x!=WAU,"Erra>1mod","Acierta")))
  #Ordena obs segun erra/acierta
  errTodos_df_lst <- sort(unique(df_lst$id.x[which(df_lst$`Erra>1mod`=='Erra>1mod')]))
  aciTodos_df_lst <- setdiff(sort(unique(df_lst$id.x)),errTodos_df_lst)
  #Sobre DF de datos originales: crea nueva variable "a quien aciertan siempre?"
  dfy$aciTodos <- factor(with(dfy,ifelse(rownames(dfy) %in% aciTodos_df_lst,"Todos_aciertan","Alguno_erra")))
  if(acm){
    ## 4a) ACM de errores por modelo (modif 050419 para incluir ID x obs)
    df_lst_acm <- df_lst[,c("OrdxObs","id.x",grep(pattern="Ypred_.*",x=names(df_lst),value=TRUE),"Erra>1mod")]
    #(1) Corremos ACM
    acmErr_df_lst_acm <- FactoMineR::MCA(X=df_lst_acm[,3:ncol(df_lst_acm)],ncp=10,graph=FALSE)
    #(3) Descripcion de cada dimension
    dedi_acmErr_df_lst_acm <- FactoMineR::dimdesc(acmErr_df_lst_acm,axes=c(1,2))
    #(4) Calidad de representacion y contribucion en grafico+salida
    varimp_acmErr_df_lst_acm <- factoextra::get_mca_var(acmErr_df_lst_acm)
    obsimp_acmErr_df_lst_acm <- factoextra::get_mca_ind(acmErr_df_lst_acm) #agregado 30/11/18
    ## 4b) ACM de datos (variables activas Y suplementarias)
    #(1) Corremos ACM -- c(unlist(lapply(dfy,class)) %in% "factor",unlist(lapply(dfy,class)) %in% "numeric")
    dfy_acm <- droplevels(dfy[,c(dfy_vars,ncol(dfy))]) #30/11/18: corregido pa incluir variable aciTodos (solo valores numericos)
    acm_dfy_ <- FactoMineR::MCA(X=dfy_acm,quanti.sup=acm_quanti.sup,quali.sup=acm_quali.sup,graph=FALSE)
    #(3) Descripcion de cada dimension
    dedi_acm_dfy_ <- FactoMineR::dimdesc(acm_dfy_,axes=c(1,2))
    #(4) Calidad de representacion y contribucion en grafico+salida
    varimp_acm_dfy_ <- factoextra::get_mca_var(acm_dfy_) #{Ext,SinDat} outliers mal >> sacarlos del analisis!
    obsimp_acm_dfy_ <- factoextra::get_mca_ind(acm_dfy_) #agregado 30/11/18
  }
  return(list("Salida_ACM"=list("ACM_Err_Mod"=list("SalidaACM"=acmErr_df_lst_acm,"DescrxDim"=dedi_acmErr_df_lst_acm,"QRepr_v"=varimp_acmErr_df_lst_acm,
                                                   "QRepr_obs"=obsimp_acmErr_df_lst_acm),
                         "ACM_Err_Dat"=list("SalidaACM"=acm_dfy_,"DescrxDim"=dedi_acm_dfy_,"QRepr_v"=varimp_acm_dfy_,"QRepr_obs"=obsimp_acm_dfy_)),
              "Datos_modificados"=list("DF_Err_ACM"=df_lst_acm,"DF_datos_AciErr"=cbind("id"=dfy[,1],dfy_acm))))
}

# load("X:/6. USUARIOS/3. Daniel/UEFI/Tesis-DA/Reuniones/R73-120718/Pedidos/2.D_An/Resultados/Corr0816full_optimY1Y2_Blq1_vars0816_EM.RData")
# #levantas listas
# pru1 <- acm_err(g08_y1_50pCurs1ro_Opt2[[29]]$ASM$MC_Todos$Pred,g08y1,dfy_vars=c(2:6,9:12),acm_quanti.sup=5:6,acm_quali.sup=7:8)
# library(ggplot2)
# factoextra::fviz_screeplot(pru1$Salida_ACM$ACM_Err_Mod$SalidaACM,title="ACM errores: Grafico de baston (scree plot)",xlab="Dimensiones",ylab="%Var(.) explicada",caption="[Gen2008 -- Y1 -- Formula::29]") +
#   theme(plot.title=element_text(hjust=0.5),plot.subtitle=element_text(hjust=0.5))



#############################################################

#-- **__nombrefuncion__**
#-- Que hace:
#-- Entra:
#-- Sale:
#-- Otros: surge para sacar info rankeable mas facilmente de objetos 'asm'

##' Titulo -- DOCUMENTACION roxygen
##'
##' (parrafo1: va debajo del titulo, que hace en 1 oracion)
##'
##' (parrafos2-: van en Details, explayarse)
##'
##' #@export
##' #@keywords internal


#__nose__ <- function(){
#
#}