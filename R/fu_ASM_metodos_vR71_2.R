############################################################
###### TESIS DA [Reunion 71]: wrapper general modelos ######
############################################################

#' ASM: Ajuste Simultaneo Modelos (super corta) -- metodos implementados

#' Metodo 'plot' para objetos de clase 'asm' (basico, completar)
#' @importFrom ggplot2 ggplot
#' @param x Objeto de clase 'asm'
#' @param tipo (x ahora implementado 'boxplot')
#' @param ... (algo)
#' @keywords plot.asm
#' @export
#' @return
#' #@seealso
#' #@examples
#' plot.asm()

plot.asm2 <- function(x,tipo="boxplot",destacar=NULL,col.dest="blue",
  ind.mdc=c("sen","fpr","acc"),...){
  #TODO: implementar AUC-ROC como parte del metodo plot.asm
  #>>IMPRESCINDIBLE: GUARDAR predict(...) x modelo en salida asm2!!! PROBAR
  larg <- x$Metadatos$parti
  frml <- x$Metadatos$Formula
  if(!class(x) %in% c("asm","asm2")){
    paste0("MAL!! El objeto", deparse(substitute(x)),"no es de clase 'asm', sino de clase", class(x))
    break #esto es mejor que stop/stopifnot? 170618
  }
  if(!is.null(destacar) & !all(ind.mdc %in% destacar))
    stop("'ind.mdc' NO es subconjunto de 'destacar', revisar!")
  #S-A-C para poder graficar
  mimp <- x$Metadatos$mimp #para gralizar rep() antes de hacer dfnomb:
  repmi <- unlist(lapply(mimp, function(mimp) rep(mimp,larg))) #>> Modelo=rempi #FALTA PROBAR!
  dfnomb <- data.frame(Reduce(rbind,x$ASM$lge.gr),"Modelo"=repmi) #
  if(tipo=="boxplot"){
    dfa <- reshape2::melt(data=dfnomb,variable.name="Est_MdConf",value.name="Valor",
      id.vars="Modelo") #OK
	if(is.null(destacar))
		dfb <- subset(dfa,Est_MdConf %in% ind.mdc)
	else
		dfb <- subset(dfa,Est_MdConf %in% c(destacar,ind.mdc))
	gr1 <- ggplot(dfb,aes(x=Modelo,y=Valor,fill=Modelo))
  gr1 + geom_boxplot() +
    geom_boxplot(data=dfb,aes(x=Modelo,y=Valor),col=col.dest) +
      theme(text=element_text(size=8)) +
      facet_wrap(~Est_MdConf,scales="free") + labs(title=frml)
  } else {
    if((tipo!="boxplot"))
      cat("NO implementamos nada fuera del boxplot")
  }
}


## Version 6: 17/06/18 (v1:04/04/18,v2:12/04/18,v3:17/04/18,v4:25/04-21/05/18,v6:11/05/18)

#' Metodo 'summary' para objetos de clase 'asm' (basico, completar)
#'
#' (parrafo1: va debajo del titulo) Resume en tablas resultados de funcion \code{asm2()}
#'
#' (parrafos2-: van en Details, explayarse)
#'
#' @section Comentarios adicionales (poner lo que quieras; esto no sale en help pero si lo ves en archivo R)
#'
#' @param obj_1,obj_2 Objetos de clase [asm()] para resumir/comparar
#' @param nroform1,nroform2 Numero de formula utilizado (solo en caso de loops con muchas formulas)
#' @param indic_MdC Indicador(es) deseado(s) provenientes de una matriz de confusion. Las opciones
#'  actuales son "todos","ROC","aciertos","errores"
#' @param mdr Medida de resumen a mostrar en salida. Opciones implementadas: "media", "mediana"
#' @export
summary.asm <- function(obj_1,obj_2=NULL,nroform1,nroform2=NULL,indic_MdC,mdr="media"){
  #Generales: cambiar acceso a objetos con nueva ASM
  #(defecto: NO esta pensado para 1 sola formula, solo para bloques de formulas (objeto: lista con al menos 2))
  if(!class(obj_1) %in% c("asm","asm2","list"))
    paste0("MAL!! El objeto",deparse(substitute(obj_1)),"no es de clase 'asm' o 'list', sino de clase",class(obj_1))
  moc <- obj_1[[nroform1]]$Metadatos$moc
  ini <- c(is.null(obj_1),nroform1,indic_MdC)
  if(any(is.null(ini)))
    stop(paste("El objeto ",deparse(substitute(obj_1))," y/o ",deparse(substitute(indic_MdC))," y/o ",deparse(substitute(nroform1))," son NULL")) #control sencillo
  caso <- if((!is.null(obj_1) & !is.null(nroform1)) & (is.null(obj_2) & is.null(nroform2))) 1
    else if(is.null(obj_2) & !is.null(nroform2)) 2
  	else if(!is.null(obj_2) & is.null(nroform2)) 3
  	else if(!is.null(obj_2) & !is.null(nroform2)) 4
  	else 0 #caso irreal; no tendria que pasar NUNCA por aca
  #Funciones para calculos especificos
  cve <- function(x,dig=3){
    stopifnot(class(x) %in% c("integer","numeric","double"))
    round(sd(x)/abs(mean(x)),digits=dig)
  }
  mdr_f <- function(x,mdres="media",dig=3,...){
    switch(EXPR=mdres,
           media=round(mean(x,...),digits=dig),
           mediana=round(median(x,...),digits=dig))
  }
  toderp2 <- NULL
  ## A:sin MoCs
  if(length(moc)<=0){
    if(!is.null(obj_1)){ #pensar 1ro orden de TODAS las cosas: isnull(nform2) ... , !isnull(...)...
    mimp <- obj_1[[nroform1]]$Metadatos$mimp
    toderp1 <- list("CART"=obj_1[[nroform1]]$ASM$CART$iCART$todos.erp,"Logit"=obj_1[[nroform1]]$ASM$Logit$iLogit$todos.erp,
                   "SVM"=obj_1[[nroform1]]$ASM$SVM$iSVM$todos.erp,"RF"=obj_1[[nroform1]]$ASM$RF$iRF$todos.erp,
                   "Boost"=obj_1[[nroform1]]$ASM$Boost$iBoost$todos.erp,"CDB"=obj_1[[nroform1]]$ASM$CDB$iCDB$todos.erp)
    }
    if(!is.null(obj_2) & is.null(nroform2)){
      toderp2 <- list("CART"=obj_2[[nroform1]]$ASM$CART$iCART$todos.erp,"Logit"=obj_2[[nroform1]]$ASM$Logit$iLogit$todos.erp,
                      "SVM"=obj_2[[nroform1]]$ASM$SVM$iSVM$todos.erp,"RF"=obj_2[[nroform1]]$ASM$RF$iRF$todos.erp,
                      "Boost"=obj_2[[nroform1]]$ASM$Boost$iBoost$todos.erp,"CDB"=obj_2[[nroform1]]$ASM$CDB$iCDB$todos.erp)
    }
    if(is.null(obj_2) & !is.null(nroform2)){
      mimp2 <- obj_1[[nroform2]]$Metadatos$mimp
      toderp2 <- list("CART"=obj_1[[nroform2]]$ASM$CART$iCART$todos.erp,"Logit"=obj_1[[nroform2]]$ASM$Logit$iLogit$todos.erp,
                     "SVM"=obj_1[[nroform2]]$ASM$SVM$iSVM$todos.erp,"RF"=obj_1[[nroform2]]$ASM$RF$iRF$todos.erp,
                     "Boost"=obj_1[[nroform2]]$ASM$Boost$iBoost$todos.erp,"CDB"=obj_1[[nroform2]]$ASM$CDB$iCDB$todos.erp)
    }
    if(!is.null(obj_2) & !is.null(nroform2)){
      mimp2 <- obj_2[[nroform2]]$Metadatos$mimp
      toderp2 <- list("CART"=obj_2[[nroform2]]$ASM$CART$iCART$todos.erp,"Logit"=obj_2[[nroform2]]$ASM$Logit$iLogit$todos.erp,
                     "SVM"=obj_2[[nroform2]]$ASM$SVM$iSVM$todos.erp,"RF"=obj_2[[nroform2]]$ASM$RF$iRF$todos.erp,
                     "Boost"=obj_2[[nroform2]]$ASM$Boost$iBoost$todos.erp,"CDB"=obj_2[[nroform2]]$ASM$CDB$iCDB$todos.erp)
    }
    #Calculos: calculas TODO, devolves lo que usuario pide!!!
    if(!is.null(obj_1) & !is.null(indic_MdC)){#Falta controlar salida: si tenes -1 modelo sale 'subsc out of bounds'
      #todos1_tpr <- list(toderp$CART["sen"],toderp$Logit["sen"],toderp$SVM["sen"],toderp$RF["sen"],toderp$Boost["sen"],toderp$CDB["sen"])
      tod1res <- lapply(toderp1,function(lst) lapply(lst,function(x) mdr_f(t(na.omit(x)),mdres=mdr,dig=3,na.rm=TRUE))) #lapply anidados!
    	tod2res <- NULL
    }
    if(!is.null(toderp2) & !is.null(indic_MdC)){
      tod2res <- lapply(toderp2,function(lst) lapply(lst,function(x) mdr_f(t(na.omit(x)),mdres=mdr,dig=3,na.rm=TRUE))) #lapply anidados!
    }
    #A.Metadatos
    ## 1 solo objeto, 1 formula ### VER DE ACA EN MAS QUE FALTA PARA TERMINAAAARRRRR!!!
    if(!is.null(obj_1) & is.null(obj_2) & is.null(nroform2)){
      cat("\n=================INFORMACION GENERAL: METADATOS==================\n")
      cat("\nModelos implementados:\n",deparse(obj_1[[nroform1]]$Metadatos$mimp),"\n")
      cat("\nVariable a predecir (Y):\n",deparse(obj_1[[nroform1]]$Metadatos$Y),"de tipo",deparse(obj_1[[nroform1]]$Metadatos$tipoY),"\n")
      #cat("\nCall (formula para todos los modelos):\n",deparse(obj_1[[nroform1]]$Metadatos$Formula),"\n")
      cat("\nCantidad de permutaciones en {MEnt,MTest}:\n",deparse(obj_1[[nroform1]]$Metadatos$parti),"\n")
      cat("\nProporcion muestra entrenamiento:\n",deparse(100*(obj_1[[nroform1]]$Metadatos$propme)),"%\n")
      cat("\n========COMPARACION: INDICADORES DE MATRICES DE CONFUSION========\n\n")
      cat("\n(Se reporta **",mdr,"** entre las",deparse(obj_1[[nroform1]]$Metadatos$parti),"particiones para estadisticos '",indic_MdC,"')\n")
      cat("\nCall (formula para todos los modelos):\n",deparse(obj_1[[nroform1]]$Metadatos$Formula),"\n\n")
  	}
  	## 2 objetos, 1 sola formula a comparar
    if(!is.null(obj_1) & !is.null(obj_2) & is.null(nroform2)){
  	cat("\n=================INFORMACION GENERAL: METADATOS==================\n")
      cat("\nModelos implementados:\n --",deparse(obj_1[[nroform1]]$Metadatos$mimp),"\n --",deparse(obj_2[[nroform1]]$Metadatos$mimp),"\n")
      cat("\nVariable a predecir (Y):\n --",deparse(obj_1[[nroform1]]$Metadatos$Y),"de tipo",deparse(obj_1[[nroform1]]$Metadatos$tipoY),"\n --",deparse(obj_2[[nroform1]]$Metadatos$Y),"de tipo",deparse(obj_2[[nroform1]]$Metadatos$tipoY),"\n")
      cat("\nCantidad de permutaciones en {MEnt,MTest}:\n --",deparse(obj_1[[nroform1]]$Metadatos$parti),"Objeto 1\n --",deparse(obj_2[[nroform1]]$Metadatos$parti),"Objeto 2\n\n")
      cat("\nProporcion muestra entrenamiento:\n --",deparse(100*(obj_1[[nroform1]]$Metadatos$propme)),"% Objeto 1\n --",deparse(100*(obj_2[[nroform1]]$Metadatos$propme)),"% Objeto 2\n")
      cat("\n========COMPARACION: INDICADORES DE MATRICES DE CONFUSION========\n\n")
      cat("\n(Se reporta **",mdr,"** entre las",deparse(obj_1[[nroform1]]$Metadatos$parti),"particiones para estadisticos '",indic_MdC,"')\n")
      cat("\nCall (formula para todos los modelos del Objeto 1):\n",deparse(obj_1[[nroform1]]$Metadatos$Formula),"\n\n")
    }
    ## 1 solo objeto, 2 formulas a comparar FALTAAAAA!!!
    if(!is.null(obj_1) & is.null(obj_2) & !is.null(nroform2)){
  	  cat("\n=================INFORMACION GENERAL: METADATOS==================\n")
      cat("\nModelos implementados:\n --",deparse(obj_1[[nroform1]]$Metadatos$mimp),"\n --",deparse(obj_1[[nroform2]]$Metadatos$mimp),"\n")
  	  cat("\nVariable a predecir (Y):\n",deparse(obj_1[[nroform1]]$Metadatos$Y),"de tipo",deparse(obj_1[[nroform1]]$Metadatos$tipoY),"\n")
      cat("\nCantidad de permutaciones en {MEnt,MTest}:\n --",deparse(obj_1[[nroform1]]$Metadatos$parti),"Formula 1\n --",deparse(obj_1[[nroform2]]$Metadatos$parti),"Formula 2\n\n")
      cat("\nProporcion muestra entrenamiento:\n --",deparse(100*(obj_1[[nroform1]]$Metadatos$propme)),"% Formula 1\n --",deparse(100*(obj_1[[nroform2]]$Metadatos$propme)),"% Formula 2\n")
      cat("\n========COMPARACION: INDICADORES DE MATRICES DE CONFUSION========\n\n")
      cat("\n(Se reporta **",mdr,"** entre las",deparse(obj_1[[nroform1]]$Metadatos$parti),"particiones para estadisticos '",indic_MdC,"')\n")
      cat("\nCall (formula para todos los modelos de la Formula 1):\n",deparse(obj_1[[nroform1]]$Metadatos$Formula),"\n\n")
      cat("\nCall (formula para todos los modelos de la Formula 2):\n",deparse(obj_1[[nroform2]]$Metadatos$Formula),"\n\n")
      #cat("                       |   CART   |  Logit    |   SVM     |    RF     | Boosting  |   CDB     |  \n")
    }
    ## 2 objetos, 2 formulas a comparar
    if(!is.null(obj_1) & !is.null(obj_2) & !is.null(nroform2)){
      cat("\nModelos implementados:\n --",deparse(obj_1[[nroform1]]$Metadatos$mimp),"\n --",deparse(obj_2[[nroform2]]$Metadatos$mimp),"\n")
      cat("\nVariable a predecir (Y):\n --",deparse(obj_1[[nroform1]]$Metadatos$Y),"de tipo",deparse(obj_1[[nroform1]]$Metadatos$tipoY),"\n --",deparse(obj_2[[nroform2]]$Metadatos$Y),"de tipo",deparse(obj_2[[nroform2]]$Metadatos$tipoY),"\n")
      cat("\nCantidad de permutaciones en {MEnt,MTest}:\n --",deparse(obj_1[[nroform1]]$Metadatos$parti),"Objeto 1\n --",deparse(obj_2[[nroform2]]$Metadatos$parti),"Objeto 2\n\n")
      cat("\nProporcion muestra entrenamiento:\n --",deparse(100*(obj_1[[nroform1]]$Metadatos$propme)),"% Objeto 1\n --",deparse(100*(obj_2[[nroform2]]$Metadatos$propme)),"% Objeto 2\n")
      cat("\n========COMPARACION: INDICADORES DE MATRICES DE CONFUSION========\n\n")
      cat("\n(Se reporta **",mdr,"** entre las",deparse(obj_1[[nroform1]]$Metadatos$parti),"particiones para estadisticos '",indic_MdC,"')\n")
      cat("\nCall (formula para todos los modelos del Objeto 1):\n",deparse(obj_1[[nroform1]]$Metadatos$Formula),"\n\n")
      cat("\nCall (formula para todos los modelos del Objeto 2):\n",deparse(obj_2[[nroform2]]$Metadatos$Formula),"\n\n")
    }
    #nfo <- if(!is.null(nroform2) & !is.null(obj_2)) 1 else ""
    nfo <- switch(caso,`1`="",`2`=1,`3`=1,`4`=1)
    if(!is.null(obj_1)) #exceso de ifs (pensar opcion '&&'; si ya hay stop() antes no seguir preguntando)
  	  cat("Formula ",nfo,"   :",deparse(obj_1[[nroform1]]$Metadatos$Formula),"\n\n")
  	cat("                       |   CART   |   Logit   |    SVM    |     RF    | Boosting  |    CDB    |  \n")
    #B.Tablas con salidas
    if(indic_MdC %in% c("todos","ROC","aciertos")){
      #sen
      cat("Sen::P(Y=1|Y^=1)       | ", if(length(tod1res[[1]])>0) sprintf('%.3f',tod1res[[1]][[1]]) else '     ',"|",if(length(tod1res[[2]])>0) sprintf('%.3f',tod1res[[2]][[1]]) else '     ',"|",if(length(tod1res[[3]])>0) sprintf('%.3f',tod1res[[3]][[1]]) else '     ',"|",if(length(tod1res[[4]])>0) sprintf('%.3f',tod1res[[4]][[1]]) else '     ',"|",if(length(tod1res[[5]])>0) sprintf('%.3f',tod1res[[5]][[1]]) else '     ',"|",if(length(tod1res[[6]])>0) sprintf('%.3f',tod1res[[6]][[1]]) else '     ',"| \n")
     }
    if(indic_MdC %in% c("todos","aciertos")){
      #esp
      cat("Esp::P(Y=0|Y^=0)       | ", if(length(tod1res[[1]])>0) sprintf('%.3f',tod1res[[1]][[2]]) else '     ',"|",if(length(tod1res[[2]])>0) sprintf('%.3f',tod1res[[2]][[2]]) else '     ',"|",if(length(tod1res[[3]])>0) sprintf('%.3f',tod1res[[3]][[2]]) else '     ',"|",if(length(tod1res[[4]])>0) sprintf('%.3f',tod1res[[4]][[2]]) else '     ',"|",if(length(tod1res[[5]])>0) sprintf('%.3f',tod1res[[5]][[2]]) else '     ',"|",if(length(tod1res[[6]])>0) sprintf('%.3f',tod1res[[6]][[2]]) else '     ',"| \n")
      }
    if(indic_MdC %in% c("todos","aciertos")){
      #PPV
      cat("VPP::P(Y^=1|Y=1)       | ", if(length(tod1res[[1]])>0) sprintf('%.3f',tod1res[[1]][[3]]) else '     ',"|",if(length(tod1res[[2]])>0) sprintf('%.3f',tod1res[[2]][[3]]) else '     ',"|",if(length(tod1res[[3]])>0) sprintf('%.3f',tod1res[[3]][[3]]) else '     ',"|",if(length(tod1res[[4]])>0) sprintf('%.3f',tod1res[[4]][[3]]) else '     ',"|",if(length(tod1res[[5]])>0) sprintf('%.3f',tod1res[[5]][[3]]) else '     ',"|",if(length(tod1res[[6]])>0) sprintf('%.3f',tod1res[[6]][[3]]) else '     ',"| \n")
      }
    if(indic_MdC %in% c("todos","aciertos")){
      #NPV
      cat("VPN::P(Y^=0|Y=0)       | ", if(length(tod1res[[1]])>0) sprintf('%.3f',tod1res[[1]][[4]]) else '     ',"|",if(length(tod1res[[2]])>0) sprintf('%.3f',tod1res[[2]][[4]]) else '     ',"|",if(length(tod1res[[3]])>0) sprintf('%.3f',tod1res[[3]][[4]]) else '     ',"|",if(length(tod1res[[4]])>0) sprintf('%.3f',tod1res[[4]][[4]]) else '     ',"|",if(length(tod1res[[5]])>0) sprintf('%.3f',tod1res[[5]][[4]]) else '     ',"|",if(length(tod1res[[6]])>0) sprintf('%.3f',tod1res[[6]][[4]]) else '     ',"| \n")
      }
    if(indic_MdC %in% c("todos","aciertos")){
      #Acc
      cat("Precision::Acc         | ", if(length(tod1res[[1]])>0) sprintf('%.3f',tod1res[[1]][[5]]) else '     ',"|",if(length(tod1res[[2]])>0) sprintf('%.3f',tod1res[[2]][[5]]) else '     ',"|",if(length(tod1res[[3]])>0) sprintf('%.3f',tod1res[[3]][[5]]) else '     ',"|",if(length(tod1res[[4]])>0) sprintf('%.3f',tod1res[[4]][[5]]) else '     ',"|",if(length(tod1res[[5]])>0) sprintf('%.3f',tod1res[[5]][[5]]) else '     ',"|",if(length(tod1res[[6]])>0) sprintf('%.3f',tod1res[[6]][[5]]) else '     ',"| \n")
      }
    if(indic_MdC %in% c("todos","ROC","errores")){
      cat("FalsosPositivos::fpr   | ", if(length(tod1res[[1]])>0) sprintf('%.3f',tod1res[[1]][[6]]) else '     ',"|",if(length(tod1res[[2]])>0) sprintf('%.3f',tod1res[[2]][[6]]) else '     ',"|",if(length(tod1res[[3]])>0) sprintf('%.3f',tod1res[[3]][[6]]) else '     ',"|",if(length(tod1res[[4]])>0) sprintf('%.3f',tod1res[[4]][[6]]) else '     ',"|",if(length(tod1res[[5]])>0) sprintf('%.3f',tod1res[[5]][[6]]) else '     ',"|",if(length(tod1res[[6]])>0) sprintf('%.3f',tod1res[[6]][[6]]) else '     ',"| \n")
      }
    if(indic_MdC %in% c("todos","errores")){
      cat("FalsosDescubr::fdr     | ", if(length(tod1res[[1]])>0) sprintf('%.3f',tod1res[[1]][[7]]) else '     ',"|",if(length(tod1res[[2]])>0) sprintf('%.3f',tod1res[[2]][[7]]) else '     ',"|",if(length(tod1res[[3]])>0) sprintf('%.3f',tod1res[[3]][[7]]) else '     ',"|",if(length(tod1res[[4]])>0) sprintf('%.3f',tod1res[[4]][[7]]) else '     ',"|",if(length(tod1res[[5]])>0) sprintf('%.3f',tod1res[[5]][[7]]) else '     ',"|",if(length(tod1res[[6]])>0) sprintf('%.3f',tod1res[[6]][[7]]) else '     ',"| \n")
     }
    if(indic_MdC %in% c("todos","errores")){
      cat("FalsosNegativos::fnr   | ", if(length(tod1res[[1]])>0) sprintf('%.3f',tod1res[[1]][[8]]) else '     ',"|",if(length(tod1res[[2]])>0) sprintf('%.3f',tod1res[[2]][[8]]) else '     ',"|",if(length(tod1res[[3]])>0) sprintf('%.3f',tod1res[[3]][[8]]) else '     ',"|",if(length(tod1res[[4]])>0) sprintf('%.3f',tod1res[[4]][[8]]) else '     ',"|",if(length(tod1res[[5]])>0) sprintf('%.3f',tod1res[[5]][[8]]) else '     ',"|",if(length(tod1res[[6]])>0) sprintf('%.3f',tod1res[[6]][[8]]) else '     ',"| \n")
      }
    if(indic_MdC %in% c("todos","errores")){
      cat("FalsaOmision::fom      | ", if(length(tod1res[[1]])>0) sprintf('%.3f',tod1res[[1]][[9]]) else '     ',"|",if(length(tod1res[[2]])>0) sprintf('%.3f',tod1res[[2]][[9]]) else '     ',"|",if(length(tod1res[[3]])>0) sprintf('%.3f',tod1res[[3]][[9]]) else '     ',"|",if(length(tod1res[[4]])>0) sprintf('%.3f',tod1res[[4]][[9]]) else '     ',"|",if(length(tod1res[[5]])>0) sprintf('%.3f',tod1res[[5]][[9]]) else '     ',"|",if(length(tod1res[[6]])>0) sprintf('%.3f',tod1res[[6]][[9]]) else '     ',"| \n")
      }
    if(indic_MdC %in% c("todos","errores")){
      cat("ErrorGeneral::erg      | ", if(length(tod1res[[1]])>0) sprintf('%.3f',tod1res[[1]][[10]]) else '     ',"|",if(length(tod1res[[2]])>0) sprintf('%.3f',tod1res[[2]][[10]]) else '     ',"|",if(length(tod1res[[3]])>0) sprintf('%.3f',tod1res[[3]][[10]]) else '     ',"|",if(length(tod1res[[4]])>0) sprintf('%.3f',tod1res[[4]][[10]]) else '     ',"|",if(length(tod1res[[5]])>0) sprintf('%.3f',tod1res[[5]][[10]]) else '     ',"|",if(length(tod1res[[6]])>0) sprintf('%.3f',tod1res[[6]][[10]]) else '     ',"| \n")
      }
    cat("Prevalencia (prv)      | ", if(length(tod1res[[1]])>0) sprintf('%.3f',tod1res[[1]][[12]]) else '     ',"|",if(length(tod1res[[2]])>0) sprintf('%.3f',tod1res[[2]][[12]]) else '     ',"|",if(length(tod1res[[3]])>0) sprintf('%.3f',tod1res[[3]][[12]]) else '     ',"|",if(length(tod1res[[4]])>0) sprintf('%.3f',tod1res[[4]][[12]]) else '     ',"|",if(length(tod1res[[5]])>0) sprintf('%.3f',tod1res[[5]][[12]]) else '     ',"|",if(length(tod1res[[6]])>0) sprintf('%.3f',tod1res[[6]][[12]]) else '     ',"| \n")
    cat('\n\n')
    if(!is.null(obj_1) & !is.null(tod2res)){
      cat("Formula ",nfo+1,"   :")
  	if(caso==2)
  	  cat(deparse(obj_1[[nroform2]]$Metadatos$Formula),"\n\n")
  	else if(caso==3)
  	  cat(deparse(obj_2[[nroform1]]$Metadatos$Formula),"\n\n")
  	else if(caso==4)
  	  cat(deparse(obj_2[[nroform2]]$Metadatos$Formula),"\n\n")
  	cat("                       |   CART   |   Logit   |    SVM    |     RF    | Boosting  |    CDB    |  \n")
      if(indic_MdC %in% c("todos","ROC","aciertos")){#CAMBIAR tod2res como tod1res!!!
        #sen
        #cat("                       | 12CART78 | 1Logit78  | 12SVM678  | 123RF678  | Boosting  | 12CDB678  |  \n")
        cat("Sen::P(Y=1|Y^=1)       | ",if(length(tod2res[[1]])>0) sprintf('%.3f',tod2res[[1]][[1]]) else '     ',"|",if(length(tod2res[[2]])>0) sprintf('%.3f',tod2res[[2]][[1]]) else '     ',"|",if(length(tod2res[[3]])>0) sprintf('%.3f',tod2res[[3]][[1]]) else '     ',"|",if(length(tod2res[[4]])>0) sprintf('%.3f',tod2res[[4]][[1]]) else '     ',"|",if(length(tod2res[[5]])>0) sprintf('%.3f',tod2res[[5]][[1]]) else '     ',"|",if(length(tod2res[[6]])>0) sprintf('%.3f',tod2res[[6]][[1]]) else '     ',"| \n")
      }
      if(indic_MdC %in% c("todos","aciertos")){
        #esp
        cat("Esp::P(Y=0|Y^=0)       | ",if(length(tod2res[[1]])>0) sprintf('%.3f',tod2res[[1]][[2]]) else '     ',"|",if(length(tod2res[[2]])>0) sprintf('%.3f',tod2res[[2]][[2]]) else '     ',"|",if(length(tod2res[[3]])>0) sprintf('%.3f',tod2res[[3]][[2]]) else '     ',"|",if(length(tod2res[[4]])>0) sprintf('%.3f',tod2res[[4]][[2]]) else '     ',"|",if(length(tod2res[[5]])>0) sprintf('%.3f',tod2res[[5]][[2]]) else '     ',"|",if(length(tod2res[[6]])>0) sprintf('%.3f',tod2res[[6]][[2]]) else '     ',"| \n")
      }
      if(indic_MdC %in% c("todos","aciertos")){
        #PPV
        cat("VPP::P(Y^=1|Y=1)       | ",if(length(tod2res[[1]])>0) sprintf('%.3f',tod2res[[1]][[3]]) else '     ',"|",if(length(tod2res[[2]])>0) sprintf('%.3f',tod2res[[2]][[3]]) else '     ',"|",if(length(tod2res[[3]])>0) sprintf('%.3f',tod2res[[3]][[3]]) else '     ',"|",if(length(tod2res[[4]])>0) sprintf('%.3f',tod2res[[4]][[3]]) else '     ',"|",if(length(tod2res[[5]])>0) sprintf('%.3f',tod2res[[5]][[3]]) else '     ',"|",if(length(tod2res[[6]])>0) sprintf('%.3f',tod2res[[6]][[3]]) else '     ',"| \n")
      }
      if(indic_MdC %in% c("todos","aciertos")){
        #NPV
        cat("VPN::P(Y^=0|Y=0)       | ",if(length(tod2res[[1]])>0) sprintf('%.3f',tod2res[[1]][[4]]) else '     ',"|",if(length(tod2res[[2]])>0) sprintf('%.3f',tod2res[[2]][[4]]) else '     ',"|",if(length(tod2res[[3]])>0) sprintf('%.3f',tod2res[[3]][[4]]) else '     ',"|",if(length(tod2res[[4]])>0) sprintf('%.3f',tod2res[[4]][[4]]) else '     ',"|",if(length(tod2res[[5]])>0) sprintf('%.3f',tod2res[[5]][[4]]) else '     ',"|",if(length(tod2res[[6]])>0) sprintf('%.3f',tod2res[[6]][[4]]) else '     ',"| \n")
      }
      if(indic_MdC %in% c("todos","aciertos")){
        #Acc
        cat("Precision::Acc         | ",if(length(tod2res[[1]])>0) sprintf('%.3f',tod2res[[1]][[5]]) else '     ',"|",if(length(tod2res[[2]])>0) sprintf('%.3f',tod2res[[2]][[5]]) else '     ',"|",if(length(tod2res[[3]])>0) sprintf('%.3f',tod2res[[3]][[5]]) else '     ',"|",if(length(tod2res[[4]])>0) sprintf('%.3f',tod2res[[4]][[5]]) else '     ',"|",if(length(tod2res[[5]])>0) sprintf('%.3f',tod2res[[5]][[5]]) else '     ',"|",if(length(tod2res[[6]])>0) sprintf('%.3f',tod2res[[6]][[5]]) else '     ',"| \n")
      }
      if(indic_MdC %in% c("todos","ROC","errores")){
        cat("FalsosPositivos::fpr   | ",if(length(tod2res[[1]])>0) sprintf('%.3f',tod2res[[1]][[6]]) else '     ',"|",if(length(tod2res[[2]])>0) sprintf('%.3f',tod2res[[2]][[6]]) else '     ',"|",if(length(tod2res[[3]])>0) sprintf('%.3f',tod2res[[3]][[6]]) else '     ',"|",if(length(tod2res[[4]])>0) sprintf('%.3f',tod2res[[4]][[6]]) else '     ',"|",if(length(tod2res[[5]])>0) sprintf('%.3f',tod2res[[5]][[6]]) else '     ',"|",if(length(tod2res[[6]])>0) sprintf('%.3f',tod2res[[6]][[6]]) else '     ',"| \n")
      }
      if(indic_MdC %in% c("todos","errores")){
        cat("FalsosDescubr::fdr     | ",if(length(tod2res[[1]])>0) sprintf('%.3f',tod2res[[1]][[7]]) else '     ',"|",if(length(tod2res[[2]])>0) sprintf('%.3f',tod2res[[2]][[7]]) else '     ',"|",if(length(tod2res[[3]])>0) sprintf('%.3f',tod2res[[3]][[7]]) else '     ',"|",if(length(tod2res[[4]])>0) sprintf('%.3f',tod2res[[4]][[7]]) else '     ',"|",if(length(tod2res[[5]])>0) sprintf('%.3f',tod2res[[5]][[7]]) else '     ',"|",if(length(tod2res[[6]])>0) sprintf('%.3f',tod2res[[6]][[7]]) else '     ',"| \n")
      }
      if(indic_MdC %in% c("todos","errores")){
        cat("FalsosNegativos::fnr   | ",if(length(tod2res[[1]])>0) sprintf('%.3f',tod2res[[1]][[8]]) else '     ',"|",if(length(tod2res[[2]])>0) sprintf('%.3f',tod2res[[2]][[8]]) else '     ',"|",if(length(tod2res[[3]])>0) sprintf('%.3f',tod2res[[3]][[8]]) else '     ',"|",if(length(tod2res[[4]])>0) sprintf('%.3f',tod2res[[4]][[8]]) else '     ',"|",if(length(tod2res[[5]])>0) sprintf('%.3f',tod2res[[5]][[8]]) else '     ',"|",if(length(tod2res[[6]])>0) sprintf('%.3f',tod2res[[6]][[8]]) else '     ',"| \n")
      }
      if(indic_MdC %in% c("todos","errores")){
        cat("FalsaOmision::fom      | ",if(length(tod2res[[1]])>0) sprintf('%.3f',tod2res[[1]][[9]]) else '     ',"|",if(length(tod2res[[2]])>0) sprintf('%.3f',tod2res[[2]][[9]]) else '     ',"|",if(length(tod2res[[3]])>0) sprintf('%.3f',tod2res[[3]][[9]]) else '     ',"|",if(length(tod2res[[4]])>0) sprintf('%.3f',tod2res[[4]][[9]]) else '     ',"|",if(length(tod2res[[5]])>0) sprintf('%.3f',tod2res[[5]][[9]]) else '     ',"|",if(length(tod2res[[6]])>0) sprintf('%.3f',tod2res[[6]][[9]]) else '     ',"| \n")
      }
      if(indic_MdC %in% c("todos","errores")){
        cat("ErrorGeneral::erg      | ",if(length(tod2res[[1]])>0) sprintf('%.3f',tod2res[[1]][[10]]) else '     ',"|",if(length(tod2res[[2]])>0) sprintf('%.3f',tod2res[[2]][[10]]) else '     ',"|",if(length(tod2res[[3]])>0) sprintf('%.3f',tod2res[[3]][[10]]) else '     ',"|",if(length(tod2res[[4]])>0) sprintf('%.3f',tod2res[[4]][[10]]) else '     ',"|",if(length(tod2res[[5]])>0) sprintf('%.3f',tod2res[[5]][[10]]) else '     ',"|",if(length(tod2res[[6]])>0) sprintf('%.3f',tod2res[[6]][[10]]) else '     ',"| \n")
      }
      cat("Prevalencia (prv)      | ",if(length(tod2res[[1]])>0) sprintf('%.3f',tod2res[[1]][[12]]) else '     ',"|",if(length(tod2res[[2]])>0) sprintf('%.3f',tod2res[[2]][[12]]) else '     ',"|",if(length(tod2res[[3]])>0) sprintf('%.3f',tod2res[[3]][[12]]) else '     ',"|",if(length(tod2res[[4]])>0) sprintf('%.3f',tod2res[[4]][[12]]) else '     ',"|",if(length(tod2res[[5]])>0) sprintf('%.3f',tod2res[[5]][[12]]) else '     ',"|",if(length(tod2res[[6]])>0) sprintf('%.3f',tod2res[[6]][[12]]) else '     ',"| \n")
      #cat("\n\nCall (formula para todos los modelos del Objeto 2):\n",deparse(obj_2[[nroform1]]$Metadatos$Formula),"\n\n")
    }
    cat('\n\n')
    #>>A FUTURO: exportar data.frames con estas medidas para hacer mas comparaciones (no es tan dificil!)
  }
  else if(length(moc)>0){
    if(!is.null(obj_1)){ #pensar 1ro orden de TODAS las cosas: isnull(nform2) ... , !isnull(...)...
    mimp <- obj_1[[nroform1]]$Metadatos$mimp
    toderp1 <- list("CART"=obj_1[[nroform1]]$ASM$CART$iCART$todos.erp,"Logit"=obj_1[[nroform1]]$ASM$Logit$iLogit$todos.erp,
                   "SVM"=obj_1[[nroform1]]$ASM$SVM$iSVM$todos.erp,"RF"=obj_1[[nroform1]]$ASM$RF$iRF$todos.erp,
                   "Boost"=obj_1[[nroform1]]$ASM$Boost$iBoost$todos.erp,"CDB"=obj_1[[nroform1]]$ASM$CDB$iCDB$todos.erp,
                   "MC_VM"=obj_1[[nroform1]]$ASM$MC_VM$iVM$todos.erp,"MC_MP"=obj_1[[nroform1]]$ASM$MC_MP$iMP$todos.erp,
                   "MC_WA"=obj_1[[nroform1]]$ASM$MC_WA$iWA$todos.erp)
    }
    if(!is.null(obj_2) & is.null(nroform2)){
      toderp2 <- list("CART"=obj_2[[nroform1]]$ASM$CART$iCART$todos.erp,"Logit"=obj_2[[nroform1]]$ASM$Logit$iLogit$todos.erp,
                      "SVM"=obj_2[[nroform1]]$ASM$SVM$iSVM$todos.erp,"RF"=obj_2[[nroform1]]$ASM$RF$iRF$todos.erp,
                      "Boost"=obj_2[[nroform1]]$ASM$Boost$iBoost$todos.erp,"CDB"=obj_2[[nroform1]]$ASM$CDB$iCDB$todos.erp,
                   "MC_VM"=obj_2[[nroform1]]$ASM$MC_VM$iVM$todos.erp,"MC_MP"=obj_2[[nroform1]]$ASM$MC_MP$iMP$todos.erp,
                   "MC_WA"=obj_2[[nroform1]]$ASM$MC_WA$iWA$todos.erp)
    }
    if(is.null(obj_2) & !is.null(nroform2)){
      mimp2 <- obj_1[[nroform2]]$Metadatos$mimp
      toderp2 <- list("CART"=obj_1[[nroform2]]$ASM$CART$iCART$todos.erp,"Logit"=obj_1[[nroform2]]$ASM$Logit$iLogit$todos.erp,
                     "SVM"=obj_1[[nroform2]]$ASM$SVM$iSVM$todos.erp,"RF"=obj_1[[nroform2]]$ASM$RF$iRF$todos.erp,
                     "Boost"=obj_1[[nroform2]]$ASM$Boost$iBoost$todos.erp,"CDB"=obj_1[[nroform2]]$ASM$CDB$iCDB$todos.erp,
                   "MC_VM"=obj_1[[nroform2]]$ASM$MC_VM$iVM$todos.erp,"MC_MP"=obj_1[[nroform2]]$ASM$MC_MP$iMP$todos.erp,
                   "MC_WA"=obj_1[[nroform2]]$ASM$MC_WA$iWA$todos.erp)
    }
    if(!is.null(obj_2) & !is.null(nroform2)){
      mimp2 <- obj_2[[nroform2]]$Metadatos$mimp
      toderp2 <- list("CART"=obj_2[[nroform2]]$ASM$CART$iCART$todos.erp,"Logit"=obj_2[[nroform2]]$ASM$Logit$iLogit$todos.erp,
                     "SVM"=obj_2[[nroform2]]$ASM$SVM$iSVM$todos.erp,"RF"=obj_2[[nroform2]]$ASM$RF$iRF$todos.erp,
                     "Boost"=obj_2[[nroform2]]$ASM$Boost$iBoost$todos.erp,"CDB"=obj_2[[nroform2]]$ASM$CDB$iCDB$todos.erp,
                   "MC_VM"=obj_2[[nroform2]]$ASM$MC_VM$iVM$todos.erp,"MC_MP"=obj_2[[nroform2]]$ASM$MC_MP$iMP$todos.erp,
                   "MC_WA"=obj_2[[nroform2]]$ASM$MC_WA$iWA$todos.erp)
    }
    #Calculos: calculas TODO, devolves lo que usuario pide!!!
    if(!is.null(obj_1) & !is.null(indic_MdC)){#Falta controlar salida: si tenes -1 modelo sale 'subsc out of bounds'
      #todos1_tpr <- list(toderp$CART["sen"],toderp$Logit["sen"],toderp$SVM["sen"],toderp$RF["sen"],toderp$Boost["sen"],toderp$CDB["sen"])
      tod1res <- lapply(toderp1,function(lst) lapply(lst,function(x) mdr_f(t(na.omit(x)),mdres=mdr,dig=3,na.rm=TRUE))) #lapply anidados!
    	tod2res <- NULL
    }
    if(!is.null(toderp2) & !is.null(indic_MdC)){
      tod2res <- lapply(toderp2,function(lst) lapply(lst,function(x) mdr_f(t(na.omit(x)),mdres=mdr,dig=3,na.rm=TRUE))) #lapply anidados!
    }
    #A.Metadatos
    ## 1 solo objeto, 1 formula ### VER DE ACA EN MAS QUE FALTA PARA TERMINAAAARRRRR!!!
    if(!is.null(obj_1) & is.null(obj_2) & is.null(nroform2)){
      cat("\n=================INFORMACION GENERAL: METADATOS==================\n")
      cat("\nModelos simples implementados:\n",deparse(obj_1[[nroform1]]$Metadatos$mimp),"\n")
      cat("\nModelos de consenso implementados:\n",deparse(obj_1[[nroform1]]$Metadatos$moc),"\n")
      cat("\nVariable a predecir (Y):\n",deparse(obj_1[[nroform1]]$Metadatos$Y),"de tipo",deparse(obj_1[[nroform1]]$Metadatos$tipoY),"\n")
      #cat("\nCall (formula para todos los modelos):\n",deparse(obj_1[[nroform1]]$Metadatos$Formula),"\n")
      cat("\nCantidad de permutaciones en {MEnt,MTest}:\n",deparse(obj_1[[nroform1]]$Metadatos$parti),"\n")
      cat("\nProporcion muestra entrenamiento:\n",deparse(100*(obj_1[[nroform1]]$Metadatos$propme)),"%\n")
      cat("\n========COMPARACION: INDICADORES DE MATRICES DE CONFUSION========\n\n")
      cat("\n(Se reporta **",mdr,"** entre las",deparse(obj_1[[nroform1]]$Metadatos$parti),"particiones para estadisticos '",indic_MdC,"')\n")
      cat("\nCall (formula para todos los modelos):\n",deparse(obj_1[[nroform1]]$Metadatos$Formula),"\n\n")
  	}
  	## 2 objetos, 1 sola formula a comparar
    if(!is.null(obj_1) & !is.null(obj_2) & is.null(nroform2)){
  	  cat("\n=================INFORMACION GENERAL: METADATOS==================\n")
      cat("\nModelos simples implementados:\n --",deparse(obj_1[[nroform1]]$Metadatos$mimp),"\n --",deparse(obj_2[[nroform1]]$Metadatos$mimp),"\n")
      cat("\nModelos de consenso implementados:\n --",deparse(obj_1[[nroform1]]$Metadatos$moc),"\n --",deparse(obj_2[[nroform1]]$Metadatos$moc),"\n")
      cat("\nVariable a predecir (Y):\n --",deparse(obj_1[[nroform1]]$Metadatos$Y),"de tipo",deparse(obj_1[[nroform1]]$Metadatos$tipoY),"\n --",deparse(obj_2[[nroform1]]$Metadatos$Y),"de tipo",deparse(obj_2[[nroform1]]$Metadatos$tipoY),"\n")
      cat("\nCantidad de permutaciones en {MEnt,MTest}:\n --",deparse(obj_1[[nroform1]]$Metadatos$parti),"Objeto 1\n --",deparse(obj_2[[nroform1]]$Metadatos$parti),"Objeto 2\n\n")
      cat("\nProporcion muestra entrenamiento:\n --",deparse(100*(obj_1[[nroform1]]$Metadatos$propme)),"% Objeto 1\n --",deparse(100*(obj_2[[nroform1]]$Metadatos$propme)),"% Objeto 2\n")
      cat("\n========COMPARACION: INDICADORES DE MATRICES DE CONFUSION========\n\n")
      cat("\n(Se reporta **",mdr,"** entre las",deparse(obj_1[[nroform1]]$Metadatos$parti),"particiones para estadisticos '",indic_MdC,"')\n")
      cat("\nCall (formula para todos los modelos del Objeto 1):\n",deparse(obj_1[[nroform1]]$Metadatos$Formula),"\n\n")
    }
    ## 1 solo objeto, 2 formulas a comparar FALTAAAAA!!!
    if(!is.null(obj_1) & is.null(obj_2) & !is.null(nroform2)){
  	  cat("\n=================INFORMACION GENERAL: METADATOS==================\n")
      cat("\nModelos simples implementados:\n --",deparse(obj_1[[nroform1]]$Metadatos$mimp),"\n --",deparse(obj_1[[nroform2]]$Metadatos$mimp),"\n")
      cat("\nModelos de consenso implementados:\n --",deparse(obj_1[[nroform1]]$Metadatos$moc),"\n --",deparse(obj_1[[nroform2]]$Metadatos$moc),"\n")
  	  cat("\nVariable a predecir (Y):\n",deparse(obj_1[[nroform1]]$Metadatos$Y),"de tipo",deparse(obj_1[[nroform1]]$Metadatos$tipoY),"\n")
      cat("\nCantidad de permutaciones en {MEnt,MTest}:\n --",deparse(obj_1[[nroform1]]$Metadatos$parti),"Formula 1\n --",deparse(obj_1[[nroform2]]$Metadatos$parti),"Formula 2\n\n")
      cat("\nProporcion muestra entrenamiento:\n --",deparse(100*(obj_1[[nroform1]]$Metadatos$propme)),"% Formula 1\n --",deparse(100*(obj_1[[nroform2]]$Metadatos$propme)),"% Formula 2\n")
      cat("\n========COMPARACION: INDICADORES DE MATRICES DE CONFUSION========\n\n")
      cat("\n(Se reporta **",mdr,"** entre las",deparse(obj_1[[nroform1]]$Metadatos$parti),"particiones para estadisticos '",indic_MdC,"')\n")
      cat("\nCall (formula para todos los modelos de la Formula 1):\n",deparse(obj_1[[nroform1]]$Metadatos$Formula),"\n\n")
      cat("\nCall (formula para todos los modelos de la Formula 2):\n",deparse(obj_1[[nroform2]]$Metadatos$Formula),"\n\n")
      #cat("                       |   CART   |  Logit    |   SVM     |    RF     | Boosting  |   CDB     |  \n")
    }
    ## 2 objetos, 2 formulas a comparar
    if(!is.null(obj_1) & !is.null(obj_2) & !is.null(nroform2)){
      cat("\nModelos simples implementados:\n --",deparse(obj_1[[nroform1]]$Metadatos$mimp),"\n --",deparse(obj_2[[nroform2]]$Metadatos$mimp),"\n")
      cat("\nModelos de consenso implementados:\n --",deparse(obj_1[[nroform1]]$Metadatos$moc),"\n --",deparse(obj_2[[nroform2]]$Metadatos$moc),"\n")
      cat("\nVariable a predecir (Y):\n --",deparse(obj_1[[nroform1]]$Metadatos$Y),"de tipo",deparse(obj_1[[nroform1]]$Metadatos$tipoY),"\n --",deparse(obj_2[[nroform2]]$Metadatos$Y),"de tipo",deparse(obj_2[[nroform2]]$Metadatos$tipoY),"\n")
      cat("\nCantidad de permutaciones en {MEnt,MTest}:\n --",deparse(obj_1[[nroform1]]$Metadatos$parti),"Objeto 1\n --",deparse(obj_2[[nroform2]]$Metadatos$parti),"Objeto 2\n\n")
      cat("\nProporcion muestra entrenamiento:\n --",deparse(100*(obj_1[[nroform1]]$Metadatos$propme)),"% Objeto 1\n --",deparse(100*(obj_2[[nroform2]]$Metadatos$propme)),"% Objeto 2\n")
      cat("\n========COMPARACION: INDICADORES DE MATRICES DE CONFUSION========\n\n")
      cat("\n(Se reporta **",mdr,"** entre las",deparse(obj_1[[nroform1]]$Metadatos$parti),"particiones para estadisticos '",indic_MdC,"')\n")
      cat("\nCall (formula para todos los modelos del Objeto 1):\n",deparse(obj_1[[nroform1]]$Metadatos$Formula),"\n\n")
      cat("\nCall (formula para todos los modelos del Objeto 2):\n",deparse(obj_2[[nroform2]]$Metadatos$Formula),"\n\n")
    }
    #nfo <- if(!is.null(nroform2) & !is.null(obj_2)) 1 else ""
    nfo <- switch(caso,`1`="",`2`=1,`3`=1,`4`=1)
    if(!is.null(obj_1)) #exceso de ifs (pensar opcion '&&'; si ya hay stop() antes no seguir preguntando)
  	  cat("Formula ",nfo,"   :",deparse(obj_1[[nroform1]]$Metadatos$Formula),"\n\n")
  	cat("                 |  CART | Logit |  SVM  |   RF  |Boostng|  CDB  | MC_VM | MC_MP | MC_WA |\n")
  	#cat("                 | 12CART78 | 1Logit78  | 12SVM678  | 123RF678  |Boosting| 12CDB678  |  \n")
    #B.Tablas con salidas
    if(indic_MdC %in% c("todos","ROC","aciertos")){
      #sen
      cat("Sen::P(Y=1|Y^=1) |", if(length(tod1res[[1]])>0) sprintf('%.3f',tod1res[[1]][[1]]) else '     ',"|",if(length(tod1res[[2]])>0) sprintf('%.3f',tod1res[[2]][[1]]) else '     ',"|",if(length(tod1res[[3]])>0) sprintf('%.3f',tod1res[[3]][[1]]) else '     ',"|",if(length(tod1res[[4]])>0) sprintf('%.3f',tod1res[[4]][[1]]) else '     ',"|",if(length(tod1res[[5]])>0) sprintf('%.3f',tod1res[[5]][[1]]) else '     ',"|",if(length(tod1res[[6]])>0) sprintf('%.3f',tod1res[[6]][[1]]) else '     ',"|",if(length(tod1res[[7]])>0) sprintf('%.3f',tod1res[[7]][[1]]) else '     ',"|",if(length(tod1res[[8]])>0) sprintf('%.3f',tod1res[[8]][[1]]) else '     ',"|",if(length(tod1res[[9]])>0) sprintf('%.3f',tod1res[[9]][[1]]) else '     ',"|\n")
     }
    if(indic_MdC %in% c("todos","aciertos")){
      #esp
      cat("Esp::P(Y=0|Y^=0) |", if(length(tod1res[[1]])>0) sprintf('%.3f',tod1res[[1]][[2]]) else '     ',"|",if(length(tod1res[[2]])>0) sprintf('%.3f',tod1res[[2]][[2]]) else '     ',"|",if(length(tod1res[[3]])>0) sprintf('%.3f',tod1res[[3]][[2]]) else '     ',"|",if(length(tod1res[[4]])>0) sprintf('%.3f',tod1res[[4]][[2]]) else '     ',"|",if(length(tod1res[[5]])>0) sprintf('%.3f',tod1res[[5]][[2]]) else '     ',"|",if(length(tod1res[[6]])>0) sprintf('%.3f',tod1res[[6]][[2]]) else '     ',"|",if(length(tod1res[[7]])>0) sprintf('%.3f',tod1res[[7]][[2]]) else '     ',"|",if(length(tod1res[[8]])>0) sprintf('%.3f',tod1res[[8]][[2]]) else '     ',"|",if(length(tod1res[[9]])>0) sprintf('%.3f',tod1res[[9]][[2]]) else '     ',"|\n")
      }
    if(indic_MdC %in% c("todos","aciertos")){
      #PPV
      cat("VPP::P(Y^=1|Y=1) |", if(length(tod1res[[1]])>0) sprintf('%.3f',tod1res[[1]][[3]]) else '     ',"|",if(length(tod1res[[2]])>0) sprintf('%.3f',tod1res[[2]][[3]]) else '     ',"|",if(length(tod1res[[3]])>0) sprintf('%.3f',tod1res[[3]][[3]]) else '     ',"|",if(length(tod1res[[4]])>0) sprintf('%.3f',tod1res[[4]][[3]]) else '     ',"|",if(length(tod1res[[5]])>0) sprintf('%.3f',tod1res[[5]][[3]]) else '     ',"|",if(length(tod1res[[6]])>0) sprintf('%.3f',tod1res[[6]][[3]]) else '     ',"|",if(length(tod1res[[7]])>0) sprintf('%.3f',tod1res[[7]][[3]]) else '     ',"|",if(length(tod1res[[8]])>0) sprintf('%.3f',tod1res[[8]][[3]]) else '     ',"|",if(length(tod1res[[9]])>0) sprintf('%.3f',tod1res[[9]][[3]]) else '     ',"|\n")
      }
    if(indic_MdC %in% c("todos","aciertos")){
      #NPV
      cat("VPN::P(Y^=0|Y=0) |", if(length(tod1res[[1]])>0) sprintf('%.3f',tod1res[[1]][[4]]) else '     ',"|",if(length(tod1res[[2]])>0) sprintf('%.3f',tod1res[[2]][[4]]) else '     ',"|",if(length(tod1res[[3]])>0) sprintf('%.3f',tod1res[[3]][[4]]) else '     ',"|",if(length(tod1res[[4]])>0) sprintf('%.3f',tod1res[[4]][[4]]) else '     ',"|",if(length(tod1res[[5]])>0) sprintf('%.3f',tod1res[[5]][[4]]) else '     ',"|",if(length(tod1res[[6]])>0) sprintf('%.3f',tod1res[[6]][[4]]) else '     ',"|",if(length(tod1res[[7]])>0) sprintf('%.3f',tod1res[[7]][[4]]) else '     ',"|",if(length(tod1res[[8]])>0) sprintf('%.3f',tod1res[[8]][[4]]) else '     ',"|",if(length(tod1res[[9]])>0) sprintf('%.3f',tod1res[[9]][[4]]) else '     ',"|\n")
      }
    if(indic_MdC %in% c("todos","aciertos")){
      #Acc
      cat("Precision::Acc   |", if(length(tod1res[[1]])>0) sprintf('%.3f',tod1res[[1]][[5]]) else '     ',"|",if(length(tod1res[[2]])>0) sprintf('%.3f',tod1res[[2]][[5]]) else '     ',"|",if(length(tod1res[[3]])>0) sprintf('%.3f',tod1res[[3]][[5]]) else '     ',"|",if(length(tod1res[[4]])>0) sprintf('%.3f',tod1res[[4]][[5]]) else '     ',"|",if(length(tod1res[[5]])>0) sprintf('%.3f',tod1res[[5]][[5]]) else '     ',"|",if(length(tod1res[[6]])>0) sprintf('%.3f',tod1res[[6]][[5]]) else '     ',"|",if(length(tod1res[[7]])>0) sprintf('%.3f',tod1res[[7]][[5]]) else '     ',"|",if(length(tod1res[[8]])>0) sprintf('%.3f',tod1res[[8]][[5]]) else '     ',"|",if(length(tod1res[[9]])>0) sprintf('%.3f',tod1res[[9]][[5]]) else '     ',"|\n")
      }
    if(indic_MdC %in% c("todos","ROC","errores")){
      cat("FalsosPosit::fpr |", if(length(tod1res[[1]])>0) sprintf('%.3f',tod1res[[1]][[6]]) else '     ',"|",if(length(tod1res[[2]])>0) sprintf('%.3f',tod1res[[2]][[6]]) else '     ',"|",if(length(tod1res[[3]])>0) sprintf('%.3f',tod1res[[3]][[6]]) else '     ',"|",if(length(tod1res[[4]])>0) sprintf('%.3f',tod1res[[4]][[6]]) else '     ',"|",if(length(tod1res[[5]])>0) sprintf('%.3f',tod1res[[5]][[6]]) else '     ',"|",if(length(tod1res[[6]])>0) sprintf('%.3f',tod1res[[6]][[6]]) else '     ',"|",if(length(tod1res[[7]])>0) sprintf('%.3f',tod1res[[7]][[6]]) else '     ',"|",if(length(tod1res[[8]])>0) sprintf('%.3f',tod1res[[8]][[6]]) else '     ',"|",if(length(tod1res[[9]])>0) sprintf('%.3f',tod1res[[9]][[6]]) else '     ',"|\n")
      }
    if(indic_MdC %in% c("todos","errores")){
      cat("FalsosDescu::fdr |", if(length(tod1res[[1]])>0) sprintf('%.3f',tod1res[[1]][[7]]) else '     ',"|",if(length(tod1res[[2]])>0) sprintf('%.3f',tod1res[[2]][[7]]) else '     ',"|",if(length(tod1res[[3]])>0) sprintf('%.3f',tod1res[[3]][[7]]) else '     ',"|",if(length(tod1res[[4]])>0) sprintf('%.3f',tod1res[[4]][[7]]) else '     ',"|",if(length(tod1res[[5]])>0) sprintf('%.3f',tod1res[[5]][[7]]) else '     ',"|",if(length(tod1res[[6]])>0) sprintf('%.3f',tod1res[[6]][[7]]) else '     ',"|",if(length(tod1res[[7]])>0) sprintf('%.3f',tod1res[[7]][[7]]) else '     ',"|",if(length(tod1res[[8]])>0) sprintf('%.3f',tod1res[[8]][[7]]) else '     ',"|",if(length(tod1res[[9]])>0) sprintf('%.3f',tod1res[[9]][[7]]) else '     ',"|\n")
     }
    if(indic_MdC %in% c("todos","errores")){
      cat("FalsosNegat::fnr |", if(length(tod1res[[1]])>0) sprintf('%.3f',tod1res[[1]][[8]]) else '     ',"|",if(length(tod1res[[2]])>0) sprintf('%.3f',tod1res[[2]][[8]]) else '     ',"|",if(length(tod1res[[3]])>0) sprintf('%.3f',tod1res[[3]][[8]]) else '     ',"|",if(length(tod1res[[4]])>0) sprintf('%.3f',tod1res[[4]][[8]]) else '     ',"|",if(length(tod1res[[5]])>0) sprintf('%.3f',tod1res[[5]][[8]]) else '     ',"|",if(length(tod1res[[6]])>0) sprintf('%.3f',tod1res[[6]][[8]]) else '     ',"|",if(length(tod1res[[7]])>0) sprintf('%.3f',tod1res[[7]][[8]]) else '     ',"|",if(length(tod1res[[8]])>0) sprintf('%.3f',tod1res[[8]][[8]]) else '     ',"|",if(length(tod1res[[9]])>0) sprintf('%.3f',tod1res[[9]][[8]]) else '     ',"|\n")
      }
    if(indic_MdC %in% c("todos","errores")){
      cat("FalsaOmis::fom   |", if(length(tod1res[[1]])>0) sprintf('%.3f',tod1res[[1]][[9]]) else '     ',"|",if(length(tod1res[[2]])>0) sprintf('%.3f',tod1res[[2]][[9]]) else '     ',"|",if(length(tod1res[[3]])>0) sprintf('%.3f',tod1res[[3]][[9]]) else '     ',"|",if(length(tod1res[[4]])>0) sprintf('%.3f',tod1res[[4]][[9]]) else '     ',"|",if(length(tod1res[[5]])>0) sprintf('%.3f',tod1res[[5]][[9]]) else '     ',"|",if(length(tod1res[[6]])>0) sprintf('%.3f',tod1res[[6]][[9]]) else '     ',"|",if(length(tod1res[[7]])>0) sprintf('%.3f',tod1res[[7]][[9]]) else '     ',"|",if(length(tod1res[[8]])>0) sprintf('%.3f',tod1res[[8]][[9]]) else '     ',"|",if(length(tod1res[[9]])>0) sprintf('%.3f',tod1res[[9]][[9]]) else '     ',"|\n")
      }
    if(indic_MdC %in% c("todos","errores")){
      cat("ErrorGral::erg   |", if(length(tod1res[[1]])>0) sprintf('%.3f',tod1res[[1]][[10]]) else '     ',"|",if(length(tod1res[[2]])>0) sprintf('%.3f',tod1res[[2]][[10]]) else '     ',"|",if(length(tod1res[[3]])>0) sprintf('%.3f',tod1res[[3]][[10]]) else '     ',"|",if(length(tod1res[[4]])>0) sprintf('%.3f',tod1res[[4]][[10]]) else '     ',"|",if(length(tod1res[[5]])>0) sprintf('%.3f',tod1res[[5]][[10]]) else '     ',"|",if(length(tod1res[[6]])>0) sprintf('%.3f',tod1res[[6]][[10]]) else '     ',"|",if(length(tod1res[[7]])>0) sprintf('%.3f',tod1res[[7]][[10]]) else '     ',"|",if(length(tod1res[[8]])>0) sprintf('%.3f',tod1res[[8]][[10]]) else '     ',"|",if(length(tod1res[[9]])>0) sprintf('%.3f',tod1res[[9]][[10]]) else '     ',"|\n")
      }
    cat("Prevalencia (prv)|", if(length(tod1res[[1]])>0) sprintf('%.3f',tod1res[[1]][[12]]) else '     ',"|",if(length(tod1res[[2]])>0) sprintf('%.3f',tod1res[[2]][[12]]) else '     ',"|",if(length(tod1res[[3]])>0) sprintf('%.3f',tod1res[[3]][[12]]) else '     ',"|",if(length(tod1res[[4]])>0) sprintf('%.3f',tod1res[[4]][[12]]) else '     ',"|",if(length(tod1res[[5]])>0) sprintf('%.3f',tod1res[[5]][[12]]) else '     ',"|",if(length(tod1res[[6]])>0) sprintf('%.3f',tod1res[[6]][[12]]) else '     ',"|",if(length(tod1res[[7]])>0) sprintf('%.3f',tod1res[[7]][[12]]) else '     ',"|",if(length(tod1res[[8]])>0) sprintf('%.3f',tod1res[[8]][[12]]) else '     ',"|",if(length(tod1res[[9]])>0) sprintf('%.3f',tod1res[[9]][[12]]) else '     ',"|\n")
    cat('\n\n')
    if(!is.null(obj_1) & !is.null(tod2res)){
      cat("Formula ",nfo+1,"   :")
  	if(caso==2)
  	  cat(deparse(obj_1[[nroform2]]$Metadatos$Formula),"\n\n")
  	else if(caso==3)
  	  cat(deparse(obj_2[[nroform1]]$Metadatos$Formula),"\n\n")
  	else if(caso==4)
  	  cat(deparse(obj_2[[nroform2]]$Metadatos$Formula),"\n\n")
  	cat("                 |  CART | Logit |  SVM  |   RF  |Boostng|  CDB  | MC_VM | MC_MP | MC_WA |\n")
      if(indic_MdC %in% c("todos","ROC","aciertos")){#CAMBIAR tod2res como tod1res!!!
        #sen
        #cat("                       | 12CART78 | 1Logit78  | 12SVM678  | 123RF678  | Boosting  | 12CDB678  |  \n")
        cat("Sen::P(Y=1|Y^=1) |",if(length(tod2res[[1]])>0) sprintf('%.3f',tod2res[[1]][[1]]) else '     ',"|",if(length(tod2res[[2]])>0) sprintf('%.3f',tod2res[[2]][[1]]) else '     ',"|",if(length(tod2res[[3]])>0) sprintf('%.3f',tod2res[[3]][[1]]) else '     ',"|",if(length(tod2res[[4]])>0) sprintf('%.3f',tod2res[[4]][[1]]) else '     ',"|",if(length(tod2res[[5]])>0) sprintf('%.3f',tod2res[[5]][[1]]) else '     ',"|",if(length(tod2res[[6]])>0) sprintf('%.3f',tod2res[[6]][[1]]) else '     ',"|",if(length(tod2res[[7]])>0) sprintf('%.3f',tod2res[[7]][[1]]) else '     ',"|",if(length(tod2res[[8]])>0) sprintf('%.3f',tod2res[[8]][[1]]) else '     ',"|",if(length(tod2res[[9]])>0) sprintf('%.3f',tod2res[[9]][[1]]) else '     ',"|\n")
      }
      if(indic_MdC %in% c("todos","aciertos")){
        #esp
        cat("Esp::P(Y=0|Y^=0) |",if(length(tod2res[[1]])>0) sprintf('%.3f',tod2res[[1]][[2]]) else '     ',"|",if(length(tod2res[[2]])>0) sprintf('%.3f',tod2res[[2]][[2]]) else '     ',"|",if(length(tod2res[[3]])>0) sprintf('%.3f',tod2res[[3]][[2]]) else '     ',"|",if(length(tod2res[[4]])>0) sprintf('%.3f',tod2res[[4]][[2]]) else '     ',"|",if(length(tod2res[[5]])>0) sprintf('%.3f',tod2res[[5]][[2]]) else '     ',"|",if(length(tod2res[[6]])>0) sprintf('%.3f',tod2res[[6]][[2]]) else '     ',"|",if(length(tod2res[[7]])>0) sprintf('%.3f',tod2res[[7]][[2]]) else '     ',"|",if(length(tod2res[[8]])>0) sprintf('%.3f',tod2res[[8]][[2]]) else '     ',"|",if(length(tod2res[[9]])>0) sprintf('%.3f',tod2res[[9]][[2]]) else '     ',"|\n")
      }
      if(indic_MdC %in% c("todos","aciertos")){
        #PPV
        cat("VPP::P(Y^=1|Y=1) |",if(length(tod2res[[1]])>0) sprintf('%.3f',tod2res[[1]][[3]]) else '     ',"|",if(length(tod2res[[2]])>0) sprintf('%.3f',tod2res[[2]][[3]]) else '     ',"|",if(length(tod2res[[3]])>0) sprintf('%.3f',tod2res[[3]][[3]]) else '     ',"|",if(length(tod2res[[4]])>0) sprintf('%.3f',tod2res[[4]][[3]]) else '     ',"|",if(length(tod2res[[5]])>0) sprintf('%.3f',tod2res[[5]][[3]]) else '     ',"|",if(length(tod2res[[6]])>0) sprintf('%.3f',tod2res[[6]][[3]]) else '     ',"|",if(length(tod2res[[7]])>0) sprintf('%.3f',tod2res[[7]][[3]]) else '     ',"|",if(length(tod2res[[8]])>0) sprintf('%.3f',tod2res[[8]][[3]]) else '     ',"|",if(length(tod2res[[9]])>0) sprintf('%.3f',tod2res[[9]][[3]]) else '     ',"|\n")
      }
      if(indic_MdC %in% c("todos","aciertos")){
        #NPV
        cat("VPN::P(Y^=0|Y=0) |",if(length(tod2res[[1]])>0) sprintf('%.3f',tod2res[[1]][[4]]) else '     ',"|",if(length(tod2res[[2]])>0) sprintf('%.3f',tod2res[[2]][[4]]) else '     ',"|",if(length(tod2res[[3]])>0) sprintf('%.3f',tod2res[[3]][[4]]) else '     ',"|",if(length(tod2res[[4]])>0) sprintf('%.3f',tod2res[[4]][[4]]) else '     ',"|",if(length(tod2res[[5]])>0) sprintf('%.3f',tod2res[[5]][[4]]) else '     ',"|",if(length(tod2res[[6]])>0) sprintf('%.3f',tod2res[[6]][[4]]) else '     ',"|",if(length(tod2res[[7]])>0) sprintf('%.3f',tod2res[[7]][[4]]) else '     ',"|",if(length(tod2res[[8]])>0) sprintf('%.3f',tod2res[[8]][[4]]) else '     ',"|",if(length(tod2res[[9]])>0) sprintf('%.3f',tod2res[[9]][[4]]) else '     ',"|\n")
      }
      if(indic_MdC %in% c("todos","aciertos")){
        #Acc
        cat("Precision::Acc   |",if(length(tod2res[[1]])>0) sprintf('%.3f',tod2res[[1]][[5]]) else '     ',"|",if(length(tod2res[[2]])>0) sprintf('%.3f',tod2res[[2]][[5]]) else '     ',"|",if(length(tod2res[[3]])>0) sprintf('%.3f',tod2res[[3]][[5]]) else '     ',"|",if(length(tod2res[[4]])>0) sprintf('%.3f',tod2res[[4]][[5]]) else '     ',"|",if(length(tod2res[[5]])>0) sprintf('%.3f',tod2res[[5]][[5]]) else '     ',"|",if(length(tod2res[[6]])>0) sprintf('%.3f',tod2res[[6]][[5]]) else '     ',"|",if(length(tod2res[[7]])>0) sprintf('%.3f',tod2res[[7]][[5]]) else '     ',"|",if(length(tod2res[[8]])>0) sprintf('%.3f',tod2res[[8]][[5]]) else '     ',"|",if(length(tod2res[[9]])>0) sprintf('%.3f',tod2res[[9]][[5]]) else '     ',"|\n")
      }
      if(indic_MdC %in% c("todos","ROC","errores")){
        cat("FalsosPosit::fpr |",if(length(tod2res[[1]])>0) sprintf('%.3f',tod2res[[1]][[6]]) else '     ',"|",if(length(tod2res[[2]])>0) sprintf('%.3f',tod2res[[2]][[6]]) else '     ',"|",if(length(tod2res[[3]])>0) sprintf('%.3f',tod2res[[3]][[6]]) else '     ',"|",if(length(tod2res[[4]])>0) sprintf('%.3f',tod2res[[4]][[6]]) else '     ',"|",if(length(tod2res[[5]])>0) sprintf('%.3f',tod2res[[5]][[6]]) else '     ',"|",if(length(tod2res[[6]])>0) sprintf('%.3f',tod2res[[6]][[6]]) else '     ',"|",if(length(tod2res[[7]])>0) sprintf('%.3f',tod2res[[7]][[6]]) else '     ',"|",if(length(tod2res[[8]])>0) sprintf('%.3f',tod2res[[8]][[6]]) else '     ',"|",if(length(tod2res[[9]])>0) sprintf('%.3f',tod2res[[9]][[6]]) else '     ',"|\n")
      }
      if(indic_MdC %in% c("todos","errores")){
        cat("FalsosDescu::fdr |",if(length(tod2res[[1]])>0) sprintf('%.3f',tod2res[[1]][[7]]) else '     ',"|",if(length(tod2res[[2]])>0) sprintf('%.3f',tod2res[[2]][[7]]) else '     ',"|",if(length(tod2res[[3]])>0) sprintf('%.3f',tod2res[[3]][[7]]) else '     ',"|",if(length(tod2res[[4]])>0) sprintf('%.3f',tod2res[[4]][[7]]) else '     ',"|",if(length(tod2res[[5]])>0) sprintf('%.3f',tod2res[[5]][[7]]) else '     ',"|",if(length(tod2res[[6]])>0) sprintf('%.3f',tod2res[[6]][[7]]) else '     ',"|",if(length(tod2res[[7]])>0) sprintf('%.3f',tod2res[[7]][[7]]) else '     ',"|",if(length(tod2res[[8]])>0) sprintf('%.3f',tod2res[[8]][[7]]) else '     ',"|",if(length(tod2res[[9]])>0) sprintf('%.3f',tod2res[[9]][[7]]) else '     ',"|\n")
      }
      if(indic_MdC %in% c("todos","errores")){
        cat("FalsosNegat::fnr |",if(length(tod2res[[1]])>0) sprintf('%.3f',tod2res[[1]][[8]]) else '     ',"|",if(length(tod2res[[2]])>0) sprintf('%.3f',tod2res[[2]][[8]]) else '     ',"|",if(length(tod2res[[3]])>0) sprintf('%.3f',tod2res[[3]][[8]]) else '     ',"|",if(length(tod2res[[4]])>0) sprintf('%.3f',tod2res[[4]][[8]]) else '     ',"|",if(length(tod2res[[5]])>0) sprintf('%.3f',tod2res[[5]][[8]]) else '     ',"|",if(length(tod2res[[6]])>0) sprintf('%.3f',tod2res[[6]][[8]]) else '     ',"|",if(length(tod2res[[7]])>0) sprintf('%.3f',tod2res[[7]][[8]]) else '     ',"|",if(length(tod2res[[8]])>0) sprintf('%.3f',tod2res[[8]][[8]]) else '     ',"|",if(length(tod2res[[9]])>0) sprintf('%.3f',tod2res[[9]][[8]]) else '     ',"|\n")
      }
      if(indic_MdC %in% c("todos","errores")){
        cat("FalsaOmis::fom   |",if(length(tod2res[[1]])>0) sprintf('%.3f',tod2res[[1]][[9]]) else '     ',"|",if(length(tod2res[[2]])>0) sprintf('%.3f',tod2res[[2]][[9]]) else '     ',"|",if(length(tod2res[[3]])>0) sprintf('%.3f',tod2res[[3]][[9]]) else '     ',"|",if(length(tod2res[[4]])>0) sprintf('%.3f',tod2res[[4]][[9]]) else '     ',"|",if(length(tod2res[[5]])>0) sprintf('%.3f',tod2res[[5]][[9]]) else '     ',"|",if(length(tod2res[[6]])>0) sprintf('%.3f',tod2res[[6]][[9]]) else '     ',"|",if(length(tod2res[[7]])>0) sprintf('%.3f',tod2res[[7]][[9]]) else '     ',"|",if(length(tod2res[[8]])>0) sprintf('%.3f',tod2res[[8]][[9]]) else '     ',"|",if(length(tod2res[[9]])>0) sprintf('%.3f',tod2res[[9]][[9]]) else '     ',"|\n")
      }
      if(indic_MdC %in% c("todos","errores")){
        cat("ErrorGral::erg   |",if(length(tod2res[[1]])>0) sprintf('%.3f',tod2res[[1]][[10]]) else '     ',"|",if(length(tod2res[[2]])>0) sprintf('%.3f',tod2res[[2]][[10]]) else '     ',"|",if(length(tod2res[[3]])>0) sprintf('%.3f',tod2res[[3]][[10]]) else '     ',"|",if(length(tod2res[[4]])>0) sprintf('%.3f',tod2res[[4]][[10]]) else '     ',"|",if(length(tod2res[[5]])>0) sprintf('%.3f',tod2res[[5]][[10]]) else '     ',"|",if(length(tod2res[[6]])>0) sprintf('%.3f',tod2res[[6]][[10]]) else '     ',"|",if(length(tod2res[[7]])>0) sprintf('%.3f',tod2res[[7]][[10]]) else '     ',"|",if(length(tod2res[[8]])>0) sprintf('%.3f',tod2res[[8]][[10]]) else '     ',"|",if(length(tod2res[[9]])>0) sprintf('%.3f',tod2res[[9]][[10]]) else '     ',"|\n")
      }
      cat("Prevalencia (prv)|",if(length(tod2res[[1]])>0) sprintf('%.3f',tod2res[[1]][[12]]) else '     ',"|",if(length(tod2res[[2]])>0) sprintf('%.3f',tod2res[[2]][[12]]) else '     ',"|",if(length(tod2res[[3]])>0) sprintf('%.3f',tod2res[[3]][[12]]) else '     ',"|",if(length(tod2res[[4]])>0) sprintf('%.3f',tod2res[[4]][[12]]) else '     ',"|",if(length(tod2res[[5]])>0) sprintf('%.3f',tod2res[[5]][[12]]) else '     ',"|",if(length(tod2res[[6]])>0) sprintf('%.3f',tod2res[[6]][[12]]) else '     ',"|",if(length(tod2res[[7]])>0) sprintf('%.3f',tod2res[[7]][[12]]) else '     ',"|",if(length(tod2res[[8]])>0) sprintf('%.3f',tod2res[[8]][[12]]) else '     ',"|",if(length(tod2res[[9]])>0) sprintf('%.3f',tod2res[[9]][[12]]) else '     ',"|\n")
      #cat("\n\nCall (formula para todos los modelos del Objeto 2):\n",deparse(obj_2[[nroform1]]$Metadatos$Formula),"\n\n")
    }
    cat('\n\n')
    #>>A FUTURO: exportar data.frames con estas medidas para hacer mas comparaciones (no es tan dificil!)
  }
}

############################################################

#' ASM: Ajuste Simultaneo Modelos
#'
#' Metodo 'print' para objetos de clase 'asm' (basico, completar)
#' #@param x Objeto de clase 'asm'
#' #@param sign (no funca x ahora)
#' #@param ...
#' #@keywords print.asm
#' #@export
#' #@return
#' #@seealso
#' #@examples
#' #print.asm()


