############################################################
###### TESIS DA [Reunion 71]: wrapper general modelos ######
############################################################

#' ASM: Ajuste Simultaneo Modelos
#'
#' wrapper para ajuste simultaneo modelos ML
#'
#' @importFrom stats glm
#' @importFrom MASS stepAIC
#' @import rpart
#' @import randomForest
#' @import e1071
#' @import adabag
#'
#' @export
#' @param frml #gral
#' @param datFULL #gral
#' @param columnaY #gral
#' @param modelosML #gral
#' @param parti #gral
#' @param propme #gral
#' @param semillas #gral
#' @param metadatos #gral
#' @param error.resum #gral
#' @param cart.ctrl #cart
#' @param lgt.stepaic #glm-logit
#' @param costsvm #svm
#' @param kernel #svm
#' @param svm.gamma #svm
#' @param ntree #rf
#' @param mtry #rf
#' @param b.mfinal #boosting
#' @param uso.em #cdb-em
#' @param em.alfa10 #cdb-em
#' @param em.iter #cdb-em
#' @param em.tol #cdb-em
#' @param cdb.dens #cdb-gral
#' @param predicts.out #nuevo: controla salida DFs con Ypred x modelo
#' @param ... parametros adicionales para modelos ML
#' @keywords asm
#' @return objeto S3 de clase 'asm'
#' #@seealso gsub, grep
#' #@examples
#' asm2()

asm2 <- function(frml,datFULL,columnaY,modelosML,parti=10,propme=0.75,predicts.out=FALSE,
    semillas=NULL,metadatos=NULL,error.resum=TRUE,moc=c("vm","mp","waauc"),
    cart.ctrl=rpart::rpart.control(cp=.01), #CART
    lgt.stepaic=FALSE, #logit
    costsvm=1,kernel="radial",svm.gamma=0.5, #SVM
    ntree=100, mtry=2, #RF
    b.mfinal=100, #Boost
    uso.em=TRUE,em.alfa10=0.5,em.iter=100,em.tol=1e-5,cdb.dens="density",...){
  ###1.Modelos "usuales"
  init <- Sys.time()
  mimp <- c("CART","Logit","SVM","RF","Boosting","CDB")
  mimp <- mimp[sort(match(x=modelosML,table=mimp))]
  parti <- 1:parti
  fo <- as.formula(frml)
  largo <- length(attr(terms(fo),"term.labels"))
  if(!is.null(datFULL))
    tipoY <- class(datFULL[,columnaY])
  else
    tipoY <- class(datT[,columnaY])
  nivY <- levels(as.factor(datFULL[,columnaY]))
  dpg <- FALSE
  if(sum(sapply(datFULL,function(x) sum(is.na(x)),simplify=TRUE))>0){  #if(missingdata)
    datFULL <- model.frame(formula=fo,data=datFULL)
    columnaY <- 1
    dpg <- TRUE
  }
  ##Funcion ml_dfs v2 (v1 hasta 28/05/18)
  ml_dfs <- function(dat,mod,tipo,i,rownm=FALSE){
    pms <- c("pm.cart.df","pm.logit.df","pm.svm.df","pm.rf.df","pm.boost.df","pm.cdb.df","pmc.cart.df","pmc.logit.df","pmc.rf.df","pmc.boost.df","pmc.svm.df","pmc.cdb.df")
    ml <- unique(gsub(pattern="([a-z]+\\.)([a-z]+)(\\.[a-z]+)",replacement="\\2",x=pms))
    ml_dfs <- strtrim(tolower(mimp[sort(match(x=mod,table=ml))]),5) #asi evitamos problemas con mimp::Boosting
    modelo <- paste0("Yp_",ml_dfs,collapse="")
    if(paste0(tipo,".",ml_dfs,collapse="") %in% c("pm.cart","pm.rf","pm.cdb")){
      #1.pm: cart/rf, cdb OK -- cambiamo condicion 060618 x arbol c/nodo raiz >> attr(names)=NULL
      if(is.null(attr(dat[[i]],which="names")))
        out <- try(data.frame(id = rownm, modelo = dat[[i]]),silent = TRUE)
      else
        out <- try(data.frame("id"=attr(dat[[i]],which="names"),modelo=dat[[i]]),silent=TRUE)
    } else if(paste0(tipo,".",ml_dfs,collapse="") %in% c("pm.svm","pm.logit","pm.svm2","pm.logit2")){
      #2.pm: svm logit
      out <- try(dat,silent=TRUE)
    } else if(paste0(tipo,".",ml_dfs,collapse="") %in% "pm.boost"){
      #3.pm boost -- CAMBIAR
      out <- try(data.frame("id"=rownm,modelo=dat[[i]]$class),silent=TRUE)
    } else if(paste0(tipo,".",ml_dfs,collapse="") %in% c("pmc.cart","pmc.rf")){
      #4.pmc: cart/rf OK
      if(is.null(attr(dat[[i]],which="dimnames")[[1]]))
        out <- try(data.frame(id=rownm, modelo=dat[[i]]),silent=TRUE)
      else
        out <- try(data.frame("id"=attr(dat[[i]],which="dimnames")[[1]],modelo=dat[[i]]),silent=TRUE)
    } else if(paste0(tipo,".",ml_dfs,collapse="") %in% "pmc.logit") {
      #5.pmc logit -- OK
      out <- try(data.frame("id"=attr(dat,which="row.names"),modelo=dat),silent=TRUE)
    } else if(paste0(tipo,".",ml_dfs,collapse="") %in% "pmc.svm") {
      #6.pmc svm: attr(pm.svm[[i]],"probabilities")) OK
      out <- try(data.frame("id"=attr(dat,which="names"),attr(dat,which="probabilities")),silent=TRUE)
    } else if(paste0(tipo,".",ml_dfs,collapse="") %in% "pmc.boost"){
      #7.pmc boost -- OK
      out <- try(data.frame("id"=rownm,modelo=dat[[i]]$prob),silent=TRUE)
      names(out)[2:3] <- c("Boost_0","Boost_1")
    } else {
      out <- try(data.frame("id"=attr(dat[[i]],which="names"),modelo=dat[[i]]),silent=TRUE)
    }
    if(is.data.frame(out) && ncol(out)==2) #usar cortocircuito xq sino devuelve logical(0)
      names(out)[2] <- modelo
    #B:revisar (ej if(dim=NULL)...) #ver si es pertinente; creo que alcanza con C
    #C:tryCatch a A/B
    out <- tryCatch(out,
        error=function(cond){
          message("Error, va mensaje R")
          message(cond)
          return(NA)
        },
        warning=function(cond){
          message("Advertencia, revisar")
          message(cond)
          return(NULL)
        },
        finally=message(paste0(" -- ",ml_dfs," iteracion :",i)))
    return(out)
  }
  #a <- cart_dfs(pm.cart[[i]],i); aa <- cart_dfs(pmc.cart[[i]],i)
  trf <- tbag <- t.mlg1 <- tsvm <- tnb <- tcb <- tcart <- papl <- perme <- permt <- em.alf <- m.logit.sa <-
   tboost <- pm.cart <- pm.logit2 <- pm.svm2 <- pm.rf <- pm.boost <- pm.cdb <- comp.vs.Yobs <- dfpmlg <-
    pmc.cart <- pmc.rf <- comp.vs.papc <- mg <- vml <- mpl <- waul <- vector(mode="list",length=max(parti))
  for(i in parti){
    if(!is.null(semillas)) set.seed(semillas[i])
    perm_df <- uefi2:::mute(df=datFULL,nroperm=1,pme=propme)
    datE <- perm_df$dat_me[[1]]
    datT <- perm_df$dat_mt[[1]]
    perme[[i]] <- perm_df$indME
    permt[[i]] <- perm_df$rnMT #row.names, no indices ordenados como hasta ahora
    nf.dT <- row.names(datT) #cambio 060618 para contemplar casos CART en ml_dfs()
    if("CART" %in% modelosML){
      #requireNamespace("rpart",quietly=TRUE)
      m.cart <- rpart::rpart(fo,data=datE,method="class",control=cart.ctrl)
      pm.cart[[i]] <- predict(m.cart,newdata=if(dpg) datT else datT[,-columnaY],type="class")
	    pmc.cart[[i]] <- predict(m.cart,newdata=if(dpg) datT else datT[,-columnaY],type="prob")
      tcart[[i]] <- table(datT[,columnaY],factor(pm.cart[[i]],levels=nivY),dnn=c("YObs","YPred"))
      t.cart <- mdcmed(tcart[[i]])
	  if(predicts.out){
	    pm.cart.df <- ml_dfs(dat=pm.cart,mod="cart",tipo="pm",i=i,rownm=nf.dT)
		  pmc.cart.df <- ml_dfs(dat=pmc.cart,mod="cart",tipo="pmc",i=i,rownm=nf.dT)
	    if(!is.null(dim(pmc.cart.df)))
        names(pmc.cart.df)[2:(length(nivY)+1)] <- paste("CART_",nivY,sep="")
	    }
    }
    if("Logit" %in% modelosML){
      #requireNamespace("stats",quietly=TRUE)
      m.logit <- stats::glm(fo,family="binomial",data=datE,...)
      if(lgt.stepaic)
        m.logit.sa[[i]] <- MASS::stepAIC(m.logit,scope=list(upper=fo,lower=~1),direction="ba",trace=0)
      pm.logit <- stats::predict(m.logit,newdata=if(dpg) datT else datT[,-columnaY],type="response")
      #devuelve P(exito)=P(Y=1) en TU caso; depende del orden de niveles de Y!!!
      dfpmlg[[i]] <- data.frame(1-pm.logit,pm.logit)
	    names(dfpmlg[[i]]) <- nivY
	    pm.logit2[[i]] <- data.frame("id"=attr(dfpmlg[[i]],"row.names"),"Ypred"=factor(colnames(dfpmlg[[i]])[apply(dfpmlg[[i]],1,which.max)],levels=nivY))	#dfpmlg$ypred
      t.mlg1[[i]] <- table(datT[,columnaY],pm.logit2[[i]]$Ypred,dnn=c("YObs","YPred"))
      t.mlg <- mdcmed(t.mlg1[[i]])
      if(predicts.out){
		    pm.lgt.df <- ml_dfs(dat=pm.logit2[[i]],mod="logit",tipo="pm",i=i)
		    pmc.lgt.df <- ml_dfs(dat=dfpmlg[[i]],mod="logit",tipo="pmc",i=i)
		    if(!is.null(dim(pmc.lgt.df)))
			    names(pmc.lgt.df)[2:(length(nivY)+1)] <- paste("Lgt_",nivY,sep="") #REVISAR DESDE ACA! 300318 -- salida tryCatch=salida(print) ej resultado de arbol
	    }
    }
    if("SVM" %in% modelosML){
      #requireNamespace("e1071",quietly=TRUE)
      m.svm <- svm(fo,data=datE,cost=costsvm,kernel=kernel,decision.values=TRUE,gamma=svm.gamma,probability=TRUE)
      pm.svm <- predict(object=m.svm,newdata=if(dpg) datT else datT[,-columnaY],probability=TRUE,decision.values=TRUE)
      pm.svm2[[i]] <- data.frame("id"=attr(pm.svm,"names"),"Ypred"=factor(colnames(attr(pm.svm,"probabilities"))[apply(attr(pm.svm,"probabilities"),1,which.max)],levels=nivY))	#dfpmlg$ypred
      tsvm[[i]] <- table(datT[,columnaY],pm.svm2[[i]]$Ypred,dnn=c("YObs","YPred"))
      t.svm <- mdcmed(tsvm[[i]])
	    if(predicts.out){
		    pm.svm.df <- ml_dfs(dat=pm.svm2[[i]],mod="svm",tipo="pm",i=i)
		    pmc.svm.df <- ml_dfs(dat=pm.svm,mod="svm",tipo="pmc",i=i)
		    if(!is.null(dim(pmc.svm.df)))
			    names(pmc.svm.df)[2:(length(nivY)+1)] <- paste("SVM_",nivY,sep="")
		  }
    }
    if("RF" %in% modelosML){
      #requireNamespace("randomForest",quietly=TRUE)
      m.rf <- randomForest(fo,data=datE,mtry=mtry,importance=TRUE,keep.forest=TRUE,ntree=ntree,proximity=TRUE,...)
      pm.rf[[i]] <- predict(object=m.rf,newdata=if(dpg) datT else datT[,-columnaY])
	    pmc.rf[[i]] <- predict(object=m.rf,newdata=if(dpg) datT else datT[,-columnaY],type="prob")
      trf[[i]] <- table(datT[,columnaY],factor(pm.rf[[i]],levels=nivY),dnn=c("YObs","YPred"))
      t.mrf <- mdcmed(trf[[i]])
	    if(predicts.out){
		    pm.rf.df <- ml_dfs(dat=pm.rf,mod="rf",tipo="pm",i=i)
		    pmc.rf.df <- ml_dfs(dat=pmc.rf,mod="rf",tipo="pmc",i=i)
		    if(!is.null(dim(pmc.rf.df)))
			    names(pmc.rf.df)[2:(length(nivY)+1)] <- paste("RF_",nivY,sep="")
		  }
    }
    if("Boosting" %in% modelosML){
      #requireNamespace("adabag",quietly=TRUE)
      m.boost <- boosting(fo,data=datE,mfinal=b.mfinal)
      pm.boost[[i]] <- predict(object=m.boost,newdata=if(dpg) datT else datT[,-columnaY],type="class")
      tboost[[i]] <- table(datT[,columnaY],factor(pm.boost[[i]]$class,levels=nivY),dnn=c("YObs","YPred"))
      t.boost <- mdcmed(tboost[[i]])
  	  if(predicts.out){
  		  pm.boost.df <- ml_dfs(dat=pm.boost,mod="boost",tipo="pm",i=i,rownm=nf.dT)
  		  pmc.boost.df <- ml_dfs(dat=pm.boost,mod="boost",tipo="pmc",i=i,rownm=nf.dT) #falta contemplar "Yp_Boost"=factor(pm.boo[[i]]$class) y "Boost"=pm.boo[[i]]$prob
  		}
    }
    if("CDB" %in% modelosML){
	  #A.cdb(...)
      if(dpg){
        datEm <- datE
        datTm <- datT
        Ym <- columnaY
      } else {
        datEm <- model.frame(fo,data=datE)
        datTm <- model.frame(fo,data=datT)
        Ym <- 1
      }
      efet <- dk(dfe=datEm,dft=datTm,vclase=Ym,dens=cdb.dens)
      if(uso.em){ ##switch con "indicador de salida" #efet$f_mix$fmix_1$pf_ctcl
        tsf <- efet$tsf
        alf <- emalg2k(alfa1=em.alfa10,f1=switch(EXPR=tsf,snum=efet$f_num$fy1_Xte$fnum_Xte_prod,
            scat=efet$f_cat$fcat_1$fcat_prod,mixt=efet$f_mix$fmix_1$pf_ctcl),
            f2=switch(EXPR=tsf,snum=efet$f_num$fy2_Xte$fnum_Xte_prod,
            scat=efet$f_cat$fcat_2$fcat_prod,mixt=efet$f_mix$fmix_2$pf_ctcl),
            tol=em.tol,plota=FALSE,listalfa=TRUE)
	  	#B.predict.cdb(...)
        papl[[i]] <- pape(f_dk=efet,yr=datTm[,Ym],x=datTm[,-Ym],datfalt=efet$datfalt,
          alfas=c(alf$alfa1,alf$alfa2))
      } else {#B.predict.cdb(...)
        papl[[i]] <- pape(alfas=NULL,f_dk=efet,yr=datTm[,Ym],x=datTm[,-Ym],
						datfalt=efet$datfalt,nomb.obs=attr(datTm,"row.names"))
      }
      pm.cdb[[i]] <- papl[[i]]$PaP$ypred #la posta del predict esta aca
	    #C.MdConf
      tcb[[i]] <- table(datTm[,Ym],factor(pm.cdb[[i]],levels=nivY),dnn=c("YObs","YPred"))
      if(uso.em){
        em.alf[[i]] <- c("alfa1"=alf$alfa1,"alfa2"=alf$alfa2)
      }
	    if(predicts.out){
		  #mas complejo! como captamos papl[[i]]$PaP[,1:length(nivY)]?? Mejorar ml_dfs()!!!
	     pm.cdb.df <- data.frame("id"=attr(datT,which="row.names"),"Yp_CDB"=pm.cdb[[i]])
	     pmc.cdb.df <- data.frame("id"=attr(datT,which="row.names"),papl[[i]]$PaP[,1:length(nivY)])
	     if(!is.null(dim(pmc.cdb.df)))
	      names(pmc.cdb.df)[2:(length(nivY)+1)] <- paste("CDB_",nivY,sep="")
	     }
	  }
	  ###Predicts
	  if(predicts.out){
		###2.Predicts para TODOS (menos pa MoCos)
		##Predicts
		listdfs <- list("YObs"=data.frame("id"=attr(datT,which="row.names"),"Yobs"=datT[,columnaY]),
                    if(is.data.frame(pm.cart.df)) "cart"=pm.cart.df,
                    if(is.data.frame(pm.svm.df)) "svm"=pm.svm.df,
                    if(is.data.frame(pm.rf.df)) "rf"=pm.rf.df,
                    if(is.data.frame(pm.lgt.df)) "logit"=pm.lgt.df,
                    if(is.data.frame(pm.boost.df)) "boost"=pm.boost.df,
                    if(is.data.frame(pm.cdb.df)) "cdb"=pm.cdb.df) #FALTA: implementar para pm.mod.df=NULL y que no chille
		if(any(unlist(lapply(listdfs,function(x) any(is.data.frame(x)))))){
			#DF generado x Reduce(merge(...)), ordenado x nro 'correcto' obs
			cvY_noOrd <- Reduce(f=function(...) merge(...,by="id",all=TRUE),listdfs) #30/05/18 revisar merge
			#cvY_noOrd$t1s_pred <- round(rowSums(cvY_noOrd[,4:ncol(cvY_noOrd)])/length(mimp),2) #rowSums NO funca: TODOS son factores!
			comp.vs.Yobs[[i]] <- cbind("OrdxObs"=rownames(cvY_noOrd)[order(rownames(cvY_noOrd))],cvY_noOrd[order(rownames(cvY_noOrd)),])
		} #>>Falta: devolucion si tenes uno de los elementos de la lista NULL
		##Prob a posteriori
		listdfsp <- list("YObs"=data.frame("id"=attr(datT,which="row.names"),"Yobs"=datT[,columnaY]),
                     if(is.data.frame(pmc.cart.df)) "cart"=pmc.cart.df,
                     if(is.data.frame(pmc.svm.df)) "svm"=pmc.svm.df,
                     if(is.data.frame(pmc.rf.df)) "rf"=pmc.rf.df,
                     if(is.data.frame(pmc.lgt.df)) "logit"=pmc.lgt.df,
                     if(is.data.frame(pmc.boost.df)) "boost"=pmc.boost.df,
                     if(is.data.frame(pmc.cdb.df)) "cdb"=pmc.cdb.df) #FALTA: implementar para pm.mod.df=NULL y que no chille
		if(any(unlist(lapply(listdfsp,function(x) any(is.data.frame(x)))))){
			#DF generado x Reduce(merge(...)), ordenado x nro 'correcto' obs
			cvY_noOrd_p <- Reduce(f=function(...) merge(...,by="id",all=TRUE),listdfsp)
			#cvY_noOrd$t1s_pred <- round(rowSums(cvY_noOrd[,4:ncol(cvY_noOrd)])/length(mimp),2) #rowSums NO funca: TODOS son factores!
			comp.vs.papc[[i]] <- cbind("OrdxObs"=rownames(cvY_noOrd_p)[order(rownames(cvY_noOrd_p))],cvY_noOrd_p[order(rownames(cvY_noOrd_p)),])
		} #>>idem anterior
	 }
  }
  ###3.ModelosConsenso (afuera de loop xq precisan YprVsYob,YprVsPaP)
  mc <- FALSE
  mocimp <- c("vm","mp","waauc")
  mocimp <- mocimp[sort(match(x=moc,table=mocimp))]
  if(length(mocimp)>0)
    mc <- TRUE
  if(mc){
    lmi <- length(mimp)
    part <- max(parti)
    ypr <- comp.vs.Yobs
    pap <- comp.vs.papc #idem
    mg <- vector(mode="list",length=part)
    if("vm" %in% moc){
      for(k in 1:part){
        ### MeCo1::VM (1 si >50% son 1, 0 resto; en caso de empate asignar aleatoriamente)
        vm0 <- apply(X=ypr[[k]][,4:(lmi+3)],MARGIN=1,FUN=function(x) sum(x=="0")) #error correg 21/02/19
        vm1 <- apply(X=ypr[[k]][,4:(lmi+3)],MARGIN=1,FUN=function(x) sum(x=="1"))
        dfvm <- data.frame("VM_0"=vm0,"VM_1"=vm1)
        ypr[[k]]$VM <- factor(levels(ypr[[k]]$Yobs)[max.col(dfvm)])
        pap[[k]]$VM_0 <- dfvm$VM_0
        pap[[k]]$VM_1 <- dfvm$VM_1
      }
    }
    if("mp" %in% moc){
      for(k in 1:part){
        ### MeCo2::MeanProb
        mp0 <- apply(X=pap[[k]][,seq(from=4,to=(lmi*2)+2,by=2)],MARGIN=1,FUN=function(x) sum(x)/length(mimp))
        mp1 <- apply(X=pap[[k]][,seq(from=5,to=(lmi*2)+3,by=2)],MARGIN=1,FUN=function(x) sum(x)/length(mimp))
        dfmp <- data.frame("MP_0"=mp0,"MP_1"=mp1)
        ypr[[k]]$MP <- factor(levels(pap[[k]]$Yobs)[max.col(dfmp)])
        pap[[k]]$MP_0 <- dfmp$MP_0
        pap[[k]]$MP_1 <- dfmp$MP_1
      }
    }
    if("waauc" %in% moc){
      ### MeCo3::WA-AUC (promedio ponderado) -- hacer auroc que funke ADENTRO de asm2(); ver
      #Funcion "auroc interna" (sustituye 'wa <- auroc(x)') 090618
      wa <- data.frame(matrix(nrow=part,ncol=lmi))
      vec <- seq(from=5,to=(lmi*2)+3,by=2)
      for(k in 1:part){
        for(j in seq_along(vec)){
          wa[k,j] <- furoc(pred=pap[[k]][,vec[j]],patroro=pap[[k]][,3],med="auc")
        }
      }
      names(wa) <- paste("AUROC_",mimp,sep="")
      #resto:
      for(k in 1:part){
        m0 <- pap[[k]][,seq(from=4,to=(lmi*2)+2,by=2)]
        wm0 <- as.matrix(m0)%*%t(as.matrix(wa))
        pap[[k]]$wau0 <- rowSums(wm0)
        m1 <- pap[[k]][,seq(from=5,to=(lmi*2)+3,by=2)]
        wm1 <- as.matrix(m1)%*%t(as.matrix(wa))
        pap[[k]]$wau1 <- rowSums(wm1)
        pap[[k]]$WAU <- factor(levels(pap[[k]]$Yobs)[max.col(data.frame(pap[[k]]$wau0,pap[[k]]$wau1))])
      }
    }
    #Juntamos todo en df: MdConf ACA!
    for(k in 1:part){
      mg_df <- merge(ypr[[k]],pap[[k]],by.x="OrdxObs",by.y="OrdxObs")
      mg[[k]] <- mg_df[,c(1:10,which(names(mg_df) %in% c("MP","WAU")))] #25,28 y VM???!
      if("vm" %in% moc)
        vml[[k]] <- table(mg[[k]]$Yobs.x,factor(mg[[k]]$VM,levels=nivY),dnn=c("YObs","YPred")) #forzamos niveles Ypred
      if("mp" %in% moc)
        mpl[[k]] <- table(mg[[k]]$Yobs.x,factor(mg[[k]]$MP,levels=nivY),dnn=c("YObs","YPred"))
		  if("waauc" %in% moc)
		    waul[[k]] <- table(mg[[k]]$Yobs.x,factor(mg[[k]]$WAU,levels=nivY),dnn=c("YObs","YPred"))
    }
  }
    #UNIR: mg con resto de salidas (Yobs,Yprob): [id|pm.cart.df|...|pm.vm.df|...|pm.wa.df] --  es necesario???
    le.trf <- if("RF" %in% modelosML) errp(trf)
    le.tmlg <- if("Logit" %in% modelosML) errp(t.mlg1)
    le.tsvm <- if("SVM" %in% modelosML) errp(tsvm)
    le.tboost <- if("Boosting" %in% modelosML) errp(tboost)
    le.tcb <- if("CDB" %in% modelosML) errp(tcb)
    le.tcart <- if("CART" %in% modelosML) errp(tcart)
    if(mc){
      le.erpv <- if("vm" %in% moc) errp(vml)
      le.erpm <- if("mp" %in% moc) errp(mpl)
      le.erpw <- if("waauc" %in% moc) errp(waul)
      lge.gr <- list(le.tcart$todos.erp,le.tmlg$todos.erp,le.tsvm$todos.erp,le.trf$todos.erp,le.tboost$todos.erp,le.tcb$todos.erp,
                   le.erpv$todos.erp,le.erpw$todos.erp,le.erpm$todos.erp)
    } else {
      lge.gr <- list(le.tcart$todos.erp,le.tmlg$todos.erp,le.tsvm$todos.erp,le.trf$todos.erp,le.tboost$todos.erp,le.tcb$todos.erp)
    }
    #Tiempo de corridas:
    fint <- Sys.time()
    tej <- fint-init
    ###4.SALIDA FINAL: 1)infoY, 2)metadat$, 3)asm$(mod1,mod2,...)$(MdConf,i...,pred_), 4)otros??
    out <- list("Metadatos"=list("Y"=names(datE[columnaY]),tipoY=tipoY,"Formula"=frml,mimp=mimp,moc=moc,
                                 parti=max(parti),propme=propme,permt=permt,perme=perme,"TiempoEjec"=round(tej,2)),
                "ASM"=list("CART"=list("MdConf_CART"=tcart,"iCART"=le.tcart,"pred_CART"=pm.cart),
                  "Logit"=list("MdConf_Logit"=t.mlg1,"iLogit"=le.tmlg,"SA_Logit"=m.logit.sa,"pred_Logit"=pm.logit2),
                  "SVM"=list("MdConf_SVM"=tsvm,"iSVM"=le.tsvm,"pred_SVM"=pm.svm),
                  "RF"=list("MdConf_RF"=trf,"iRF"=le.trf,"pred_RF"=pm.rf),
                  "Boost"=list("MdConf_Boost"=tboost,"iBoost"=le.tboost,"pred_Boost"=pm.boost),
                  "CDB"=list("MdConf_CDB"=tcb,"iCDB"=le.tcb,"pred_CDB"=pm.cdb,em.alf=em.alf),
                  "MC_VM"= if(predicts.out & "vm" %in% moc) list("MdConf_VM"=vml,"iVM"=le.erpv),
                  "MC_MP"= if(predicts.out & "mp" %in% moc) list("MdConf_MP"=mpl,"iMP"=le.erpm),
                  "MC_WA"= if(predicts.out & "waauc" %in% moc) list("MdConf_WA"=waul,"iWA"=le.erpw),
                  "MC_Todos"= if(predicts.out) list("Pred"=mg),
                lge.gr=lge.gr,"YprVsYob"=comp.vs.Yobs,"YprVsPaP"=comp.vs.papc)) #incluir MoCos en esta salida!!!
    class(out) <- "asm"
    return(invisible(out))
}

############################################################
