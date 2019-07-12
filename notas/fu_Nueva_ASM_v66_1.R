############################################################
###### TESIS DA [Reunion 66]: NUEVA version ASM FULL! ######
############################################################

### Nueva Funcion "asm2"
#-- Que hace: 
#-- Entra: 
#-- Sale: 
#-- Otros: 
#-- 

# asm2 <- function(frml,datFULL,columnaY,modelosML,parti=10,propme=0.75,
#                  semillas=NULL,metadatos=NULL,error.resum=TRUE,moc=c("vm","mp","waauc"),
#                  cart.ctrl=rpart.control(cp=.01), #CART
#                  lgt.stepaic=FALSE, #logit
#                  costsvm=1,kernel="radial",svm.gamma=0.5, #SVM
#                  ntree=100, mtry=2, #RF
#                  b.mfinal=100, #Boost
#                  uso.em=TRUE,em.alfa10=0.5,em.iter=100,em.tol=1e-5,cdb.dens="density",...){
#   ###1.Modelos "usuales"
#   init <- Sys.time()
#   mimp <- c("CART","Logit","SVM","RF","Boosting","CDB")
#   mimp <- mimp[sort(match(x=modelosML,table=mimp))]
#   parti <- 1:parti
#   fo <- as.formula(frml)
#   largo <- length(attr(terms(fo),"term.labels"))
#   if(!is.null(datFULL))
#     tipoY <- class(datFULL[,columnaY])
#   else
#     tipoY <- class(datT[,columnaY])
#   nivY <- levels(as.factor(datFULL[,columnaY]))
#   dpg <- FALSE
#   if(sum(sapply(datFULL,function(x) sum(is.na(x)),simplify=TRUE))>0){  #if(missingdata)
#     datFULL <- model.frame(formula=fo,data=datFULL)
#     columnaY <- 1
#     dpg <- TRUE
#   }
#   trf <- tbag <- t.mlg1 <- tsvm <- tnb <- tcb <- tcart <- papl <- perme <- permt <- em.alf <- m.logit.sa <-
#     tboo <- pm.cart <- pm.logit2 <- pm.svm <- pm.rf <- pm.boo <- pm.cdb <- comp.vs.Yobs <-
#     pmc.cart <- pmc.rf <- comp.vs.papc <- mg <- vml <- mpl <- waul <- vector(mode="list",length=max(parti)) #pmc.logit
#   for(i in parti){
#     if(!is.null(semillas)) set.seed(semillas[i])
#     perm_df <- uefi:::mute(df=datFULL,nroperm=1,pme=propme)
#     datE <- perm_df$dat_me[[1]]
#     datT <- perm_df$dat_mt[[1]]
#     perme[[i]] <- perm_df$indME
#     permt[[i]] <- perm_df$rnMT #row.names, no indices ordenados como hasta ahora
#     if("CART" %in% modelosML){
#       m.cart <- rpart(fo,data=datE,method="class",control=cart.ctrl)
#       pm.cart[[i]] <- predict(m.cart,newdata=if(dpg) datT else datT[,-columnaY],type="class")
#       pmc.cart[[i]] <- predict(m.cart,newdata=if(dpg) datT else datT[,-columnaY],type="prob")
#       tcart[[i]] <- table(datT[,columnaY],factor(pm.cart[[i]],levels=nivY),dnn=c("YObs","YPred"))
#       t.cart <- mdcmed(tcart[[i]])
#       #>>falta tryCatch(); ver lato
#       pm.cart.df <- data.frame("id"=attr(pm.cart[[i]],which="names"),"Yp_CART"=pm.cart[[i]])
#       pmc.cart.df <- data.frame("id"=attr(pmc.cart[[i]],which="dimnames")[[1]],pmc.cart[[i]])
#       names(pmc.cart.df)[2:(length(nivY)+1)] <- paste("CART_",nivY,sep="")
#     }
#     if("Logit" %in% modelosML){
#       m.logit <- glm(fo,family="binomial",data=datE,...)
#       if(lgt.stepaic)
#         m.logit.sa[[i]] <- stepAIC(m.logit,scope=list(upper=fo,lower=~1),direction="ba",trace=0)
#       pm.logit <- predict(m.logit,newdata=if(dpg) datT else datT[,-columnaY],type="response")
#       dfpmlg <- data.frame(pm.logit,1-pm.logit)
#       names(dfpmlg) <- nivY	
#       pm.logit2[[i]] <- factor(colnames(dfpmlg)[apply(dfpmlg,1,which.max)])	#dfpmlg$ypred
#       t.mlg1[[i]] <- table(datT[,columnaY],factor(pm.logit2[[i]],levels=nivY),dnn=c("YObs","YPred"))
#       t.mlg <- mdcmed(t.mlg1[[i]])
#       #>>falta tryCatch(); ver lato
#       pm.lgt.df <- data.frame("id"=attr(pm.logit,which="names"),"Yp_Logit"=pm.logit2[[i]])
#       pmc.lgt.df <- data.frame("id"=attr(pm.logit,which="names"),dfpmlg)
#       names(pmc.lgt.df)[2:(length(nivY)+1)] <- paste("Lgt_",nivY,sep="")
#     }
#     if("SVM" %in% modelosML){
#       m.svm <- svm(fo,data=datE,cost=costsvm,kernel=kernel,decision.values=TRUE,gamma=svm.gamma,probability=TRUE)
#       pm.svm[[i]] <- predict(object=m.svm,newdata=if(dpg) datT else datT[,-columnaY],probability=TRUE,decision.values=TRUE)
#       tsvm[[i]] <- table(datT[,columnaY],factor(pm.svm[[i]],levels=nivY),dnn=c("YObs","YPred"))
#       t.svm <- mdcmed(tsvm[[i]])
#       #>>falta tryCatch(); ver lato
#       pm.svm.df <- data.frame("id"=attr(pm.svm[[i]],which="names"),"Yp_SVM"=pm.svm[[i]])
#       pmc.svm.df <- data.frame("id"=attr(pm.svm[[i]],which="names"),attr(pm.svm[[i]],"probabilities"))
#       names(pmc.svm.df)[2:(length(nivY)+1)] <- paste("SVM_",nivY,sep="")
#     }
#     if("RF" %in% modelosML){
#       m.rf <- randomForest(fo,data=datE,mtry=mtry,importance=TRUE,keep.forest=TRUE,ntree=ntree,proximity=TRUE,...)
#       pm.rf[[i]] <- predict(object=m.rf,newdata=if(dpg) datT else datT[,-columnaY])
#       pmc.rf[[i]] <- predict(object=m.rf,newdata=if(dpg) datT else datT[,-columnaY],type="prob")
#       trf[[i]] <- table(datT[,columnaY],factor(pm.rf[[i]],levels=nivY),dnn=c("YObs","YPred"))
#       t.mrf <- mdcmed(trf[[i]])
#       #>>falta tryCatch(); ver lato
#       pm.rf.df <- data.frame("id"=attr(pm.rf[[i]],which="names"),"Yp_RF"=pm.rf[[i]])
#       pmc.rf.df <- data.frame("id"=attr(pmc.rf[[i]],which="dimnames")[[1]],pmc.rf[[i]])
#       names(pmc.rf.df)[2:(length(nivY)+1)] <- paste("RF_",nivY,sep="")
#     }
#     if("Boosting" %in% modelosML){
#       m.boo <- boosting(fo,data=datE,mfinal=b.mfinal)
#       pm.boo[[i]] <- predict(object=m.boo,newdata=if(dpg) datT else datT[,-columnaY],type="class")
#       tboo[[i]] <- table(datT[,columnaY],factor(pm.boo[[i]]$class,levels=nivY),dnn=c("YObs","YPred"))
#       t.boo <- mdcmed(tboo[[i]])
#       #>>falta tryCatch(); ver lato
#       pm.boo.df <- data.frame("id"=attr(datT,which="row.names"),"Yp_Boost"=factor(pm.boo[[i]]$class))
#       pmc.boo.df <- data.frame("id"=attr(datT,which="row.names"),"Boost"=pm.boo[[i]]$prob)
#     }
#     if("CDB" %in% modelosML){
#       #A.cdb(...)
#       if(dpg){
#         datEm <- datE
#         datTm <- datT
#         Ym <- columnaY
#       } else {
#         datEm <- model.frame(fo,data=datE)
#         datTm <- model.frame(fo,data=datT)
#         Ym <- 1
#       }
#       efet <- dk(dfe=datEm,dft=datTm,vclase=Ym,dens=cdb.dens)
#       if(uso.em){ ##switch con "indicador de salida" #efet$f_mix$fmix_1$pf_ctcl
#         tsf <- efet$tsf
#         alf <- emalg2k(alfa1=em.alfa10,f1=switch(EXPR=tsf,snum=efet$f_num$fy1_Xte$fnum_Xte_prod,
#                                                  scat=efet$f_cat$fcat_1$fcat_prod,mixt=efet$f_mix$fmix_1$pf_ctcl),
#                        f2=switch(EXPR=tsf,snum=efet$f_num$fy2_Xte$fnum_Xte_prod,
#                                  scat=efet$f_cat$fcat_2$fcat_prod,mixt=efet$f_mix$fmix_2$pf_ctcl),
#                        tol=em.tol,plota=FALSE,listalfa=TRUE)
#         #B.predict.cdb(...)
#         papl[[i]] <- pape(f_dk=efet,yr=datTm[,Ym],x=datTm[,-Ym],datfalt=efet$datfalt,
#                           alfas=c(alf$alfa1,alf$alfa2))
#       } else {#B.predict.cdb(...)
#         papl[[i]] <- pape(alfas=NULL,f_dk=efet,yr=datTm[,Ym],x=datTm[,-Ym],
#                           datfalt=efet$datfalt,nomb.obs=attr(datTm,"row.names"))
#       }
#       pm.cdb[[i]] <- papl[[i]]$PaP$ypred #la posta del predict esta aca
#       #C.MdConf
#       tcb[[i]] <- table(datTm[,Ym],factor(pm.cdb[[i]],levels=nivY),dnn=c("YObs","YPred"))
#       if(uso.em){
#         em.alf[[i]] <- c("alfa1"=alf$alfa1,"alfa2"=alf$alfa2)
#       }
#       #D.
#       pm.cdb.df <- data.frame("id"=attr(datT,which="row.names"),"Yp_CDB"=pm.cdb[[i]])
#       pmc.cdb.df <- data.frame("id"=attr(datT,which="row.names"),papl[[i]]$PaP[,1:length(nivY)])
#       names(pmc.cdb.df)[2:(length(nivY)+1)] <- paste("CDB_",nivY,sep="")
#       }
#     }
#     ###2.Predicts para TODOS (menos pa MoCos)
#     ##Predicts
#     listdfs <- list("YObs"=data.frame("id"=attr(datT,which="row.names"),"Yobs"=datT[,columnaY]),
#                     if(!is.null(pm.cart.df)) pm.cart.df,
#                     if(!is.null(pm.svm.df)) pm.svm.df,
#                     if(!is.null(pm.rf.df)) pm.rf.df,
#                     if(!is.null(pm.lgt.df)) pm.lgt.df,
#                     if(!is.null(pm.boo.df)) pm.boo.df,
#                     if(!is.null(pm.cdb.df)) pm.cdb.df) #FALTA: implementar para pm.mod.df=NULL y que no chille
#     if(lapply(listdfs,function(x) any(!is.null(x)))){
#       #DF generado x Reduce(merge(...)), ordenado x nro 'correcto' obs
#       cvY_noOrd <- Reduce(f=function(...) merge(...,by="id",all=TRUE),listdfs)
#       #cvY_noOrd$t1s_pred <- round(rowSums(cvY_noOrd[,4:ncol(cvY_noOrd)])/length(mimp),2) #rowSums NO funca: TODOS son factores!
#       comp.vs.Yobs[[i]] <- cbind("OrdxObs"=rownames(cvY_noOrd)[order(rownames(cvY_noOrd))],cvY_noOrd[order(rownames(cvY_noOrd)),])
#       } #>>Falta: devolucion si tenes uno de los elementos de la lista NULL
#     ##Prob a posteriori
#     listdfsp <- list("YObs"=data.frame("id"=attr(datT,which="row.names"),"Yobs"=datT[,columnaY]),
#                      if(!is.null(pmc.cart.df)) pmc.cart.df,
#                      if(!is.null(pmc.svm.df)) pmc.svm.df,
#                      if(!is.null(pmc.rf.df)) pmc.rf.df,
#                      if(!is.null(pmc.lgt.df)) pmc.lgt.df,
#                      if(!is.null(pmc.boo.df)) pmc.boo.df,
#                      if(!is.null(pmc.cdb.df)) pmc.cdb.df) #FALTA: implementar para pm.mod.df=NULL y que no chille
#     if(lapply(listdfsp,function(x) any(!is.null(x)))){
#       #DF generado x Reduce(merge(...)), ordenado x nro 'correcto' obs
#       cvY_noOrd_p <- Reduce(f=function(...) merge(...,by="id",all=TRUE),listdfsp)
#       #cvY_noOrd$t1s_pred <- round(rowSums(cvY_noOrd[,4:ncol(cvY_noOrd)])/length(mimp),2) #rowSums NO funca: TODOS son factores!
#       comp.vs.papc[[i]] <- cbind("OrdxObs"=rownames(cvY_noOrd_p)[order(rownames(cvY_noOrd_p))],cvY_noOrd_p[order(rownames(cvY_noOrd_p)),])
#       } #>>idem anterior
#     ###3.ModelosConsenso (afuera de loop xq precisan YprVsYob,YprVsPaP)
#     lmi <- length(mimp)
#     part <- parti 
#     ypr <- comp.vs.Yobs 
#     pap <- comp.vs.papc #idem
#     mg <- vector(mode="list",length=part)
#     if("vm" %in% moc){
#       for(k in 1:part){
#         ### MeCo1::VM (1 si >50% son 1, 0 resto; en caso de empate asignar aleatoriamente)
#         vm0 <- apply(X=ypr[[k]][,4:lmi+3],MARGIN=1,FUN=function(x) sum(x=="0"))
#         vm1 <- apply(X=ypr[[k]][,4:lmi+3],MARGIN=1,FUN=function(x) sum(x=="1"))
#         dfvm <- data.frame(vm0,vm1)
#         ypr[[k]]$VM <- factor(levels(ypr[[k]]$Yobs)[max.col(dfvm)])
#       }
#     }
#     if("mp" %in% moc){
#       for(k in 1:part){
#         ### MeCo2::MeanProb
#         mp0 <- apply(X=pap[[k]][,seq(from=4,to=(lmi*2)+2,by=2)],MARGIN=1,FUN=function(x) sum(x)/6)
#         mp1 <- apply(X=pap[[k]][,seq(from=5,to=(lmi*2)+3,by=2)],MARGIN=1,FUN=function(x) sum(x)/6)
#         dfmp <- data.frame(mp0,mp1)
#         pap[[k]]$MP <- factor(levels(pap[[k]]$Yobs)[max.col(dfmp)])
#       }
#     }
#     if("waauc" %in% moc){
#       for(k in 1:part){
#         wa <- auroc(x)
#         m0 <- pap[[k]][,seq(from=4,to=(lmi*2)+2,by=2)]
#         wm0 <- as.matrix(m0)%*%t(as.matrix(wa))
#         pap[[k]]$wau0 <- rowSums(wm0)
#         m1 <- pap[[k]][,seq(from=5,to=(lmi*2)+3,by=2)]
#         wm1 <- as.matrix(m1)%*%t(as.matrix(wa))
#         pap[[k]]$wau1 <- rowSums(wm1)
#         pap[[k]]$WAU <- factor(levels(pap[[k]]$Yobs)[max.col(data.frame(pap[[k]]$wau0,pap[[k]]$wau1))])
#       }
#     }
#     #Juntamos todo en df
#     for(k in 1:part){
#       mg_df <- merge(ypr[[k]],pap[[k]],by.x="OrdxObs",by.y="OrdxObs")
#       mg[[k]] <- mg_df[,c(1:10,which(names(mg_df) %in% c("MP","WAU")))] #25,28 y VM???!
#       vml[[k]] <- table(mg[[k]]$Yobs.x,mg[[k]]$VM,dnn=c("YObs","YPred"))
#       mpl[[k]] <- table(mg[[k]]$Yobs.x,mg[[k]]$MP,dnn=c("YObs","YPred"))
#       waul[[k]] <- table(mg[[k]]$Yobs.x,mg[[k]]$WAU,dnn=c("YObs","YPred"))      
#     }
#     #UNIR: mg con resto de salidas (Yobs,Yprob): [id|pm.cart.df|...|pm.vm.df|...|pm.wa.df]
#     le.trf <- if("RF" %in% modelosML) errp(trf) else NULL
#     le.tmlg <- if("Logit" %in% modelosML) errp(t.mlg1) else NULL
#     le.tsvm <- if("SVM" %in% modelosML) errp(tsvm) else NULL
#     le.tboo <- if("Boosting" %in% modelosML) errp(tboo) else NULL
#     le.tcb <- if("CDB" %in% modelosML) errp(tcb) else NULL
#     le.tcart <- if("CART" %in% modelosML) errp(tcart) else NULL
#     le.erpv <- if("vm" %in% moc) uefi:::errp(vml)
#     le.erpw <- if("mp" %in% moc) uefi:::errp(waul)
#     le.erpm <- if("waauc" %in% moc) uefi:::errp(mpl)
#     lge.gr <- list(le.tcart$todos.erp,le.tmlg$todos.erp,le.tsvm$todos.erp,le.trf$todos.erp,le.tboo$todos.erp,le.tcb$todos.erp,
#                    le.erpv$todos.erp,le.erpw$todos.erp,le.erpm$todos.erp)
#     #Tiempo de corridas:
#     fint <- Sys.time()
#     tej <- fint-init
#     ###4.SALIDA FINAL: 1)infoY, 2)metadat$, 3)asm$(mod1,mod2,...)$(MdConf,i...,pred_), 4)otros??
#     out <- list("Metadatos"=list("Y"=names(datE[columnaY]),tipoY=tipoY,"Formula"=frml,mimp=mimp,parti=max(parti),
#                                  propme=propme,permt=permt,perme=perme,"TiempoEjec"=round(tej,2)),
#                 "ASM"=list("CART"=list("MdConf_CART"=tcart,"iCART"=le.tcart,"pred_CART"=pm.cart),
#                            "Logit"=list("MdConf_Logit"=t.mlg1,"iLogit"=le.tmlg,"SA_Logit"=m.logit.sa,"pred_Logit"=pm.logit2),
#                            "SVM"=list("MdConf_SVM"=tsvm,"iSVM"=le.tsvm,"pred_SVM"=pm.svm),
#                            "RF"=list("MdConf_RF"=trf,"iRF"=le.trf,"pred_RF"=pm.rf),
#                            "Boost"=list("MdConf_Boost"=tboo,"iBoost"=le.tboo,"pred_Boost"=pm.boo),
#                            "CDB"=list("MdConf_CDB"=tcb,"iCDB"=le.tcb,"pred_CDB"=pm.cdb,em.alf=em.alf),
#                            "MC_VM"=list("MdConf_VM"=,"iVM"=,"pred_VM"=), #falta
#                            "MC_MP"=list("MdConf_MP"=,"iMP"=,"pred_MP"=), #falta
#                            "MC_WA"=list("MdConf_WA"=,"iWA"=,"pred_WA"=)),#falta
#                 lge.gr=lge.gr,"YprVsYob"=comp.vs.Yobs,"YprVsPaP"=comp.vs.papc) #incluir MoCos en esta salida!!!
#     class(out) <- "asm"
#     return(invisible(out))
# }
