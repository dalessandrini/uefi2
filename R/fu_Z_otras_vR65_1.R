#############################################################
### TESIS DA [Reunion 57]: otras funciones (p.ej sin uso) ###
#############################################################

## Version 1: desde 18:45 02/09/2017

#############################################################

### Funcion 4: bloques de wrappers para modelos ML
#-- Que hacen:
#-- Entran:
#-- Salen:
#-- Otros:

## Bloque Random Forest/Bagging:
#' #@export
# etm.rf <- function(fo,train,test,colY,mtry,ntree,...){
  # raf <- randomForest(fo,data=train,mtry=mtry,importance=TRUE,keep.forest=TRUE,ntree=ntree,proximity=TRUE,...)
  # pred_raf <- predict(object=raf,newdata=test)
  # mdcrf <- table(test[,colY],pred_raf)
  # resu.mdcrf <- mdcmed(mdcrf)
  # list(raf=raf,mdcrf=mdcrf,resu.mdcrf=resu.mdcrf)
# }
#En bloque dentro de asm2c
#rf[[i]] <- etm.rf(fo=fo,colY,train=datE,test=datT,mtry={mtry,largo}) #mtry<largo::Bagging!!

## Bloque GLM-Logit:
#' #@export
# etm.logit <- function(fo,train,test,colY,...){
  # mlogit <- glm(fo,family="binomial",data=train,...)
  # pred_mlogit <- predict(object=mlogit,newdata=test,type="response")
  # pm.logit.Y <- numeric(length=length(pred_mlogit))
  # pm.logit.Y[pred_mlogit>=0.5] <- 1
  # mdclogit <- table(test[,colY],pm.logit.Y)
  # resu.mdclogit <- mdcmed(mdclogit)
  # list(mlogit=mlogit,mdclogit=mdclogit,resu.mdclogit=resu.mdclogit)
# }
#En bloque dentro de asm2c
#logit[[i]] <- etm.logit(fo=fo,colY,train=datE,test=datT)

## Bloque SVM:
#' #@export
# etm.svm <- function(fo,train,test,colY,costsvm,kernel,...){
  # msvm <- svm(fo,data=train,cost=costsvm,kernel=kernel,decision.values=TRUE,...)
  # pred_msvm <- predict(object=msvm,newdata=test,decision.values=TRUE)
  # mdcsvm <- table(test[,colY],pred_msvm)
  # resu.mdcsvm <- mdcmed(mdcsvm)
  # list(msvm=msvm,mdcsvm=mdcsvm,resu.mdcsvm=resu.mdcsvm)
# }
#En bloque dentro de asm2c
#svm[[i]] <- etm.svm(fo=fo,colY,train=datE,test=datT)

## Bloque Naïve Bayes:
#' #@export
# etm.nb <- function(fo,train,test,colY,usekernel=TRUE,...){
  # mnb <- NaiveBayes(fo,data=train,usekernel=usekernel,...)
  # pred_mnb <- predict(object=mnb,newdata=test)$class
  # mdcnb <- table(test[,colY],pred_mnb)
  # resu.mdcnb <- mdcmed(mdcnb)
  # list(mnb=mnb,mdcnb=mdcnb,resu.mdcnb=resu.mdcnb)
# }
#En bloque dentro de asm2c
#nb[[i]] <- etm.nb(fo=fo,colY,train=datE,test=datT)

## Bloque Clasificador Bayesiano:
#' #@export
# etm.cdb <- function(fo,train,test,datafull,alfa1,iter,tol,...){
  # datFULLm <- model.frame(fo,data=datafull)
  # datEm <- model.frame(fo,data=train)
  # datTm <- model.frame(fo,data=test)
  # Ym <- 1
  # fkde_train <- dk(dfe=datEm,graf=FALSE,vclase=Ym) #efee
  # alftest <- emalg2k(alfa1=alfa1,iter=iter,datE=datEm[,-Ym],f1=fkde_train$f1KDE,
    # f2=fkde_train$f2KDE,f=fkde_train$f_KDE,tol=tol,
    # plota=FALSE,listalfa=TRUE)
  # fkde_test <- dk(dfe=datEm,graf=FALSE,vclase=Ym,dft=datTm) #efet
  # pred_cdb <- pape(alfa1=alftest$alfa1,f1=fkde_test$f1KDE$estimate,
    # f2=fkde_test$f2KDE$estimate,yr=datTm[,Ym],
    # x=fkde_test$f_KDE$eval.points,graf=FALSE,grr=FALSE)
  # mdccdb <- pred_cdb$MdConf
  # resu.mdccdb <- pred_cdb$Clasif
  # em.alftest <- c("alfa1"=alftest$alfa1,"alfa2"=alftest$alfa2)
  # list(f1test=fkde_test$f1KDE$estimate,f2test=fkde_test$f2KDE$estimate,
    # em.alftest=em.alftest,mdccdb=mdccdb,resu.mdccdb=resu.mdccdb)
# }
#En bloque dentro de asm2c
#cdb[[i]] <- etm.cdb(fo=fo,train=datE,test=datT,datafull=datFULL,alfa1=em.alfa10,iter=em.iter,tol=em.tol)

#############################################################

### Falta hacer:
#-- Y continua: calculo MSE
#-- Manejo de Excepciones: tryCatch(...)
#-- 1)"EM": meter ks::kde
#-- 2)k-fold CV: solo partis 1 vez; ideal partir muchas veces, calcular promedio+sd
#(matriz x metodo, listas globales... pensarlo bien!) --> graficos adecuados
#-- meter "mejor formulas glm" padentro
#-- 1)ver como meter productos de KDE (no indep), empezar con 1,2,3 variables luego gralizas
#-- metodo print.asm: tabla COMUN!!!
#3)(antes: solo table() 0,1 vs 0,1) ok
#-- ver Y desbalanceada o no?: si es asi, cambiar umbrales! (ej FY2 desb=SI, FY1=No)
#--4) imprimir curvas ROC ok
