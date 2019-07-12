############################################################
#### TESIS DA [Reunion 56]: script con funciones utiles ####
############################################################

### Función 3 "dk"
#-- Que hace: halla KDE de datos
#-- Entra: datos (dfe,dft,variable clase),dens:funcion/paq usado para estimar f_KDE
#-- Sale: f_KDE

#' Densidad por nucleos para datos numericos o categoricos
#'
#' Entra: datos (dfe,dft,variable clase),dens:funcion/paq usado para estimar f_KDE
#' Sale: f_KDE
#'
#' (parrafos2-: van en Details, explayarse)
#'
#' @param dfe Datos de entrenamiento
#' @param dft Datos de prueba/testing
#' @param vclase Variable categorica de clase
#' @param dens Funcion de densidad KDE implementada en R. Opciones: stats::density,
#' ks::kde, np::npudens
#' @param ... No implementado
#' @export
dk <- function(dfe,dft=NULL,vclase=NULL,dens="density",...){
  if(!(dens %in% c("density","ks","np")))
	stop(paste("El parametro 'dens', que vale",dens,"debe ser alguno de estos: density, kde, np"))
  tipo <- class(dfe)
  ixc <- uefi2:::infoxcol(dfe)
  if(tipo=="list"){#pendiente: mejorarlo
    stop(paste("No implementamos", tipo, "aun"))
  }
  if(tipo=="numeric"){
    dfe <- data.frame(dfe)
    tipo <- class(dfe)
  }
  if(tipo=="data.frame"){
    nr <- nrow(dfe)
    nc <- ncol(dfe)
    datfalt <- which(!complete.cases(dfe))
    datfalt <- if(identical(datfalt,integer(0))) NULL else datfalt
    if(!is.null(vclase)){
      fac <- nif(dfe[,vclase])
      cn <- fac$cniv
      nm <- which(1:nc!=vclase)
      et <- factor(fac$niv) #OK hasta aca! (Sat Apr 15 17:44:04 2017)
      ## Datos numericos
      vct <- which(ixc$Clase %in% c("numeric","double","integer"))
      if(length(vct)>0){
        dfect <- data.frame(dfe[,vct]) #cuanTi
        dftct <- data.frame(dft[,vct]) #me lo morfe antes!
        lili1 <- split(dfect,f=as.factor(fac$orig))
        names(lili1) <- paste0("dfect",1:length(lili1))
        nuevenv1 <- list2env(lili1)
        list2env(lili1,envir=parent.env(env=nuevenv1))
		  if(dens=="np"){
			  #USAR: npudens(bws,tdat,edat)
			    for(i in 1:cn){#probl: si alguna variable es CONSTANTE! (salta)
				    assign(paste0("bws",i,collapse=""),np::npudensbw(get(paste0("dfect",i,collapse=""))))
				    assign(paste0("fy",i,"_Xtr",collapse=""),np::npudens(get(paste0("bws",i,collapse="")),tdat=get(paste0("dfect",i,collapse=""))))
				    assign(paste0("fy",i,"_Xte",collapse=""),np::npudens(get(paste0("bws",i,collapse="")),
					    tdat=get(paste0("dfect",i,collapse="")),edat=if(!is.null(dftct)) dftct else get(paste0("dfect",i,collapse=""))))
			    }
		  }
      if(dens=="density"){
			  for(i in 1:cn){
				  for(j in 1:ncol(dfect)){
					  assign(paste0("fy",i,"_Xtr",j,collapse=""),predict.hist(hh=density(get(paste0("dfect",i,collapse=""),envir=nuevenv1)[[j]]),
					    x=get(paste0("dfect",i,collapse=""),envir=nuevenv1)[[j]]))
					  assign(paste0("fy",i,"_Xte",j,collapse=""),predict.hist(hh=density(get(paste0("dfect",i,collapse=""),envir=nuevenv1)[[j]]),
						  x=if(!is.null(dftct)) get(paste0("dftct",collapse=""),envir=nuevenv1)[[j]] else get(paste0("dfect",i,collapse=""),envir=nuevenv1)[[j]]))
					  if((j-1)==0){
					    assign(paste0("actr",i,"[[",j,"]]",collapse=""),get(paste0("fy",i,"_Xtr",j,collapse=""))[[2]])
					    assign(paste0("acte",i,"[[",j,"]]",collapse=""),get(paste0("fy",i,"_Xte",j,collapse=""))[[2]])
					  } else {
					    assign(paste0("actr",i,"[[",j,"]]",collapse=""),cbind(get(paste0("actr",i,"[[",j-1,"]]",collapse="")),get(paste0("fy",i,"_Xtr",j,collapse=""))[[2]]))
					    assign(paste0("acte",i,"[[",j,"]]",collapse=""),cbind(get(paste0("acte",i,"[[",j-1,"]]",collapse="")),get(paste0("fy",i,"_Xte",j,collapse=""))[[2]]))
						}
					}
				  #[C]Producto X FILA de [B]
			    assign(paste0("f_actr",i,"_",collapse=""),data.frame(do.call("cbind",list(get(paste0("actr",i,"[[",ncol(dfect),"]]",collapse=""))))))
			    assign(paste0("fy",i,"_Xtr",collapse=""),Reduce(f='*',x=data.frame(get(paste0("f_actr",i,"_",collapse=""))),accumulate=FALSE))
			    assign(paste0("f_acte",i,"_",collapse=""),data.frame(do.call("cbind",list(get(paste0("acte",i,"[[",ncol(dfect),"]]",collapse=""))))))
			    assign(paste0("fy",i,"_Xte",collapse=""),Reduce(f='*',x=data.frame(get(paste0("f_acte",i,"_",collapse=""))),accumulate=FALSE))
				}
			} else if(dens=="ks"){
			    for(i in 1:cn){
				    for(j in 1:ncol(dfect)){
				      if(lapply(get(paste0("dfect",i,collapse=""),envir=nuevenv1)[[j]],IQR)==0){
				        assign(paste0("hf",i,"k",j,collapse=""),KernSmooth::dpik(x=as.numeric(as.matrix(get(paste0("dfect",i,collapse=""),envir=nuevenv1)[[j]])),
				          scalest="s"))
				        assign(paste0("fy",i,"_Xtr",j,collapse=""),ks::kde(x=get(paste0("dfect",i,collapse=""),envir=nuevenv1)[[j]],
				          eval.points=get(paste0("dfect",i,collapse=""),envir=nuevenv1)[[j]],h=get(paste0("hf",i,"k",j,collapse=""))))
				        assign(paste0("fy",i,"_Xte",j,collapse=""),ks::kde(x=get(paste0("dfect",i,collapse=""),envir=nuevenv1)[[j]],
				          eval.points=if(!is.null(dftct)) get(paste0("dftct",collapse=""),envir=nuevenv1)[[j]] else get(paste0("dfect",i,collapse=""),envir=nuevenv1)[[j]],
				          h=get(paste0("hf",i,"k",j,collapse=""))))
				      } else {
				        assign(paste0("fy",i,"_Xtr",j,collapse=""),ks::kde(x=get(paste0("dfect",i,collapse=""),envir=nuevenv1)[[j]],
				          eval.points=get(paste0("dfect",i,collapse=""),envir=nuevenv1)[[j]]))
				        assign(paste0("fy",i,"_Xte",j,collapse=""),ks::kde(x=get(paste0("dfect",i,collapse=""),envir=nuevenv1)[[j]],
				          eval.points=if(!is.null(dftct)) get(paste0("dftct",collapse=""),envir=nuevenv1)[[j]] else get(paste0("dfect",i,collapse=""),envir=nuevenv1)[[j]]))
				      }
					    if((j-1)==0){
					      assign(paste0("actr",i,"[[",j,"]]",collapse=""),get(paste0("fy",i,"_Xtr",j,collapse=""))[[3]])
					      assign(paste0("acte",i,"[[",j,"]]",collapse=""),get(paste0("fy",i,"_Xte",j,collapse=""))[[3]])
					    } else {
					      assign(paste0("actr",i,"[[",j,"]]",collapse=""),cbind(get(paste0("actr",i,"[[",j-1,"]]",collapse="")),get(paste0("fy",i,"_Xtr",j,collapse=""))[[3]]))
					      assign(paste0("acte",i,"[[",j,"]]",collapse=""),cbind(get(paste0("acte",i,"[[",j-1,"]]",collapse="")),get(paste0("fy",i,"_Xte",j,collapse=""))[[3]]))
					      }
					}
				  #[C]Producto X FILA de [B]
			      assign(paste0("f_actr",i,"_",collapse=""),data.frame(do.call("cbind",list(get(paste0("actr",i,"[[",ncol(dfect),"]]",collapse=""))))))
			      assign(paste0("fy",i,"_Xtr",collapse=""),Reduce(f='*',x=data.frame(get(paste0("f_actr",i,"_",collapse=""))),accumulate=FALSE))
			      assign(paste0("f_acte",i,"_",collapse=""),data.frame(do.call("cbind",list(get(paste0("acte",i,"[[",ncol(dfect),"]]",collapse=""))))))
			      assign(paste0("fy",i,"_Xte",collapse=""),Reduce(f='*',x=data.frame(get(paste0("f_acte",i,"_",collapse=""))),accumulate=FALSE))
			    }
			}
		}
    ## Datos categoricos
    vcl <- which(ixc$Clase %in% c("factor","character"))
    if(length(vcl)>1){
      dfecl <- data.frame(dfe[,vcl]) #cuaLi
      dftcl <- data.frame(dft[,vcl]) #idem!!
      nombcl <- names(dfecl)[-vclase]
      dfeclnoY <- data.frame(dfecl[,-vclase])
      dftclnoY <- data.frame(dftcl[,-vclase])
      names(dfeclnoY) <- names(dftclnoY) <- nombcl
      lili2 <- split(dfeclnoY,f=as.factor(fac$orig)) #asumimos 'Y' siempre categorica
      names(lili2) <- paste0("dfeclnoY",1:length(lili2))
      nuevenv2 <- list2env(lili2)
      list2env(lili2,envir=parent.env(env=nuevenv2))
      for(i in 1:cn){#de aca p'arriba OK (21:15 07/07/17)
        for(j in 1:ncol(dfeclnoY)){
          assign(paste0("nifxvar_",i,j,collapse=""),nif(get(paste0("dfeclnoY",i,collapse=""),envir=nuevenv2)[[j]]))
          assign(paste0("ffreq_",i,j,collapse=""),predict.nifM(object=get(paste0("nifxvar_",i,j,collapse="")),newdataM=get(paste0("dfeclnoY",i,collapse=""),envir=nuevenv2)[[j]]))
          assign(paste0("ffreq_f",i,j,collapse=""),predict.nifM(object=get(paste0("nifxvar_",i,j,collapse="")),
            newdataM=if(!is.null(dftcl)) dftclnoY[,j] else get(paste0("dfeclnoY",i,collapse=""),envir=nuevenv2)[[j]]))
          if((j-1)==0){
            assign(paste0("accu",i,"[[",j,"]]",collapse=""),get(paste0("ffreq_f",i,j,collapse="")))
          } else {
            assign(paste0("accu",i,"[[",j,"]]",collapse=""),cbind(get(paste0("accu",i,"[[",j-1,"]]",collapse="")),get(paste0("ffreq_f",i,j,collapse=""))))
          }
        }
        #[C]Producto X FILA de [B]
        assign(paste0("ffreq_acc",i,"_",collapse=""),data.frame(do.call("cbind",list(get(paste0("accu",i,"[[",ncol(dfeclnoY),"]]",collapse=""))))))
        #Producto solo categoricas
        assign(paste0("ffreq_df",i,"_",collapse=""),Reduce(f='*',x=data.frame(get(paste0("ffreq_acc",i,"_",collapse=""))),accumulate=FALSE))
        #Producto FINAL (separado x nivel de Y)
        assign(paste0("pf_ctcl_",i,collapse=""), get(paste0("ffreq_df",i,"_",collapse="")) * if(length(vct)>0) {
            switch(EXPR=dens,density=get(paste0("fy",i,"_Xte",collapse="")),
              np=get("dens",get(paste0("fy",i,"_Xte",collapse=""))),
              ks=get(paste0("fy",i,"_Xte",collapse=""))
            )
          } else 1)
		    }
		  }
    } else {
      stop("No implementamos nada sin variable de clase (Y)") #if(is.null(vclase)) pensar algo mejor
    }
  } #FALTA: salida 'ffreq_df,i,_' c/nombre x col
  fnum_salidaftr <- fnum_salidafte <- fcat_salida <- fmix_salida <- list()
  nomsalidaftr <- nomsalidafte <- nomfcat_salida <- nomfmix_salida <- vector()
  for(i in 1:cn){
    if(dens %in% c("density","ks")){
      fnum_salidaftr[[i]] <- if(length(vct)>0) list("fnum_Xtr_df"=get(paste0("f_actr",i,"_",collapse="")),
        "fnum_Xtr_prod"=get(paste0("fy",i,"_Xtr",collapse=""))) else NULL
      fnum_salidafte[[i]] <- if(length(vct)>0) list("fnum_Xte_df"=get(paste0("f_acte",i,"_",collapse="")),
        "fnum_Xte_prod"=get(paste0("fy",i,"_Xte",collapse=""))) else NULL
    } else {
      fnum_salidaftr[[i]] <- if(length(vct)>0) get(paste0("fy",i,"_Xtr",collapse="")) else NULL
      fnum_salidafte[[i]] <- if(length(vct)>0) get(paste0("fy",i,"_Xte",collapse="")) else NULL
    }
    fcat_salida[[i]] <- if(length(vcl)>1) list("fcat_df"=get(paste0("ffreq_acc",i,"_",collapse="")),
      "fcat_prod"=get(paste0("ffreq_df",i,"_",collapse=""))) else NULL
    fmix_salida[[i]] <- if(length(vcl)>1) list("pf_ctcl"=get(paste0("pf_ctcl_",i,collapse=""))) else NULL
    if(length(vct)>0){
      nomsalidaftr[i] <- paste0("fy",i,"_Xtr",collapse="")
      nomsalidafte[i] <- paste0("fy",i,"_Xte",collapse="")
      if(dens %in% c("density","ks"))
        names(fnum_salidafte[[i]]$fnum_Xte_df) <- names(dftct)
    }
    if(length(vcl)>1){
      names(fcat_salida[[i]]$fcat_df) <- names(dftclnoY) #else NULL
      nomfcat_salida[i] <- paste0("fcat_",i,collapse="")
      nomfmix_salida[i] <- paste0("fmix_",i,collapse="")
    }
  }
  attr(fnum_salidaftr,"names") <- nomsalidaftr
  attr(fnum_salidafte,"names") <- nomsalidafte
  attr(fcat_salida,"names") <- nomfcat_salida
  attr(fmix_salida,"names") <- nomfmix_salida
  tsf <- vector()
  if(length(vct)>0 & length(vcl)<=1)
    tsf <- "snum"
  else if(length(vct)<=0 & length(vcl)>1)
    tsf <- "scat"
  else
    tsf <- "mixt"
  ss <- list("f_num"=fnum_salidafte,"f_cat"=fcat_salida,"f_mix"=fmix_salida,
    datfalt=datfalt,dens=dens,tsf=tsf)
  if(length(vcl)>1)
    class(ss) <- c("cdb_dk_f",class(ss))
  else
    class(ss) <- c("cdb_dk",class(ss))
  return(ss)
}

# # Prueba: df=dn(lme$...) #df ES UNA FUNCION!!! NO USAR ESE NOMBRE!!!
# #c1 <- dk(df=b1[[1]][[1]],graf=TRUE,vclase=2,xlab="x_Entrenamiento")
# b1a <- b1$dat_me[[1]] #b1[[1]][[1]]
# par(mfrow=c(1,2))
# c2 <- dk(df=b1a,graf=TRUE,vclase=2,xlab="x_Entrenamiento")
# #c2 <- dk(df=b1,graf=TRUE,vclase=2) #NO usarlo!

# alf <- emalg2k(alfa1=0.1,iter=100,datos=b1a[,1],f1=c2$f1KDE,f2=c2$f2KDE,
#   tol=1e-5,plota=FALSE,listalfa=TRUE)
#
# b1b <- b1$dat_mt[[1]] #b1[[2]][[1]]
# nuevof <- dk(df=b1b,graf=TRUE,vclase=2,xlab="x_Test") #OK hasta aca!

############################################################

#**metodos print, plot**
print.cdb_dk <- function(x,...)
  stop(paste("No implementamos metodo print para la clase",sQuote(x=cdb_dk)))

plot.cdb_dk <- function(x,...)
  stop(paste("No implementamos metodo print para la clase",sQuote(x=cdb_dk)))

