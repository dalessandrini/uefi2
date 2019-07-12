############################################################
#### TESIS DA [Reunion 55]: script con funciones utiles ####
############################################################

#' emalg2k: calculo alfas usando algoritmo EM y KDE
#'
#' calcula valor de proporciones a priori/mezcla (alfa) mediante algoritmo EM
#'
#' @param alfa1 Valor inicial de \code{alfa1} para comenzar con algoritmo EM
#' @param datT Datos nuevos para hallar valores de alfa
#' @param f1,f2 Funciones de densidad calculadas con densidades por nucleo (KDE)
#' @param iter Cantidad de iteraciones
#' @param tol Tolerancia
#' @param plota Grafico de valores obtenidos
#' @param listalfa Valor booleano, TRUE si
#' @param ... parametros adicionales para modelos ML
#' @keywords emalg2k
#' @return objeto S3 de clase 'list'
#' @export
#' @examples
#' emalg2k()

emalg2k <- function(alfa1=0.5, datT, f1,f2, iter=10, tol=1e-2, plota=FALSE,listalfa=FALSE){
  #Temas de R: no devuelve todos los warnings temporalmente (variable global)
  oldw <- getOption("warn") #0 es "tiro todos los warnings"
  options(warn=-1) #lo paso a -1 temporalmente
  alfa2 <- 1-alfa1 #computo alfa2
  alfa01 <- alfa1 #guardo valores iniciales de alfa1,alfa2
  alfa02 <- alfa2
  alfalfa1 <- alfalfa2 <- normalfa <- vector() #prealocacion
  #E1.Extraigo de dk() f_(Xtest) separado x clase (asumimos solo 2 x ahora)
  f1kde <- f1 #dk me hace TODO el trabajo; aca solo corro EM
  f2kde <- f2
  #f1kde <- predict(object=f1,newdata=datT) #fitted(npudens(bws=obj.bws,newdata=datT))
  #f2kde <- predict(object=f2,newdata=datT) #PROBAR ANTES!!!
  #f_kde <- predict(object=f,x=datT) #**alfa*predict(f1,eval.pts=datos.test)** vector PREDICCIONES
  #Hasta no lograr convergencia: 'loop EMC'
  for(i in 1:iter){
    ###[PasoE] Dadas densidades cuanto valen variables latentes?
    #E1.Estimo f1,f2 x KDE (fuera de loop)
    #E2.Hallo gammas: gamma_j = P(clase_j)*fj_KDE/sumaj(P(clase_j)*fj_KDE)
    a1 <- alfa1 * f1kde #numeradores de gammas
    a2 <- alfa2 * f2kde
    p1 <- a1/(a1+a2) #P(clase1)
    p2 <- a2/(a1+a2)
    ###[PasoM] Dados {datos.obs,variables latentes}, cuanto valen parametros?
    #M1.Hallo nuevos alfas: promedio(gamma_j)
    alfa1 <- sum(p1)/length(p1)
    alfa2 <- sum(p2)/length(p2)
    #M2.Guardo alfas secuencialmente
    alfalfa1[i] <- alfa1
    alfalfa2[i] <- alfa2
    alfatodos1 <- c(alfa01,alfalfa1)
    alfatodos2 <- c(alfa02,alfalfa2)
    ###[PasoC] Chequeo de **Convergencia**
    #C1.Computar diferencia entre "nuevos" y "viejos" valores hallados
    #C2.Repetir 'EMC' entero si |nuevo-viejo|>tol, sino terminar
  }
  normalfa <- 2*abs(diff(alfatodos1))
  maxiter <- iter #FUERA DEL LOOP
  alfa1fin <- alfatodos1[maxiter+1]
  alfa2fin <- alfatodos2[maxiter+1]
  # Grafico para seguir evolucion de alfa1^(m) x iteracion 1:m
  if(plota){
    # Graficos f_kde
    #viejo <- par(mfrow=c(1,1))
    plot.new()
    par(mfrow=c(1,3))
    #Graf nuevo (MBo,PBe): graficar **mezcla** (no c/componente por separado)
    # -- pasar 'xx'=f correcto (seria f=(f1,f2) JUNTOS!!!)
    plot(f,alfatodos1[iter]*f1kde+(alfatodos2[iter])*f2kde,xlim=c(-7,14),ylim=c(0,.45),
      main=expression(paste("Grafico" ~ hat(f)^(KDE),"(x=datos.nuevos)")))
    #points(datos,f_kde,col="blue") #predict(object=f,x=datos) #comentado
    points(datT,f1kde,col="red")
    points(datT,f2kde,col="blue")
    #Graf nuevo (MBo,PBe): de aca p'abajo igual q antes!
    # Graficos alfa
    plot(x=1:maxiter,y=abs(alfatodos1[1:maxiter]),col="green",lty=1,xlab="Iteraciones (m=1,2,...)",
      main=expression(paste('Convergencia de ' ~ alpha[1]^(m))),#'al valor', bquote(alpha==.(alfa1fin)))),
      type="l",lwd=2,ylab=expression(paste('Evolucion de ' ~ alpha[1]^(m))))
    abline(h=alfatodos1[maxiter+1],lty=2,col="orange")
    plot(x=1:maxiter,y=abs(alfatodos2[1:maxiter]),col="brown",lty=1,xlab="Iteraciones (m=1,2,...)",
      main=expression(paste('Convergencia de ' ~ alpha[2]^(m))),#'al valor', bquote(bold(alpha==.(alfa2fin))))),
      type="l",lwd=2,ylab=expression(paste('Evolucion de ' ~ alpha[2]^(m))))
    abline(h=alfatodos2[maxiter+1],lty=2,col="orange")
    #Reseteo parametros graficos:
    #viejo
  }
  #C3.Fin algoritmo: devolucion a usuario *FALTA CLASE cbd_em EN SALIDA*
  conve <- if(min(normalfa)>=tol)
    paste("El algoritmo EM no converge con ", iter,"iteraciones para estos datos")
  else maxiter
  # Lista final:
  if(listalfa){
    return(list("alfa1"=alfa1fin,"alfa2"=alfa2fin, "maxiter"=conve,
      invisible(options(warn=oldw)),"listalfa1"=alfatodos1,"predf1"=f1kde,"predf2"=f2kde))
  } else {
    return(list("alfa1"=alfa1fin,"alfa2"=alfa2fin,"maxiter"=conve,
      invisible(options(warn=oldw)),"predf1"=f1kde,"predf2"=f2kde))
  }
}

### Función "emalg2k"
#-- Que hace: idem emalg2 (calcula valor de proporciones mediante algoritmo EM) pero con f_KDE
#-- Usada en: saber que porcion de mezcla de densidades corresponde a c/parte
#-- Otros: http://rstudio-pubs-static.s3.amazonaws.com/1001_3177e85f5e4840be840c84452780db52.html
#(implementada solo para k=2 densidades mezcla; se puede generalizar con procedimiento analogo)
