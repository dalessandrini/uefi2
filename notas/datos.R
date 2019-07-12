#############################################################
### TESIS DA [Reunion 37]: descripcion de datos incluidos ###
#############################################################

#' R21_d05d08_EyT: datos HDI, CEAM, sociodemograficos y academicos 2005-2008
#'
#' Conjunto de datos de estudiantes FIng-UdelaR generaciones 2005 y 2008
#'
#' @format El conjunto tiene 4 data frames, agrupados en muestra de entrenamiento
#'  (datAAy.e) y de prueba (datAAy.t), siendo AA={05,08}
#' \describe{
#'  \item{SocioDemograficas}{variables de identificacion de cada estudiante, ejs:
#'   Sexo biologico, Origen (mezcla de fin de estudios secundarios con lugar de
#'   residencia anterior), EdadIngreso, Trabajo (si,no), etc.}
#'  \item{HDI}{variables relacionadas a la Herramienta Diagnostica al Ingreso,
#'  prueba obligatoria para nuevos estudiantes sobre diferentes conocimientos
#'  previos; separada por componentes Matematica, Fisica, Quimica, y agregado en
#'  Total.HDI}
#'  \item{Academicas}{relacionadas a rendimiento posterior al ingreso, ejs: CAMatxxxx:
#'  creditos acumulados relativos en Matematica al año xxxx, Y1_50pCurs1ro si
#'  el alumno llega al 50\% de aprobacion en cursos de primer año}
#'  \item{CEAM}{relacionadas al Cuestionario de Estrategias de Aprendizaje y
#'  Motivacion (2005-2016), tanto simples (MotXX) como agregadas en escalas
#'  psicometricas definidas previamente (MotML, EA, LC, etc.)}
#' }
#'
#' @source Datos internos
#'
#' @examples data('R21_d05d08_EyT'); summary("dat08y.e")
#' # Necesario juntar datos en un solo data.frame:
#' dat.e <- dat08y.e[complete.cases(dat08y.e[,c(2:3,76,6:9,27,33:75,81,82,16,17)]),]
#' dat.t <- dat08y.t[complete.cases(dat08y.t[,c(2:3,76,6:9,27,33:75,81,82,16,17)]),]
#' datfull <- rbind(dat.e,dat.t) #solo aquellos datos completos
#'
# "R21_d05d08_EyT" #NO sirve esto solo!
"dat05y.e"


#' R21_d05d08_EyT: datos HDI, CEAM, sociodemograficos y academicos 2005-2008
#'
#' Conjunto de datos de estudiantes FIng-UdelaR generaciones 2005 y 2008
#'
#' @format El conjunto tiene 4 data frames, agrupados en muestra de entrenamiento
#'  (datAAy.e) y de prueba (datAAy.t), siendo AA={05,08}
#' \describe{
#'  \item{SocioDemograficas}{variables de identificacion de cada estudiante, ejs:
#'   Sexo biologico, Origen (mezcla de fin de estudios secundarios con lugar de
#'   residencia anterior), EdadIngreso, Trabajo (si,no), etc.}
#'  \item{HDI}{variables relacionadas a la Herramienta Diagnostica al Ingreso,
#'  prueba obligatoria para nuevos estudiantes sobre diferentes conocimientos
#'  previos; separada por componentes Matematica, Fisica, Quimica, y agregado en
#'  Total.HDI}
#'  \item{Academicas}{relacionadas a rendimiento posterior al ingreso, ejs: CAMatxxxx:
#'  creditos acumulados relativos en Matematica al año xxxx, Y1_50pCurs1ro si
#'  el alumno llega al 50\% de aprobacion en cursos de primer año}
#'  \item{CEAM}{relacionadas al Cuestionario de Estrategias de Aprendizaje y
#'  Motivacion (2005-2016), tanto simples (MotXX) como agregadas en escalas
#'  psicometricas definidas previamente (MotML, EA, LC, etc.)}
#' }
#'
#' @source Datos internos
#'
#' @examples data('R21_d05d08_EyT'); summary("dat08y.e")
#' # Necesario juntar datos en un solo data.frame:
#' dat.e <- dat08y.e[complete.cases(dat08y.e[,c(2:3,76,6:9,27,33:75,81,82,16,17)]),]
#' dat.t <- dat08y.t[complete.cases(dat08y.t[,c(2:3,76,6:9,27,33:75,81,82,16,17)]),]
#' datfull <- rbind(dat.e,dat.t) #solo aquellos datos completos
#'
"dat05y.t"


#' R21_d05d08_EyT: datos HDI, CEAM, sociodemograficos y academicos 2005-2008
#'
#' Conjunto de datos de estudiantes FIng-UdelaR generaciones 2005 y 2008
#'
#' @format El conjunto tiene 4 data frames, agrupados en muestra de entrenamiento
#'  (datAAy.e) y de prueba (datAAy.t), siendo AA={05,08}
#' \describe{
#'  \item{SocioDemograficas}{variables de identificacion de cada estudiante, ejs:
#'   Sexo biologico, Origen (mezcla de fin de estudios secundarios con lugar de
#'   residencia anterior), EdadIngreso, Trabajo (si,no), etc.}
#'  \item{HDI}{variables relacionadas a la Herramienta Diagnostica al Ingreso,
#'  prueba obligatoria para nuevos estudiantes sobre diferentes conocimientos
#'  previos; separada por componentes Matematica, Fisica, Quimica, y agregado en
#'  Total.HDI}
#'  \item{Academicas}{relacionadas a rendimiento posterior al ingreso, ejs: CAMatxxxx:
#'  creditos acumulados relativos en Matematica al año xxxx, Y1_50pCurs1ro si
#'  el alumno llega al 50\% de aprobacion en cursos de primer año}
#'  \item{CEAM}{relacionadas al Cuestionario de Estrategias de Aprendizaje y
#'  Motivacion (2005-2016), tanto simples (MotXX) como agregadas en escalas
#'  psicometricas definidas previamente (MotML, EA, LC, etc.)}
#' }
#'
#' @source Datos internos
#'
#' @examples data('R21_d05d08_EyT'); summary("dat08y.e")
#' # Necesario juntar datos en un solo data.frame:
#' dat.e <- dat08y.e[complete.cases(dat08y.e[,c(2:3,76,6:9,27,33:75,81,82,16,17)]),]
#' dat.t <- dat08y.t[complete.cases(dat08y.t[,c(2:3,76,6:9,27,33:75,81,82,16,17)]),]
#' datfull <- rbind(dat.e,dat.t) #solo aquellos datos completos
#'
"dat08y.e"


#' R21_d05d08_EyT: datos HDI, CEAM, sociodemograficos y academicos 2005-2008
#'
#' Conjunto de datos de estudiantes FIng-UdelaR generaciones 2005 y 2008
#'
#' @format El conjunto tiene 4 data frames, agrupados en muestra de entrenamiento
#'  (datAAy.e) y de prueba (datAAy.t), siendo AA={05,08}
#' \describe{
#'  \item{SocioDemograficas}{variables de identificacion de cada estudiante, ejs:
#'   Sexo biologico, Origen (mezcla de fin de estudios secundarios con lugar de
#'   residencia anterior), EdadIngreso, Trabajo (si,no), etc.}
#'  \item{HDI}{variables relacionadas a la Herramienta Diagnostica al Ingreso,
#'  prueba obligatoria para nuevos estudiantes sobre diferentes conocimientos
#'  previos; separada por componentes Matematica, Fisica, Quimica, y agregado en
#'  Total.HDI}
#'  \item{Academicas}{relacionadas a rendimiento posterior al ingreso, ejs: CAMatxxxx:
#'  creditos acumulados relativos en Matematica al año xxxx, Y1_50pCurs1ro si
#'  el alumno llega al 50\% de aprobacion en cursos de primer año}
#'  \item{CEAM}{relacionadas al Cuestionario de Estrategias de Aprendizaje y
#'  Motivacion (2005-2016), tanto simples (MotXX) como agregadas en escalas
#'  psicometricas definidas previamente (MotML, EA, LC, etc.)}
#' }
#'
#' @source Datos internos
#'
#' @examples data('R21_d05d08_EyT'); summary("dat08y.e")
#' # Necesario juntar datos en un solo data.frame:
#' dat.e <- dat08y.e[complete.cases(dat08y.e[,c(2:3,76,6:9,27,33:75,81,82,16,17)]),]
#' dat.t <- dat08y.t[complete.cases(dat08y.t[,c(2:3,76,6:9,27,33:75,81,82,16,17)]),]
#' datfull <- rbind(dat.e,dat.t) #solo aquellos datos completos
#'
"dat08y.t"