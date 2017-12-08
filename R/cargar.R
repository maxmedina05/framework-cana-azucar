#' Modulo de importacion de la data en los CSV a la base de datos. Este Modulo utiliza mongoDB mediante mongolite.

library(mongolite)

# Nombre de la base de datos.
STR_BASE_DATOS = "azucarDB"

# Nombres escrito en singular que representan las colleciones de la base de datos.
nombres = c('campana', 'sesion', 'medicion', 'lote', 'planton', 'puntocaptacion')

#' Cambia el primer caracter de una cadena de caracteres a mayuscula.
#' @param x la cadena que se recibira los cambios.
#' @return una cadena de caracteres con el primer caracter en mayuscula.
simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}

#' Carga el contenido de un archivo CSV a una collecion en la base de datos.
#' @param ruta La ruta donde se encuentra el fichero que se carga a la base de datos.
#' @param nombre El nombre en singular de la collecion donde se guarda la data contenida en el fichero.
cargarFichero <- function(ruta, nombre) {
  fichero <- read.csv(ruta, header=TRUE)
  colnames(fichero)[1] <- paste("id", simpleCap(nombre), sep="")
  validarFichero(fichero, nombre)
  myMongo <- mongo(db=STR_BASE_DATOS, paste(nombre, "s", sep=""))
  myMongo$insert(fichero)
}

#' Carga en la base de datos, la data contenida en los archivos:
#' 1. Planton.csv: se especifican los datos referentes a uno o mas plantones.
#' 2. Lote.csv: se especifican los datos referentes a uno o mas lotes de cultivo de cana.
#' 3. PuntoCaptacion.csv: se especifican los datos referentes a los puntos de captacion ubicados en un lote.
#' 4. Mediciones.csv: se especifican las mediciones realizadas durante una sesion.
#' 4. Sesion.csv: se especifican los datos de entrada referentes a una sesion de observacion.
#' 5. Campana.csv: se especifican los datos de entrada referentes a una campana de investigacion.
#' @param rutaDirectorio La ruta del directorio que contiene los archivos especificados anteriormente.
cargarTodo <- function(rutaDirectorio) {
  for(i in 1:length(nombres)) {
    ruta = paste(rutaDirectorio, paste(nombres[i], "csv", sep="."), sep="/")
    cargarFichero(ruta, nombres[i])
  }
}

#' Hace las validaciones del fichero
#' @param fichero Objeto del fichero cargado
#' @param tipo Tipo de fichero: lote, sesion, etc
validarFichero <- function(fichero, tipo) {
  switch (tipo,
          'campana' = validarCampana(fichero),
          'lote' = validarLote(fichero),
          'planton' = validarPlanton(fichero),
          'puntocaptacion' = validarPuntoCaptacion(fichero),
          'medicion' = validarMedicion(fichero),
          'sesion' = validarSesion(fichero)
  )
}



# Validaciones de ficheros

#' Valida los datos de una campana
#' @param fichero Objeto del fichero cargado
validarCampana <- function(fichero) {
  fichero <- as.matrix(fichero)
  for (i in 1:nrow(fichero)) {
    # Se valida que el ID no exista en BD
    validarCod('campana', fichero[i,1])
    # Se valida la fecha de inicio
    validarFecha(fichero[i,2])
    # Se valida la fecha de fin
    validarFecha(fichero[i,3])
  }
}

#' Valida los datos de un lote
#' @param fichero Objeto del fichero cargado
validarLote <- function(fichero) {
  fichero <- as.matrix(fichero)
  for (i in 1:nrow(fichero)) {
    # Se valida que el ID no exista en BD
    validarCod('lote', fichero[i,1])
    # Se valida la fecha de plantacion
    validarFecha(fichero[i,2])
    # Se valida el valor de la latitud
    validarNumero(fichero[i,3])
    # Se valida el valor de la longitud
    validarNumero(fichero[i,4])
  }
}

#' Valida los datos de un planton
#' @param fichero Objeto del fichero cargado
validarPlanton <- function(fichero) {
  fichero <- as.matrix(fichero)
  for (i in 1:nrow(fichero)) {
    # Se valida que el ID no exista en BD
    validarCod('planton', fichero[i, 1])
    # Se valida el valor de la latitud
    validarNumero(fichero[i, 2])
    # Se valida el valor de la longitud
    validarNumero(fichero[i, 3])
    # Se valida el valor de la altura de las hojas
    validarPositivos(fichero[i, 4])
    # Se valida el valor del radio de interceptacion
    validarPositivos(fichero[i, 5])
    # Se valida el valor del id lote
    validarNumero(fichero[i, 6])
    # Se valida el valor del id sesion
    validarNumero(fichero[i, 7])
    # Se valida el valor del id campana
    validarNumero(fichero[i, 8])
    # Se valida el valor del borde
    validarNumeroBoolean(fichero[i, 9])
  }
}

#' Valida los datos de un punto de captacion
#' @param fichero Objeto del fichero cargado
validarPuntoCaptacion <- function(fichero) {
  fichero <- as.matrix(fichero)
  for (i in 1:nrow(fichero)) {
    # Se valida que el ID no exista en BD
    validarCod('puntocaptacion', fichero[i, 1])
    # Se valida el valor de la latitud
    validarNumero(fichero[i, 2])
    # Se valida el valor de la longitud
    validarNumero(fichero[i, 3])
    # Se valida el valor de la altura
    validarPositivos(fichero[i, 4])
    # Se valida el valor de la zona del punto de captacion
    puntos <- c('C', 'S')
    validarEnLista(puntos, fichero[i, 5])
    # Se valida el valor del id lote
    validarNumero(fichero[i, 6])
    # Se valida el valor del id campana
    validarNumero(fichero[i, 7])
  }
}

#' Valida los datos de las mediciones
#' @param fichero Objeto del fichero cargado
validarMedicion <- function(fichero) {
  fichero <- as.matrix(fichero)
  for (i in 1:nrow(fichero)) {
    # Se valida que el ID no exista en BD
    validarCod('medicion', fichero[i, 1])
    # Se valida el valor del id del punto de captacion
    validarNumero(fichero[i, 2])
    # Se valida el valor de la lamina
    validarPositivos(fichero[i, 3])
    # Se valida el valor del instante
    validarFecha(fichero[i, 4])
  }
}

#' Valida los datos de una sesion
#' @param fichero Objeto del fichero cargado
validarSesion <- function(fichero) {
  fichero <- as.matrix(fichero)
  for (i in 1:nrow(fichero)) {
    # Se valida que el ID no exista en BD
    validarCod('sesion', fichero[i, 1])
    # Se valida el valor del id lote
    validarNumero(fichero[i, 2])
    # Se valida el valor de la direccion del viento
    cardinals <- c('N', 'S', 'E', 'W', 'NW', 'SW', 'NE', 'SE', 'NNW', 'WNW', 'WSW', 'SSW', 'SSE', 'ESE', 'ENE', 'NNE')
    validarEnLista(cardinals, fichero[i,3])
    # Se valida el valor de velocidad del viento
    validarPositivos(fichero[i, 4])
    # Se valida el valor de la temperatura
    validarNumero(fichero[i, 5])
    # Se valida el valor del tipo de humedad
    tipos <- c('R', 'A')
    validarEnLista(tipos, fichero[i,6])
    # Se valida el valor de la humedad
    validarNumero(fichero[i, 7])
    # Se valida la fecha de inicio
    validarFecha(fichero[i,8])
    # Se valida la fecha de fin
    validarFecha(fichero[i,9])
    # Se valida el valor del id campana
    validarNumero(fichero[i, 10])
  }
}



# Validaciones de tipos de datos y valores

#' Realiza las validaciones de un numero como booleano (0 y 1)
#' @param num Valor a validar
validarNumeroBoolean <- function(num) {
  num <- validarNumero(num)
  if (num != 0 & num != 1) {
    stop(paste("Numero diferente de 0 y 1: ", num, sep = ""))
  }
}

#' Realiza las validaciones de un numero positivo
#' @param num Valor a validar
validarPositivos <- function(num) {
  num <- validarNumero(num)
  if (num < 0) {
    stop(paste("Numero no valido: ", num, sep = ""))
  }
}

#' Realiza las validaciones de un numero
#' @param num Valor a validar
validarNumero <- function(num) {
  numTemp <- as.numeric(num)
  if (is.na(numTemp)) {
    stop(paste("ID no es numerico: ", num, sep = ""))
  }
  return(numTemp)
}

#' Realiza las validaciones de fechas
#' @param fecha Valor a validar
validarFecha <- function(fecha) {
  fechaTemp <- as.Date(fecha, format = "%Y-%m-%d %H:%M:%S")
  if (is.na(fechaTemp)) {
    stop(paste("Fecha incorrecta: ", fecha, sep = ""))
  }
}

#' Realiza las validaciones del id de un registro
#' @param nombre Nombre de la coleccion en la que se validara si el id existe
#' @param cod ID a validar
validarCod <- function(nombre, cod) {
  nombreCol <- paste("id", simpleCap(nombre), sep = "")
  validarNumero(cod)
  collection <-
    mongo(db = STR_BASE_DATOS, collection = paste(nombre, "s", sep = ""))
  query <- paste('{"', nombreCol, '":', cod, "}", sep = "")
  codCount <- collection$count(query)
  if (codCount > 0) {
    stop(paste('ID ya existe en la base de datos: ', cod, sep = ""))
  }
}

#' Valida si un valor existe en la lista de valores permitidos
#' @param valores Valores permitidos
#' @param punto Valor a validar
validarEnLista <- function(valores, punto) {
  if(!is.element(punto, valores)) {
    stop(paste('Valor incorrecto: ', punto, sep = ""))
  }
}
