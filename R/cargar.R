#' Modulo de importación de la data en los CSV a la base de datos. Este Modulo utiliza mongoDB mediante mongolite.

library(mongolite)

# Nombre de la base de datos.
STR_BASE_DATOS = "azucarDB"

# Nombres escrito en singular que representan las colleciones de la base de datos.
nombres = c('campana', 'sesion', 'medicion', 'lote', 'planton', 'puntocaptacion')

#' Cambia el primer caracter de una cadena de caracteres a mayuscula.
#' @param x la cadena que se recibira los cambios.
#' @return una cadena de caracteres con el primer caracter en mayúscula.
simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}

#' Carga el contenido de un archivo CSV a una colleción en la base de datos.
#' @param ruta La ruta dónde se encuentra el fichero que se carga a la base de datos.
#' @param nombre El nombre en singular de la collecion donde se guarda la data contenida en el fichero.
cargarFicheroABaseDeDatos <- function(ruta, nombre) {
  fichero <- read.csv(ruta, header=TRUE)
  colnames(fichero)[1] <- paste("id", simpleCap(nombre), sep="")
  myMongo <- mongo(db=STR_BASE_DATOS, paste(nombre, "s", sep=""))
  myMongo$insert(fichero)
}

#' Carga en la base de datos, la data contenida en los archivos:
#' 1. Planton.csv: se especifican los datos referentes a uno o más plantones.
#' 2. Lote.csv: se especifican los datos referentes a uno o más lotes de cultivo de caña.
#' 3. PuntoCaptacion.csv: se especifican los datos referentes a los puntos de captación ubicados en un lote.
#' 4. Mediciones.csv: se especifican las mediciones realizadas durante una sesión.
#' 4. Sesion.csv: se especifican los datos de entrada referentes a una sesión de observación.
#' 5. Campana.csv: se especifican los datos de entrada referentes a una campaña de investigación.
#' @param rutaDirectorio La ruta del directorio que contiene los archivos especificados anteriormente.
cargarTodo <- function(rutaDirectorio) {
  for(i in 1:length(nombres)) {
    ruta = paste(rutaDirectorio, paste(nombres[i], "csv", sep="."), sep="/")
    print(ruta)
    cargarFicheroABaseDeDatos(ruta, nombres[i])
  }
}
