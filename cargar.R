library(mongolite)
STR_BASE_DATOS = "azucarDB"

ficheros = c('Campana.csv', 'Sesion.csv', 'Mediciones.csv', 'Lote.csv', 'Planton.csv', 'PuntoCaptacion.csv')
collecciones = c('campanas', 'sesiones', 'mediciones', 'lotes', 'plantones', 'puntocaptaciones')
nombres = c('campana', 'sesion', 'medicion', 'lote', 'planton', 'puntocaptacion')

cargarFicheroABaseDeDatos <- function(ruta, coleccion, nombre) {
  fichero <- read.csv(ruta, header=TRUE)
  colnames(fichero)[1] <- "cod"
  myMongo <- mongo(db=STR_BASE_DATOS, coleccion)
  myMongo$insert(fichero)
}

cargarTodo <- function(RutaDirectorio) {
  for(i in 1:length(ficheros)) {
    ruta = paste(RutaDirectorio, ficheros[i], sep="/")
    cargarFicheroABaseDeDatos(ruta, collecciones[i], nombres[i])
  }
}
