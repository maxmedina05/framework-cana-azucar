library(mongolite)
STR_BASE_DATOS = "azucarDB"

cargarEnBaseDeDatos <- function(coleccion, ruta) {
  fichero <- read.csv(ruta, header=TRUE)
  myMongo <- mongo(db=STR_BASE_DATOS, coleccion)
  myMongo$insert(fichero)
}

