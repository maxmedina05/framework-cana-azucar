library(mongolite)
STR_BASE_DATOS = "azucarDB"

nombres = c('campana', 'sesion', 'medicion', 'lote', 'planton', 'puntocaptacion')

simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}

cargarFicheroABaseDeDatos <- function(ruta, nombre) {
  fichero <- read.csv(ruta, header=TRUE)
  colnames(fichero)[1] <- paste("id", simpleCap(nombre), sep="")
  myMongo <- mongo(db=STR_BASE_DATOS, paste(nombre, "s", sep=""))
  myMongo$insert(fichero)
}

cargarTodo <- function(RutaDirectorio) {
  for(i in 1:length(nombres)) {
    ruta = paste(RutaDirectorio, paste(nombres[i], "csv", sep="."), sep="/")
    print(ruta)
    cargarFicheroABaseDeDatos(ruta, nombres[i])
  }
}
