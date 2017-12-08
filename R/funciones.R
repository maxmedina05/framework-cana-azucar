library(geosphere)

cargarData <- function(ruta) {
  print("-- cargarData --")

  dataCampana     <- read.csv( file=paste(ruta, 'Campana.csv', sep="/"))
  dataSesion      <- read.csv( file=paste(ruta, 'Sesion.csv', sep="/"))
  dataMediciones  <- read.csv( file=paste(ruta, 'Mediciones.csv', sep="/"))
  dataLote        <- read.csv( file=paste(ruta, 'Lote.csv', sep="/"))
  dataPlanton     <- read.csv( file=paste(ruta, 'Planton.csv', sep="/"))
  dataPunto       <- read.csv( file=paste(ruta, 'PuntoCaptacion.csv', sep="/"))

  data <- list(dataCampana, dataSesion, dataMediciones, dataLote, dataPlanton, dataPunto)
  names(data) <- c("campana", "sesion","mediciones", "lote", "planton", "puntoCaptacion")
  data
}

#' Aplica filtro sobre la data especificada
#' @param data Data a filtrar
#' @param filterExp Filtro a aplicar
filtro <- function (data, filterExp) {
  subset(data, filterExp)
}

calcularDistanciaEntreDosPuntos <- function(p1, p2) {
  distHaversine(p1, p2)
}

#' Calcula la distancia entre dos puntos geograficos
#' @param lng1 longitud 1
#' @param lat latitud 1
#' @param lng2 longitud 2
#' @param lat2 latitud 2
calcularDistanciaEntreDosCoordenadas <- function(lng1, lat1, lng2, lat2) {
  rad <- pi/180
  a1 <- lat1 * rad
  a2 <- lng1 * rad
  b1 <- lat2 * rad
  b2 <- lng2 * rad
  dlon <- b2 - a2
  dlat <- b1 - a1
  a <- (sin(dlat/2))^2 + cos(a1) * cos(b1) * (sin(dlon/2))^2
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  R <- 6378.145
  d <- R * c
  return(d)
}

#' Hace backup de las colecciones de base de datos en un directorio especificado
#' @param directorio Nombre del directorio en el que se colocaran los archivos del backup
backup <- function(directorio) {
  STR_BASE_DATOS = "azucarDB"
  colecciones = c('campanas', 'sesions', 'medicions', 'lotes', 'plantons', 'puntocaptacions')
  for (colec in colecciones) {
    myMongo <- mongo(db=STR_BASE_DATOS, colec)
    dir.create(directorio, showWarnings = FALSE)
    filepath <- paste(directorio, '/', colec, '.json', sep="")
    f <- file(filepath)
    exported <- myMongo$export(f)
  }
}
