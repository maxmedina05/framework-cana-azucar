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

filtro <- function (data, filterExp) {
  subset(data, filterExp)
}


