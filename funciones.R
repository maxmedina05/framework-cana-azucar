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

filtro <- function (data, filterExp) {
  subset(data, filterExp)
}

calcularDistanciaEntreDosPuntos <- function(p1, p2) {
  distHaversine(p1, p2)
}

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

procesarDescargaCompartimiento <- function(Aef, h) {
  res = Aef * sqrt(2 * g * h)
}

constructorModelo <- function(hm, hd, Aef, Aint) {
  
  h <- 0
  
  function(Qp) {
    
    if(h > hm) {
      Qd <- Aef * sqrt(2 * GRAVEDAD * h)
      output <- c(Qd, h)
      return(output)
    }
    
    if(h < hm & h == hd) {
        
    }
  }
}