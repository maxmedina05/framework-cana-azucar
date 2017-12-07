# Variables Globales

# El estado de proceso de descarga
estaDescargando = FALSE
# Lámina acumulada.
h = 0

procesarDescarga <- function(Aef, h) {
  Aef * sqrt(2 * GRAVEDAD * h)
}

generarModelo <- function(hm, hd, Aef, Aint) {
  estaDescargando <<- FALSE
  h <<- 0
  Qd <- 0

  function(Qp) {
    h <<- h + Qp

    if(h >= hd) {
      estaDescargando <<- TRUE
    }

    if(h <= hm) {
      estaDescargando <<- FALSE
    }

    if(Aint != (Qp - Qd)) {
      estaDescargando <<- FALSE
      print("Error: Aint dh/dt != Qp - Qd")
    }

    if(estaDescargando) {
      Qd = procesarDescarga(Aef, h)
      print(paste("Qd:", Qd))
      h <<- h - Qd
    }
    return(c(Qd, h))
  }
}
