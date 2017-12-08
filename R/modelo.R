# Variables Globales

# El estado de proceso de descarga
estaDescargando = FALSE

# Lámina acumulada.
h = 0

#' Especifica la ley que gobierna el proceso de descarga del compartimiento
procesarDescarga <- function(Aef, h) {
  Aef * sqrt(2 * GRAVEDAD * h)
}

#' Construye el modelo de interceptacion
#' @param hm Lamina minima (lamina mueta) Esta lamina no es evacuada nunca, aun cuando inciialmente este vacia.
#' @param hd Lamina maxima posible de acumular
#' @param Aef Area efectiva de descarga
#' @param Aef Area efectiva de interceptacion
#' @return una funcion que representa el modelo
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

    # if(Aint != (Qp - Qd)) {
    #   estaDescargando <<- FALSE
    #   print("Error: Aint dh/dt != Qp - Qd")
    # }

    if(estaDescargando) {
      Qd = procesarDescarga(Aef, h)
      # print(paste("Qd:", Qd))
      h <<- h - Qd
    }

    return(c(h, Qd))
  }
}

#' Procesa el modelo generado usando el metodo de "Euler" y una funcion ODE del pauqete DeSolve para generar una salida.
#' @param df Data Frame que contiene las mediciones capturadas durante la session de experimentacion
#' @param modelo es el modelo generado por la funcion previa 'generarModelo'
#' @return sol un objecto de DeSolve
procesarModelo <- function(df, modelo) {
  y0 = c(h = 0, Qd = 0)
  primeraVez <<- TRUE
  idx <<- 1

  f <- function(t, y, p, ...) {
    with(as.list(c(y, p)), {

      # Si es la primera vez entonces ignora
      if(primeraVez) {
        primeraVez = FALSE
        list(c(1,1))
      }
      else {
        salida <- modelo(df[idx, 'lamina'])
        idx <<- idx + 1
        list(c(salida[1] - h, salida[2] - Qd))
      }
    })
  }

  sol <- ode(y=y0, times=df$instante, func=f, parms = NULL, method="euler")
  sol
}

#' Genera la grafica de la h(t) y Qd(t)
#' @param df Data Frame que contiene el instante de tiempo y las mediciones de las laminas
#' @param hm Lamina minima (lamina mueta) Esta lamina no es evacuada nunca, aun cuando inciialmente este vacia.
#' @param hd Lamina maxima posible de acumular
#' @param Aef Area efectiva de descarga
#' @param Aef Area efectiva de interceptacion
generarGrafica <- function(df, hm, hd, Aef, Aint) {
  modelo <- generarModelo( hm, hd, Aef, Aint)
  sol <- procesarModelo(df, modelo)
  plot(sol)
}

df <- read.csv(file="data/medicion.csv", header=TRUE, stringsAsFactors = FALSE)
df$instante <- as.POSIXct(df$instante, format="%Y-%m-%d%H:%M")

