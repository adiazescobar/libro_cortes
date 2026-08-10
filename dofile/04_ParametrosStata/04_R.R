# Parámetros causales: la misma demostración de Stata en R
library(haven)

calcular_estimandos <- function(datos) {
  media_y1 <- mean(datos$y[datos$D == 1])
  media_y0 <- mean(datos$y[datos$D == 0])
  naive <- media_y1 - media_y0
  att <- mean(datos$tau[datos$D == 1])
  data.frame(
    ATE = mean(datos$tau),
    ATT = att,
    ATU = mean(datos$tau[datos$D == 0]),
    CATE_X0 = mean(datos$tau[datos$X == 0]),
    CATE_X1 = mean(datos$tau[datos$X == 1]),
    NAIVE = naive,
    SESGO_ATT = naive - att
  )
}

construir_resultados <- function(datos) {
  datos$tau <- datos$yd1 - datos$yd0
  datos$y <- datos$D * datos$yd1 + (1 - datos$D) * datos$yd0
  datos
}

cat("\n=== 1. EJERCICIO MANUAL ===\n")
datos <- read_dta("04_data.dta")
datos$D <- as.numeric(datos$D)
datos$X <- as.numeric(seq_len(nrow(datos)) > 4)
datos <- construir_resultados(datos)
resultado_original <- calcular_estimandos(datos)
print(datos[c("X", "D", "yd0", "yd1", "y", "tau")])
print(resultado_original)

cat("\n=== 2. MISMA SELECCIÓN CON N = 80.000 ===\n")
datos_n80000 <- datos[rep(seq_len(nrow(datos)), each = 10000), ]
row.names(datos_n80000) <- NULL
resultado_n80000 <- calcular_estimandos(datos_n80000)
stopifnot(nrow(datos_n80000) == 80000)
stopifnot(isTRUE(all.equal(
  resultado_original[c("NAIVE", "SESGO_ATT")],
  resultado_n80000[c("NAIVE", "SESGO_ATT")],
  tolerance = 1e-12
)))
print(resultado_n80000)

cat("\n=== 3. UNA ASIGNACIÓN ALEATORIA ===\n")
set.seed(87634)
datos_aleatorios <- datos_n80000
datos_aleatorios$D <- as.numeric(runif(nrow(datos_aleatorios)) < 0.5)
datos_aleatorios <- construir_resultados(datos_aleatorios)
resultado_aleatorio <- calcular_estimandos(datos_aleatorios)
print(resultado_aleatorio)

cat("\n=== 4. MONTE CARLO: UN D NUEVO EN CADA REPETICIÓN ===\n")
set.seed(87634)
n_repeticiones <- 1000
estimadores_mc <- numeric(n_repeticiones)
for (repeticion in seq_len(n_repeticiones)) {
  D_nuevo <- as.numeric(runif(nrow(datos_n80000)) < 0.5)
  y_nuevo <- D_nuevo * datos_n80000$yd1 + (1 - D_nuevo) * datos_n80000$yd0
  estimadores_mc[repeticion] <- mean(y_nuevo[D_nuevo == 1]) -
    mean(y_nuevo[D_nuevo == 0])
}

ate <- resultado_original$ATE
error_mc <- sd(estimadores_mc) / sqrt(n_repeticiones)
stopifnot(abs(mean(estimadores_mc) - ate) < 3 * error_mc)
resumen_mc <- data.frame(
  ATE = ate,
  media_estimador = mean(estimadores_mc),
  desviacion_estandar = sd(estimadores_mc),
  error_estandar_media = error_mc,
  repeticiones = n_repeticiones
)
print(resumen_mc)
cat("\nR completado: solo D cambia entre asignaciones.\n")
