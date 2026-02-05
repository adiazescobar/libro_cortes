# Clase 4 - Estimadores Causales en Secciones Transversales
# Profesora: Ana Díaz

# --------------------------
# Cargar librerías y datos
# --------------------------
library(haven)
library(dplyr)

df <- read_dta("04_data.dta")

# Generar resultado observado
df <- df %>%
  mutate(y = ifelse(D == 1, yd1, yd0))

# --------------------------
# Estadísticas descriptivas
# --------------------------
table(df$D)
summary(df$y)
df %>% group_by(D) %>% summarise(media_y = mean(y), sd_y = sd(y))

# --------------------------
# Diferencia de medias y regresión
# --------------------------
t.test(y ~ D, data = df)
summary(lm(y ~ D, data = df))

# --------------------------
# Efecto individual (tau)
# --------------------------
df <- df %>%
  mutate(tau = yd1 - yd0)

# --------------------------
# Definir función estimadores
# --------------------------
estimadores <- function(tau, y, D) {
  ATE <- mean(tau)
  ATT <- mean(tau[D == 1])
  ATU <- mean(tau[D == 0])
  ybar_1 <- mean(y[D == 1])
  ybar_0 <- mean(y[D == 0])
  NAIVE <- ybar_1 - ybar_0
  
  cat("--- Estimadores ---\n")
  cat("ATE =", ATE, "\n")
  cat("ATT =", ATT, "\n")
  cat("ATU =", ATU, "\n")
  cat("Naive =", NAIVE, "\n")
  cat("Sesgo de Selección =", NAIVE - ATT, "\n")
}

# Ejecutar función
estimadores(df$tau, df$y, df$D)

# --------------------------
# Experimento 1: Aumentar muestra
# --------------------------
df2 <- df[rep(1:nrow(df), 10000), ]
df2 <- df2 %>%
  mutate(y = ifelse(D == 1, yd1, yd0),
         tau = yd1 - yd0)

estimadores(df2$tau, df2$y, df2$D)

# --------------------------
# Experimento 2: Asignación aleatoria
# --------------------------
set.seed(87634)
df3 <- df %>%
  mutate(D = as.numeric(runif(n()) > 0.5),
         y = ifelse(D == 1, yd1, yd0),
         tau = yd1 - yd0)

estimadores(df3$tau, df3$y, df3$D)

# ==================================================
# Experimento 3: Simulación Monte Carlo
# ==================================================

# ¿Por qué eliminamos el D original en cada simulación?
# Si NO eliminamos el D original, todas las 1000 simulaciones usarían
# exactamente la misma asignación de tratamiento y darían el mismo resultado.
# El Monte Carlo no tendría sentido.
#
# Lo que queremos es simular "¿qué pasaría si repetimos el estudio 1000 veces?":
# - En cada simulación mantenemos los mismos resultados potenciales (yd0, yd1)
# - Pero re-asignamos el tratamiento de forma diferente

library(ggplot2)

# --------------------------
# Escenario 1: Con SELECCIÓN (viola independencia)
# --------------------------

cat("\n=== MONTE CARLO: Escenario 1 - CON SELECCIÓN ===\n")

# Preparar datos de clase expandidos
df_clase <- read_dta("04_data.dta")
df_expandido <- df_clase[rep(1:nrow(df_clase), 10000), ]

set.seed(12345)
n_sims <- 1000

# Vector para almacenar resultados
resultados_seleccion <- data.frame(
  sim_id = 1:n_sims,
  SESGO = NA
)

# Loop de Monte Carlo
for (i in 1:n_sims) {

  # Usar datos expandidos de clase
  df_sim <- df_expandido

  # IMPORTANTE: Eliminamos el D original y creamos uno nuevo en cada simulación
  # Si no hacemos esto, todas las simulaciones darían el mismo resultado

  # SELECCIÓN: Los que tienen mejor yd0 se tratan más
  mean_yd0 <- mean(df_sim$yd0)
  prob_D <- plogis((df_sim$yd0 - mean_yd0)/2)  # Mayor yd0 → mayor prob de D=1
  D <- as.numeric(runif(nrow(df_sim)) < prob_D)

  # Generar resultado observado y efecto individual
  y <- D * df_sim$yd1 + (1 - D) * df_sim$yd0
  tau <- df_sim$yd1 - df_sim$yd0

  # Calcular estimadores (misma nomenclatura que en clase)
  ATE <- mean(tau)
  ATT <- mean(tau[D == 1])
  ybar_1 <- mean(y[D == 1])
  ybar_0 <- mean(y[D == 0])
  NAIVE <- ybar_1 - ybar_0

  # Guardar el sesgo de esta simulación
  # Recordar: NAIVE = ATT + SESGO, por lo tanto SESGO = NAIVE - ATT
  resultados_seleccion$SESGO[i] <- NAIVE - ATT

  # Mostrar progreso
  if (i %% 100 == 0) {
    cat("Simulación", i, "de", n_sims, "completada\n")
  }
}

# Resultados
cat("\n=== RESULTADOS CON SELECCIÓN (viola independencia) ===\n")
cat("Sesgo promedio del estimador Naive:", mean(resultados_seleccion$SESGO), "\n")
cat("El sesgo persiste incluso con muchas observaciones!\n")

# Gráfico
ggplot(resultados_seleccion, aes(x = SESGO)) +
  geom_histogram(bins = 30, fill = "red", alpha = 0.5) +
  geom_vline(xintercept = 0, color = "darkred", linewidth = 1) +
  labs(
    title = "Distribución del Sesgo del Estimador Naive",
    subtitle = "1000 simulaciones con SELECCIÓN - Datos de clase",
    x = "SESGO = NAIVE - ATT",
    caption = "Línea roja = sesgo cero (lo ideal)"
  ) +
  theme_minimal()
ggsave("sesgo_con_seleccion.png", width = 8, height = 6)

# --------------------------
# Escenario 2: Con ALEATORIZACIÓN (cumple independencia)
# --------------------------

cat("\n=== MONTE CARLO: Escenario 2 - CON ALEATORIZACIÓN ===\n")

# Cargar datos de clase y expandir (si no está ya cargado del escenario anterior)
df_clase <- read_dta("04_data.dta")
df_expandido <- df_clase[rep(1:nrow(df_clase), 10000), ]

set.seed(12345)

# Vector para almacenar resultados
resultados_aleat <- data.frame(
  sim_id = 1:n_sims,
  SESGO = NA
)

# Loop de Monte Carlo
for (i in 1:n_sims) {

  # Usar datos expandidos de clase
  df_sim <- df_expandido

  # IMPORTANTE: Eliminamos el D original y creamos uno nuevo en cada simulación
  # Si no hacemos esto, todas las simulaciones darían el mismo resultado

  # ALEATORIZACIÓN: D es independiente de yd0 y yd1
  D <- as.numeric(runif(nrow(df_sim)) < 0.5)  # 50% tratamiento, 50% control

  # Generar resultado observado y efecto individual
  y <- D * df_sim$yd1 + (1 - D) * df_sim$yd0
  tau <- df_sim$yd1 - df_sim$yd0

  # Calcular estimadores (misma nomenclatura que en clase)
  ATE <- mean(tau)
  ATT <- mean(tau[D == 1])
  ybar_1 <- mean(y[D == 1])
  ybar_0 <- mean(y[D == 0])
  NAIVE <- ybar_1 - ybar_0

  # Guardar el sesgo de esta simulación
  # Recordar: NAIVE = ATT + SESGO, por lo tanto SESGO = NAIVE - ATT
  resultados_aleat$SESGO[i] <- NAIVE - ATT

  # Mostrar progreso
  if (i %% 100 == 0) {
    cat("Simulación", i, "de", n_sims, "completada\n")
  }
}

# Resultados
cat("\n=== RESULTADOS CON ALEATORIZACIÓN (cumple independencia) ===\n")
cat("Sesgo promedio del estimador Naive:", mean(resultados_aleat$SESGO), "\n")
cat("El sesgo es aproximadamente CERO!\n")

# Gráfico
ggplot(resultados_aleat, aes(x = SESGO)) +
  geom_histogram(bins = 30, fill = "green", alpha = 0.5) +
  geom_vline(xintercept = 0, color = "darkgreen", linewidth = 1) +
  labs(
    title = "Distribución del Sesgo del Estimador Naive",
    subtitle = "1000 simulaciones con ALEATORIZACIÓN - Datos de clase",
    x = "SESGO = NAIVE - ATT",
    caption = "Línea verde = sesgo cero. ¡La distribución está centrada en cero!"
  ) +
  theme_minimal()
ggsave("sesgo_con_aleatorizacion.png", width = 8, height = 6)

# --------------------------
# Comparación lado a lado
# --------------------------

# Combinar datos
resultados_seleccion$escenario <- "Con selección"
resultados_aleat$escenario <- "Con aleatorización"
resultados_completos <- rbind(resultados_seleccion, resultados_aleat)

# Gráfico comparativo
ggplot(resultados_completos, aes(x = SESGO, fill = escenario)) +
  geom_histogram(bins = 30, alpha = 0.5, position = "identity") +
  geom_vline(xintercept = 0, color = "black", linewidth = 1) +
  scale_fill_manual(values = c("Con selección" = "red", "Con aleatorización" = "green")) +
  labs(
    title = "Comparación: Sesgo con vs sin aleatorización",
    subtitle = "1000 simulaciones Monte Carlo - Datos de clase",
    x = "SESGO = NAIVE - ATT",
    fill = "Escenario"
  ) +
  theme_minimal()
ggsave("comparacion_monte_carlo.png", width = 10, height = 6)

cat("\n=== FIN DE LA SIMULACIÓN MONTE CARLO ===\n")

