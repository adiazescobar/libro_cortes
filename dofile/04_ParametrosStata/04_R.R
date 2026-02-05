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

library(ggplot2)

# --------------------------
# Escenario 1: Con SELECCIÓN (viola independencia)
# --------------------------

cat("\n=== MONTE CARLO: Escenario 1 - CON SELECCIÓN ===\n")

set.seed(12345)
n_sims <- 1000
n_obs <- 1000

# Vector para almacenar resultados
resultados_seleccion <- data.frame(
  sim_id = 1:n_sims,
  naive_sesgo = NA,
  ate_sesgo = NA
)

# Loop de Monte Carlo
for (i in 1:n_sims) {

  # Generar resultados potenciales
  yd0 <- rnorm(n_obs, mean = 10, sd = 3)
  yd1 <- yd0 + rnorm(n_obs, mean = 2, sd = 1)

  # SELECCIÓN: Los que tienen mejor yd0 se tratan más
  prob_D <- plogis((yd0 - 10)/2)  # Mayor yd0 → mayor prob de D=1
  D <- as.numeric(runif(n_obs) < prob_D)

  # Generar resultado observado
  y <- D * yd1 + (1 - D) * yd0
  tau <- yd1 - yd0

  # Calcular estimadores
  ate_sim <- mean(tau)
  att_sim <- mean(tau[D == 1])
  naive_sim <- mean(y[D == 1]) - mean(y[D == 0])

  # Guardar resultados
  resultados_seleccion$naive_sesgo[i] <- naive_sim - ate_sim
  resultados_seleccion$ate_sesgo[i] <- att_sim - ate_sim

  # Mostrar progreso
  if (i %% 100 == 0) {
    cat("Simulación", i, "de", n_sims, "completada\n")
  }
}

# Resultados
cat("\n=== RESULTADOS CON SELECCIÓN (viola independencia) ===\n")
cat("Sesgo promedio del estimador Naive:", mean(resultados_seleccion$naive_sesgo), "\n")
cat("El sesgo persiste incluso con muchas observaciones!\n")

# Gráfico
ggplot(resultados_seleccion, aes(x = naive_sesgo)) +
  geom_histogram(bins = 30, fill = "red", alpha = 0.5) +
  geom_vline(xintercept = 0, color = "darkred", linewidth = 1) +
  labs(
    title = "Distribución del Sesgo del Estimador Naive",
    subtitle = "1000 simulaciones con SELECCIÓN",
    x = "Sesgo = Naive - ATE verdadero",
    caption = "Línea roja = sesgo cero (lo ideal)"
  ) +
  theme_minimal()
ggsave("sesgo_con_seleccion.png", width = 8, height = 6)

# --------------------------
# Escenario 2: Con ALEATORIZACIÓN (cumple independencia)
# --------------------------

cat("\n=== MONTE CARLO: Escenario 2 - CON ALEATORIZACIÓN ===\n")

set.seed(12345)

# Vector para almacenar resultados
resultados_aleat <- data.frame(
  sim_id = 1:n_sims,
  naive_sesgo = NA,
  ate_sesgo = NA
)

# Loop de Monte Carlo
for (i in 1:n_sims) {

  # Generar resultados potenciales (exactamente igual que antes)
  yd0 <- rnorm(n_obs, mean = 10, sd = 3)
  yd1 <- yd0 + rnorm(n_obs, mean = 2, sd = 1)

  # ALEATORIZACIÓN: D es independiente de yd0 y yd1
  D <- as.numeric(runif(n_obs) < 0.5)  # 50% tratamiento, 50% control

  # Generar resultado observado
  y <- D * yd1 + (1 - D) * yd0
  tau <- yd1 - yd0

  # Calcular estimadores
  ate_sim <- mean(tau)
  att_sim <- mean(tau[D == 1])
  naive_sim <- mean(y[D == 1]) - mean(y[D == 0])

  # Guardar resultados
  resultados_aleat$naive_sesgo[i] <- naive_sim - ate_sim
  resultados_aleat$ate_sesgo[i] <- att_sim - ate_sim

  # Mostrar progreso
  if (i %% 100 == 0) {
    cat("Simulación", i, "de", n_sims, "completada\n")
  }
}

# Resultados
cat("\n=== RESULTADOS CON ALEATORIZACIÓN (cumple independencia) ===\n")
cat("Sesgo promedio del estimador Naive:", mean(resultados_aleat$naive_sesgo), "\n")
cat("El sesgo es aproximadamente CERO!\n")

# Gráfico
ggplot(resultados_aleat, aes(x = naive_sesgo)) +
  geom_histogram(bins = 30, fill = "green", alpha = 0.5) +
  geom_vline(xintercept = 0, color = "darkgreen", linewidth = 1) +
  labs(
    title = "Distribución del Sesgo del Estimador Naive",
    subtitle = "1000 simulaciones con ALEATORIZACIÓN",
    x = "Sesgo = Naive - ATE verdadero",
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
ggplot(resultados_completos, aes(x = naive_sesgo, fill = escenario)) +
  geom_histogram(bins = 30, alpha = 0.5, position = "identity") +
  geom_vline(xintercept = 0, color = "black", linewidth = 1) +
  scale_fill_manual(values = c("Con selección" = "red", "Con aleatorización" = "green")) +
  labs(
    title = "Comparación: Sesgo con vs sin aleatorización",
    subtitle = "1000 simulaciones Monte Carlo",
    x = "Sesgo = Naive - ATE verdadero",
    fill = "Escenario"
  ) +
  theme_minimal()
ggsave("comparacion_monte_carlo.png", width = 10, height = 6)

cat("\n=== FIN DE LA SIMULACIÓN MONTE CARLO ===\n")

