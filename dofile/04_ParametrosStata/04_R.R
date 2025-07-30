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

