# ============================================================================
# Calculos de Poder Estadistico en R
# Econometria Avanzada - Ana Maria Diaz
# ============================================================================

# Cargar librerias necesarias
if (!require("pwr")) install.packages("pwr")
library(pwr)

# ============================================================================
# 1. Tamano de muestra para un efecto dado
# ============================================================================

# Parametros
power <- 0.8
alpha <- 0.05
effect_size <- 0.3  # Tamano del efecto en desviaciones estandar (Cohen's d)

# Calcular tamano de muestra para prueba t de dos muestras
result <- pwr.t.test(d = effect_size,
                     sig.level = alpha,
                     power = power,
                     type = "two.sample")
print(result)
cat("\nTamano de muestra requerido por grupo:", ceiling(result$n), "\n")
cat("Tamano de muestra total:", ceiling(result$n) * 2, "\n")

# ============================================================================
# 2. Efecto minimo detectable (MDE) para un N dado
# ============================================================================

N_total <- 1000  # Tamano de muestra total
n_per_group <- N_total / 2

# Calcular MDE
result_mde <- pwr.t.test(n = n_per_group,
                         sig.level = alpha,
                         power = power,
                         type = "two.sample")
cat("\nEfecto minimo detectable (Cohen's d):", round(result_mde$d, 4), "\n")

# ============================================================================
# 3. Curva de poder
# ============================================================================

# Crear grafico de poder vs tamano de muestra
n_range <- seq(50, 500, by = 10)
power_values <- sapply(n_range, function(n) {
  pwr.t.test(n = n, d = 0.3, sig.level = 0.05, type = "two.sample")$power
})

plot(n_range, power_values, type = "l",
     xlab = "Tamano de muestra por grupo",
     ylab = "Poder estadistico",
     main = "Curva de poder para d = 0.3",
     col = "blue", lwd = 2)
abline(h = 0.8, col = "red", lty = 2)
legend("bottomright", legend = c("Poder", "Poder = 0.80"),
       col = c("blue", "red"), lty = c(1, 2))

# ============================================================================
# 4. Comparacion de proporciones
# ============================================================================

# Parametros para proporciones
p1 <- 0.08  # Proporcion grupo control
p2 <- 0.12  # Proporcion grupo tratamiento

# Calcular tamano del efecto (h de Cohen)
h <- 2 * asin(sqrt(p2)) - 2 * asin(sqrt(p1))

# Calcular tamano de muestra
result_prop <- pwr.2p.test(h = h,
                           sig.level = alpha,
                           power = power)
print(result_prop)
cat("\nTamano de muestra por grupo:", ceiling(result_prop$n), "\n")

# ============================================================================
# 5. Funcion personalizada para calcular poder con covariables
# ============================================================================

# Incorporando la reduccion de varianza por covariables
power_with_covariates <- function(n, effect, sd_original, r_squared_cov, alpha = 0.05) {
  # Desviacion estandar residual despues de ajustar por covariables
  sd_residual <- sd_original * sqrt(1 - r_squared_cov)

  # Tamano del efecto ajustado
  d_adjusted <- effect / sd_residual

  # Calcular poder
  result <- pwr.t.test(n = n/2, d = d_adjusted, sig.level = alpha, type = "two.sample")
  return(result$power)
}

# Ejemplo: efecto de 0.5, sd = 2, R^2 de covariables = 0.3
poder_sin_cov <- pwr.t.test(n = 100, d = 0.5/2, sig.level = 0.05, type = "two.sample")$power
poder_con_cov <- power_with_covariates(n = 200, effect = 0.5, sd_original = 2, r_squared_cov = 0.3)

cat("\nPoder sin covariables (n=200):", round(poder_sin_cov, 4), "\n")
cat("Poder con covariables (n=200, R^2=0.3):", round(poder_con_cov, 4), "\n")

# ============================================================================
# 6. Diseno por conglomerados (clusters)
# ============================================================================

# Formula para efecto de diseno
design_effect <- function(icc, cluster_size) {
  return(1 + icc * (cluster_size - 1))
}

# Parametros
icc <- 0.05  # Correlacion intra-clase
m <- 50      # Tamano del cluster

de <- design_effect(icc, m)
cat("\nEfecto de diseno (ICC =", icc, ", m =", m, "):", round(de, 2), "\n")

# Tamano de muestra ajustado por clusters
n_simple <- ceiling(pwr.t.test(d = 0.3, sig.level = 0.05, power = 0.8, type = "two.sample")$n)
n_cluster <- ceiling(n_simple * de)
k_clusters <- ceiling(n_cluster / m)

cat("Tamano de muestra simple por grupo:", n_simple, "\n")
cat("Tamano de muestra ajustado por grupo:", n_cluster, "\n")
cat("Numero de clusters necesarios por grupo:", k_clusters, "\n")

cat("\n--- Fin del script de poder estadistico ---\n")
