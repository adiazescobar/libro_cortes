# ============================================================================
# Malos Controles en R
# Econometria Avanzada - Ana Maria Diaz
# ============================================================================

set.seed(2468)

# ============================================================================
# PARTE 1: MEDIADOR PURO (sobrecontrol del efecto total)
# DGP: D -> M -> Y
# ============================================================================

n <- 10000

# Parametros (efecto total = a*b = 2)
a <- 2  # D -> M
b <- 1  # M -> Y

D <- rbinom(n, 1, 0.5)
M <- a * D + rnorm(n)
Y <- b * M + rnorm(n)

cat("=== MEDIADOR PURO ===\n")
cat("\nRegresion sin mediador (efecto TOTAL):\n")
summary(lm(Y ~ D))

cat("\nRegresion con mediador (efecto DIRECTO - MAL CONTROL):\n")
summary(lm(Y ~ D + M))

# ============================================================================
# PARTE 2: COLISIONADOR CLASICO
# D y Y independientes; C es efecto de D y Y
# ============================================================================

set.seed(2468)
n <- 1000

D <- rnorm(n)
Y <- rnorm(n)
C <- 2*D - 0.5*Y + rnorm(n)

cat("\n=== COLISIONADOR ===\n")
cat("\nRegresion sin colisionador (CORRECTO):\n")
print(summary(lm(Y ~ D))$coefficients)

cat("\nRegresion con colisionador (MAL CONTROL - sesgo):\n")
print(summary(lm(Y ~ D + C))$coefficients)

# ============================================================================
# PARTE 3: MONTE CARLO - MEDIADOR
# ============================================================================

cat("\n=== MONTE CARLO: MEDIADOR (300 reps) ===\n")

n_reps <- 300
results_mediador <- data.frame(b_total = numeric(n_reps),
                                b_directo = numeric(n_reps))

for (i in 1:n_reps) {
  D <- rbinom(3000, 1, 0.5)
  M <- 2 * D + rnorm(3000)
  Y <- 1 * M + rnorm(3000)

  results_mediador$b_total[i] <- coef(lm(Y ~ D))[2]
  results_mediador$b_directo[i] <- coef(lm(Y ~ D + M))[2]
}

cat("\nPromedio b_total (esperado ~2):", mean(results_mediador$b_total), "\n")
cat("Promedio b_directo (esperado ~0):", mean(results_mediador$b_directo), "\n")

# ============================================================================
# PARTE 4: MONTE CARLO - COLISIONADOR
# ============================================================================

cat("\n=== MONTE CARLO: COLISIONADOR (300 reps) ===\n")

results_colisionador <- data.frame(b_noC = numeric(n_reps),
                                    b_conC = numeric(n_reps))

for (i in 1:n_reps) {
  D <- rnorm(1000)
  Y <- rnorm(1000)
  C <- 2*D - 0.5*Y + rnorm(1000)

  results_colisionador$b_noC[i] <- coef(lm(Y ~ D))[2]
  results_colisionador$b_conC[i] <- coef(lm(Y ~ D + C))[2]
}

cat("\nPromedio b_noC (esperado ~0):", mean(results_colisionador$b_noC), "\n")
cat("Promedio b_conC (sesgado):", mean(results_colisionador$b_conC), "\n")

# ============================================================================
# PARTE 5: VISUALIZACION
# ============================================================================

par(mfrow = c(1, 2))

# Histograma mediador
hist(results_mediador$b_total, col = rgb(0, 0, 1, 0.4),
     main = "Mediador: Coef de D", xlab = "Coeficiente",
     xlim = c(-0.5, 2.5), breaks = 30)
hist(results_mediador$b_directo, col = rgb(1, 0, 0, 0.4), add = TRUE, breaks = 30)
legend("topright", legend = c("Total (Y~D)", "Directo (Y~D+M)"),
       fill = c(rgb(0, 0, 1, 0.4), rgb(1, 0, 0, 0.4)))

# Histograma colisionador
hist(results_colisionador$b_noC, col = rgb(0, 1, 0, 0.4),
     main = "Colisionador: Coef de D", xlab = "Coeficiente",
     xlim = c(-0.5, 0.5), breaks = 30)
hist(results_colisionador$b_conC, col = rgb(1, 0.5, 0, 0.4), add = TRUE, breaks = 30)
legend("topright", legend = c("Sin C", "Con C"),
       fill = c(rgb(0, 1, 0, 0.4), rgb(1, 0.5, 0, 0.4)))

cat("\n--- Fin del script ---\n")
