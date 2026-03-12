# ============================================================================
# MALOS CONTROLES EN R
# Econometría Avanzada — Ana María Díaz
# ============================================================================
# Regla de oro: controla variables que abren caminos espurios,
# pero NUNCA variables que están EN el camino causal o que son efectos
# tanto del tratamiento como del resultado.
# ============================================================================

# ============================================================================
# CASO 1: EL MEDIADOR — Controlar bloquea el efecto que quieres medir
# ============================================================================
# Ejemplo: Educación universitaria (D) → Tipo de empleo (M) → Salario (Y)
# Pregunta: ¿Cuánto sube el salario por tener título universitario?
# Problema: Si controlas el tipo de empleo, bloqueas el mecanismo por el cual
#           la educación sube el salario. El coeficiente colapsa hacia cero.
# ============================================================================

set.seed(2468)
n <- 10000

# Parámetros del proceso generador
efecto_educ_empleo     <- 2  # La educación mejora el tipo de empleo
efecto_empleo_salario  <- 1  # El tipo de empleo sube el salario
# Efecto TOTAL de educación en salario = 2 × 1 = 2

educacion   <- rbinom(n, 1, 0.5)                              # 1 = título universitario
tipo_empleo <- efecto_educ_empleo * educacion + rnorm(n)      # Mediador
salario     <- efecto_empleo_salario * tipo_empleo + rnorm(n) # Resultado

cat("=== CASO 1: MEDIADOR ===\n")
cat("Pregunta: ¿Cuánto sube el salario por tener título universitario?\n")

cat("\nRegresión CORRECTA — sin controlar el mediador (efecto TOTAL):\n")
modelo_total <- lm(salario ~ educacion)
print(summary(modelo_total)$coefficients)
cat("→ Coeficiente esperado: ~2 (efecto total correcto)\n")

cat("\nRegresión con MAL CONTROL — controlando el tipo de empleo:\n")
modelo_directo <- lm(salario ~ educacion + tipo_empleo)
print(summary(modelo_directo)$coefficients)
cat("→ Coeficiente de educacion ≈ 0. ¡El efecto 'desaparece'!\n")
cat("  La educación sube el salario SOLO A TRAVÉS del tipo de empleo.\n")
cat("  Al controlar el mediador, bloqueamos ese canal completamente.\n")

# ============================================================================
# CASO 2: EL COLISIONADOR — Controlar abre un camino espurio
# ============================================================================
# Ejemplo: Conexiones (D) y Productividad (Y) son independientes entre sí.
#          Ambas causan que alguien sea contratado (C = colisionador).
# Problema: Si analizas solo contratados (o controlas por C), aparece una
#           correlación negativa FALSA entre conexiones y productividad.
#           "Quienes entraron con palancas no necesitaban tanto mérito."
# ============================================================================

set.seed(2468)
n <- 5000

conexiones    <- rnorm(n)                                          # Palancas / red de contactos
productividad <- rnorm(n)                                          # Productividad real (independiente)
contratado    <- 2 * conexiones + 3 * productividad + rnorm(n)    # Colisionador

cat("\n\n=== CASO 2: COLISIONADOR ===\n")
cat("Pregunta: ¿Tienen conexiones y productividad alguna relación?\n")

cat("\nRegresión CORRECTA — sin controlar el colisionador:\n")
modelo_noC <- lm(productividad ~ conexiones)
print(summary(modelo_noC)$coefficients)
cat("→ Coeficiente esperado: ~0 (son independientes por construcción)\n")

cat("\nRegresión con MAL CONTROL — controlando ser contratado:\n")
modelo_conC <- lm(productividad ~ conexiones + contratado)
print(summary(modelo_conC)$coefficients)
cat("→ Coeficiente de conexiones es NEGATIVO: sesgo artificial.\n")
cat("  Al condicionar en 'contratado', aparece información espuria:\n")
cat("  entre los contratados, más conexiones implican menos mérito propio.\n")

# ============================================================================
# CASO 3: EL PROXY CONTAMINADO — El "control" mezcla confusor y efecto
# ============================================================================
# Ejemplo:
#   Habilidad innata (U, no observable) → Salario (Y)   [confusor]
#   Tratamiento D (programa) → Salario (Y)               [efecto que queremos]
#   Tratamiento D (programa) → Test tardío (P)           [PROBLEMA: P capta D]
#   Habilidad innata U       → Test tardío (P)           [P también capta U]
#
# Queremos controlar U. Usamos P como proxy. Pero P fue aplicado DESPUÉS
# del programa, así que mezcla habilidad + efecto del tratamiento.
# Al controlar P, absorbemos parte del efecto D que queremos medir.
# ============================================================================

set.seed(2468)
n <- 5000

habilidad_innata <- rnorm(n)                                              # U: no observable
tratamiento      <- rbinom(n, 1, 0.5)                                    # D: asignado aleatoriamente
salario          <- 2 * tratamiento + 1.5 * habilidad_innata + rnorm(n) # Y: efecto real = 2
test_tardio      <- 0.5 * tratamiento + habilidad_innata + rnorm(n, 0, 0.5) # P: proxy contaminado

cat("\n\n=== CASO 3: PROXY CONTAMINADO ===\n")
cat("Pregunta: ¿Cuánto sube el salario gracias al programa? (efecto real = 2)\n")

cat("\nRegresión CORRECTA — sin controlar (experimento aleatorio):\n")
modelo_sin_control <- lm(salario ~ tratamiento)
print(summary(modelo_sin_control)$coefficients)
cat("→ Coeficiente esperado: ~2\n")

cat("\nRegresión con el PROXY CONTAMINADO:\n")
modelo_proxy <- lm(salario ~ tratamiento + test_tardio)
print(summary(modelo_proxy)$coefficients)
cat("→ Coeficiente de tratamiento < 2. El proxy absorbe parte del efecto.\n")
cat("  El test capta tanto habilidad como efecto del programa: al controlarlo,\n")
cat("  'quitamos' parte del efecto que queríamos estimar.\n")

cat("\nRegresión IDEAL — controlando la habilidad innata directamente:\n")
modelo_ideal <- lm(salario ~ tratamiento + habilidad_innata)
print(summary(modelo_ideal)$coefficients)
cat("→ Coeficiente de tratamiento ≈ 2. Correcto: U es exógeno a D.\n")

# ============================================================================
# MONTE CARLO — Los tres casos (500 repeticiones)
# ============================================================================

cat("\n\n=== MONTE CARLO: Distribución de coeficientes (500 repeticiones) ===\n")

n_reps <- 500

# Caso 1: Mediador
b_total   <- numeric(n_reps)
b_directo <- numeric(n_reps)
for (i in 1:n_reps) {
  educ   <- rbinom(3000, 1, 0.5)
  empleo <- 2 * educ + rnorm(3000)
  sal    <- 1 * empleo + rnorm(3000)
  b_total[i]   <- coef(lm(sal ~ educ))[2]
  b_directo[i] <- coef(lm(sal ~ educ + empleo))[2]
}
cat("\nMediador — b_total (esperado ~2):", round(mean(b_total), 3),
    " | b_directo (esperado ~0):", round(mean(b_directo), 3), "\n")

# Caso 2: Colisionador
b_sinC <- numeric(n_reps)
b_conC <- numeric(n_reps)
for (i in 1:n_reps) {
  con  <- rnorm(1000)
  prod <- rnorm(1000)
  cont <- 2 * con + 3 * prod + rnorm(1000)
  b_sinC[i] <- coef(lm(prod ~ con))[2]
  b_conC[i] <- coef(lm(prod ~ con + cont))[2]
}
cat("Colisionador — b_sinC (esperado ~0):", round(mean(b_sinC), 3),
    " | b_conC (sesgado):", round(mean(b_conC), 3), "\n")

# Caso 3: Proxy contaminado
b_sinP  <- numeric(n_reps)
b_proxy <- numeric(n_reps)
b_ideal <- numeric(n_reps)
for (i in 1:n_reps) {
  U  <- rnorm(2000)
  D  <- rbinom(2000, 1, 0.5)
  Y  <- 2 * D + 1.5 * U + rnorm(2000)
  P  <- 0.5 * D + U + rnorm(2000, 0, 0.5)
  b_sinP[i]  <- coef(lm(Y ~ D))[2]
  b_proxy[i] <- coef(lm(Y ~ D + P))[2]
  b_ideal[i] <- coef(lm(Y ~ D + U))[2]
}
cat("Proxy — b_sinP:", round(mean(b_sinP), 3),
    " | b_proxy (atenuado):", round(mean(b_proxy), 3),
    " | b_ideal:", round(mean(b_ideal), 3), "\n")

# ============================================================================
# VISUALIZACIÓN — Histogramas Monte Carlo
# ============================================================================

par(mfrow = c(1, 3), mar = c(4, 4, 3, 1))

# Caso 1: Mediador
hist(b_total, col = rgb(0, 0, 1, 0.5), main = "Caso 1: Mediador",
     xlab = "Coef. de educacion", xlim = c(-0.5, 2.5), breaks = 30)
hist(b_directo, col = rgb(1, 0, 0, 0.5), add = TRUE, breaks = 30)
abline(v = 2, col = "blue", lty = 2, lwd = 2)
abline(v = 0, col = "red",  lty = 2, lwd = 2)
legend("topleft", cex = 0.8, bty = "n",
       legend = c("Correcto (~2)", "Mal control (~0)"),
       fill   = c(rgb(0,0,1,0.5), rgb(1,0,0,0.5)))

# Caso 2: Colisionador
hist(b_sinC, col = rgb(0, 0.6, 0, 0.5), main = "Caso 2: Colisionador",
     xlab = "Coef. de conexiones", xlim = c(-0.8, 0.4), breaks = 30)
hist(b_conC, col = rgb(1, 0.5, 0, 0.5), add = TRUE, breaks = 30)
abline(v = 0, col = "darkgreen", lty = 2, lwd = 2)
legend("topleft", cex = 0.8, bty = "n",
       legend = c("Correcto (~0)", "Mal control (sesgado)"),
       fill   = c(rgb(0,0.6,0,0.5), rgb(1,0.5,0,0.5)))

# Caso 3: Proxy contaminado
hist(b_sinP, col = rgb(0.5, 0, 0.8, 0.5), main = "Caso 3: Proxy contaminado",
     xlab = "Coef. de tratamiento", xlim = c(0.5, 3), breaks = 30)
hist(b_proxy, col = rgb(1, 0, 0, 0.5), add = TRUE, breaks = 30)
hist(b_ideal, col = rgb(0, 0.6, 1, 0.5), add = TRUE, breaks = 30)
abline(v = 2, col = "purple", lty = 2, lwd = 2)
legend("topleft", cex = 0.7, bty = "n",
       legend = c("Sin control (~2)", "Proxy malo (<2)", "Control ideal (~2)"),
       fill   = c(rgb(0.5,0,0.8,0.5), rgb(1,0,0,0.5), rgb(0,0.6,1,0.5)))

cat("\n=== Fin del script ===\n")
