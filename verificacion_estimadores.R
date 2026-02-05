# Verificación de estimadores con los datos del ejemplo
# Capítulo 3: Parámetros

# Datos del ejemplo
datos <- data.frame(
  i = 1:8,
  yd0 = c(10, 4, 9, 10, 5, 3, 12, 5),
  yd1 = c(12, 5, 10, 11, 6, 2, 11, 7),
  D = c(1, 0, 1, 1, 0, 0, 1, 0)
)

print("Datos:")
print(datos)

# Resultado observado
datos$Y_obs <- ifelse(datos$D == 1, datos$yd1, datos$yd0)

cat("\n========================================\n")
cat("CÁLCULO DE ESTIMADORES\n")
cat("========================================\n\n")

# 1. ESTIMADOR NAIVE (comparación de medias observadas)
mean_treated <- mean(datos$Y_obs[datos$D == 1])
mean_control <- mean(datos$Y_obs[datos$D == 0])
naive <- mean_treated - mean_control

cat("1. ESTIMADOR NAIVE:\n")
cat("   E[Y|D=1] =", mean_treated, "\n")
cat("   E[Y|D=0] =", mean_control, "\n")
cat("   Naive = E[Y|D=1] - E[Y|D=0] =", naive, "\n\n")

# 2. ATE (Average Treatment Effect)
datos$efecto_individual <- datos$yd1 - datos$yd0
ate <- mean(datos$efecto_individual)

cat("2. ATE (Average Treatment Effect):\n")
cat("   Efectos individuales:", datos$efecto_individual, "\n")
cat("   ATE = E[yd1 - yd0] =", ate, "\n\n")

# 3. ATT (Average Treatment Effect on the Treated)
att <- mean(datos$efecto_individual[datos$D == 1])

cat("3. ATT (Average Treatment Effect on the Treated):\n")
cat("   Efectos en tratados:", datos$efecto_individual[datos$D == 1], "\n")
cat("   ATT = E[yd1 - yd0 | D=1] =", att, "\n\n")

# 4. ATU (Average Treatment Effect on the Untreated)
atu <- mean(datos$efecto_individual[datos$D == 0])

cat("4. ATU (Average Treatment Effect on the Untreated):\n")
cat("   Efectos en no tratados:", datos$efecto_individual[datos$D == 0], "\n")
cat("   ATU = E[yd1 - yd0 | D=0] =", atu, "\n\n")

# SESGO DE SELECCIÓN
sesgo <- naive - att

cat("========================================\n")
cat("SESGO DE SELECCIÓN\n")
cat("========================================\n")
cat("Sesgo = Naive - ATT =", sesgo, "\n\n")

# Verificación de valores esperados
cat("========================================\n")
cat("VERIFICACIÓN DE VALORES ESPERADOS\n")
cat("========================================\n")
cat("Naive esperado: 6.75 | Calculado:", naive, "| ✓:", naive == 6.75, "\n")
cat("ATE esperado: 0.75   | Calculado:", ate, "  | ✓:", ate == 0.75, "\n")
cat("ATT esperado: 0.75   | Calculado:", att, "  | ✓:", att == 0.75, "\n")
cat("ATU esperado: 0.75   | Calculado:", atu, "  | ✓:", atu == 0.75, "\n")
