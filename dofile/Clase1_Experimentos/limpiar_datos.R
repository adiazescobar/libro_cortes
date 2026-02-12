# =============================================================================
# Limpieza y combinación de datos experimentales de múltiples semestres
# Cada semestre se hizo el mismo experimento:
#   - Grupo A (control): recibe un texto crítico de economía
#   - Grupo B (tratamiento): recibe definiciones de palabras difíciles en español
#   - Luego todos presentan el mismo quiz de vocabulario
# =============================================================================

library(haven)
library(readxl)
library(dplyr)

base_path <- tryCatch({
  normalizePath(file.path(dirname(sys.frame(1)$ofile), "..", ".."), mustWork = FALSE)
}, error = function(e) ".")
if (!file.exists(file.path(base_path, "_bookdown.yml"))) {
  base_path <- "."
}

# ─────────────────────────────────────────────────────────────────────────────
# 1) data.dta — Semestre 1 (N=15, 10 preguntas)
#    grupo B = definiciones (puntaje alto), grupo A = texto crítico
# ─────────────────────────────────────────────────────────────────────────────
d1 <- read_dta(file.path(base_path, "dofile/Clase1_Experimentos/data.dta"))

sem1 <- d1 %>%
  transmute(
    resultado = resultado,
    grupo     = grupo,                     # B=definiciones, A=texto → ya correcto
    edad      = edad,
    genero    = genero,                    # ya es "Mujer"/"Hombre"
    programa  = programa,                  # ya es "Pregrado"/"Maestría"
    libros    = libros,
    semestre  = 1L,
    n_preguntas = 10L
  )

cat("Semestre 1:", nrow(sem1), "obs. Grupo B(definiciones) mean =",
    round(mean(sem1$resultado[sem1$grupo=="B"]),2),
    "vs A(texto) mean =", round(mean(sem1$resultado[sem1$grupo=="A"]),2), "\n")

# ─────────────────────────────────────────────────────────────────────────────
# 2) experimento.dta — Semestre 2 (N=17, 10 preguntas)
#    grupo A = puntaje alto (definiciones), grupo B = puntaje bajo (texto)
#    → Hay que invertir etiquetas: A→B, B→A
# ─────────────────────────────────────────────────────────────────────────────
d2 <- read_dta(file.path(base_path, "kjhkh/experimento.dta"))

sem2 <- d2 %>%
  transmute(
    resultado = y,
    grupo     = ifelse(grupo == "A", "B", "A"),  # invertir para que B=definiciones
    edad      = edad,
    genero    = case_when(
      grepl("^Masc", genero) ~ "Hombre",
      grepl("^Fem",  genero) ~ "Mujer",
      TRUE ~ genero
    ),
    programa  = ifelse(grepl("^Maestr", programa), "Maestría",
                       ifelse(grepl("^Preg", programa), "Pregrado", programa)),
    libros    = libros,
    semestre  = 2L,
    n_preguntas = 10L
  )

cat("Semestre 2:", nrow(sem2), "obs. Grupo B(definiciones) mean =",
    round(mean(sem2$resultado[sem2$grupo=="B"]),2),
    "vs A(texto) mean =", round(mean(sem2$resultado[sem2$grupo=="A"]),2), "\n")

# ─────────────────────────────────────────────────────────────────────────────
# 3) experimento-1.dta — Semestre 3 (N=21, 8 preguntas, score en 0-100)
#    Blue (D=1) = puntaje alto (definiciones), Magenta (D=0) = texto
#    → Blue→B, Magenta→A. Usar columna 'correct' (0-8)
# ─────────────────────────────────────────────────────────────────────────────
d3 <- read_dta(file.path(base_path, "kjhkh/experimento-1.dta"))

sem3 <- d3 %>%
  transmute(
    resultado = correct,                    # 0-8 (número de correctas)
    grupo     = ifelse(D == 1, "B", "A"),   # Blue=definiciones→B
    edad      = edad,
    genero    = genero,                     # ya es "Mujer"/"Hombre"
    programa  = programa,                   # ya es "Pregrado"/"Maestría"
    libros    = libros,
    semestre  = 3L,
    n_preguntas = 8L
  )

cat("Semestre 3:", nrow(sem3), "obs. Grupo B(definiciones) mean =",
    round(mean(sem3$resultado[sem3$grupo=="B"]),2),
    "vs A(texto) mean =", round(mean(sem3$resultado[sem3$grupo=="A"]),2), "\n")

# ─────────────────────────────────────────────────────────────────────────────
# 4) experimento.xlsx — Semestre 4 (N=17, 13 preguntas)
#    grupo A = puntaje alto (definiciones), grupo B = puntaje bajo (texto)
#    → Invertir: A→B, B→A. Columna "11" es el score total.
# ─────────────────────────────────────────────────────────────────────────────
d4 <- read_excel(file.path(base_path, "kjhkh/experimento.xlsx"))

sem4 <- d4 %>%
  transmute(
    resultado = `11`,                       # score total (0-13)
    grupo     = ifelse(grupo == "A", "B", "A"),  # invertir
    edad      = edad,
    genero    = case_when(
      grepl("^Masc", genero) ~ "Hombre",
      grepl("^Fem",  genero) ~ "Mujer",
      TRUE ~ genero
    ),
    programa  = ifelse(grepl("^Maestr", grado), "Maestría",
                       ifelse(grepl("^Preg", grado), "Pregrado", grado)),
    libros    = libros,
    semestre  = 4L,
    n_preguntas = 13L
  )

cat("Semestre 4:", nrow(sem4), "obs. Grupo B(definiciones) mean =",
    round(mean(sem4$resultado[sem4$grupo=="B"]),2),
    "vs A(texto) mean =", round(mean(sem4$resultado[sem4$grupo=="A"]),2), "\n")

# ─────────────────────────────────────────────────────────────────────────────
# NOTA: "experimento fallido1.xlsx" (25-1) se EXCLUYE porque la clave de
# respuestas del quiz en Canvas estaba mal (respuestas correctas marcadas
# como incorrectas, e.g. "Insulto, ofensa" para Denuesto → r=0).
# ─────────────────────────────────────────────────────────────────────────────

# ─────────────────────────────────────────────────────────────────────────────
# Combinar todo
# ─────────────────────────────────────────────────────────────────────────────
datos <- bind_rows(sem1, sem2, sem3, sem4) %>%
  mutate(id = row_number()) %>%
  select(id, resultado, grupo, edad, genero, programa, libros, semestre, n_preguntas)

cat("\n=== RESUMEN COMBINADO ===\n")
cat("Total:", nrow(datos), "observaciones,", length(unique(datos$semestre)), "semestres\n\n")

datos %>%
  group_by(semestre) %>%
  summarise(
    n = n(),
    preguntas = first(n_preguntas),
    media_B = round(mean(resultado[grupo=="B"]), 2),
    media_A = round(mean(resultado[grupo=="A"]), 2),
    efecto  = round(mean(resultado[grupo=="B"]) - mean(resultado[grupo=="A"]), 2),
    .groups = "drop"
  ) %>%
  print()

# ─────────────────────────────────────────────────────────────────────────────
# Guardar
# ─────────────────────────────────────────────────────────────────────────────
out_path <- file.path(base_path, "dofile/Clase1_Experimentos/data.dta")

# Backup del original
file.copy(out_path, file.path(base_path, "dofile/Clase1_Experimentos/data_original_1semestre.dta"))

write_dta(datos, out_path)
cat("\nGuardado en:", out_path, "\n")
cat("Backup del original en: data_original_1semestre.dta\n")
