# Introduccion a Stata en R
# Autor: Adria Diaz Escobar
# Fecha: 2026-04-23
# Objetivo: Replicar en R varios comandos basicos del capitulo con hh_98.dta.

suppressPackageStartupMessages({
  library(tidyverse)
  library(haven)
})

options(scipen = 999)

get_script_dir <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- "--file="
  match <- grep(file_arg, args, fixed = TRUE, value = TRUE)

  if (length(match) > 0) {
    return(dirname(normalizePath(sub(file_arg, "", match[1], fixed = TRUE))))
  }

  normalizePath(getwd())
}

script_dir <- get_script_dir()
data_path <- file.path(script_dir, "hh_98.dta")

if (!file.exists(data_path)) {
  stop("No se encontro hh_98.dta en: ", data_path)
}

df_hh98 <- read_dta(data_path)

cat("--- describe y codebook ---\n")
glimpse(df_hh98)
print(summary(df_hh98))

cat("--- list en R ---\n")
print(head(df_hh98, 3))

cat("--- list con condiciones ---\n")
print(
  df_hh98 %>%
    filter(sexhead == 0, agehead < 45) %>%
    select(famsize, educhead)
)

cat("--- count en R ---\n")
n_total <- nrow(df_hh98)
n_mayor_50 <- df_hh98 %>%
  filter(agehead > 50) %>%
  nrow()

cat("Numero total de observaciones:", n_total, "\n")
cat("Numero de jefes de hogar mayores de 50:", n_mayor_50, "\n")

cat("--- summarize con detalle ---\n")
print(
  df_hh98 %>%
    select(famsize, educhead) %>%
    summary()
)

cat("--- summarize por grupo (by) ---\n")
print(
  df_hh98 %>%
    group_by(dfmfd) %>%
    summarise(
      mean_famsize = mean(famsize, na.rm = TRUE),
      mean_educhead = mean(educhead, na.rm = TRUE),
      sd_famsize = sd(famsize, na.rm = TRUE),
      sd_educhead = sd(educhead, na.rm = TRUE),
      .groups = "drop"
    )
)

cat("--- tabulate (tab) ---\n")
print(df_hh98 %>% count(dfmfd))

cat("--- tabla de contingencia ---\n")
tabla_cruzada <- table(df_hh98$dfmfd, df_hh98$sexhead)
print(tabla_cruzada)
cat("Tabla con porcentajes por fila:\n")
print(prop.table(tabla_cruzada, 1))

grafico_hist <- df_hh98 %>%
  ggplot(aes(x = agehead)) +
  geom_histogram(binwidth = 5, fill = "steelblue", color = "black") +
  labs(
    title = "Histograma de la edad del jefe de hogar",
    x = "Edad del jefe de hogar",
    y = "Frecuencia"
  )

grafico_scatter <- df_hh98 %>%
  ggplot(aes(x = agehead, y = educhead)) +
  geom_point(alpha = 0.7) +
  labs(
    title = "Educacion vs. edad del jefe de hogar",
    x = "Edad del jefe de hogar",
    y = "Educacion del jefe de hogar"
  )

cat("--- correlacion ---\n")
print(
  df_hh98 %>%
    select(famsize, educhead, agehead) %>%
    cor(use = "complete.obs")
)

df_hh98 <- df_hh98 %>%
  mutate(oldhead = if_else(agehead > 50, 1, 0)) %>%
  group_by(sexhead) %>%
  mutate(avgagemf = mean(agehead, na.rm = TRUE)) %>%
  ungroup()

df_hh98_keep <- df_hh98 %>%
  filter(famsize <= 6)

df_hh98_drop <- df_hh98 %>%
  select(-dmmfd, -dfmfd)

df_1 <- df_hh98 %>% select(nh, famsize, educhead)
df_2 <- df_hh98 %>% select(nh, dmmfd, dfmfd)
df_merged <- left_join(df_1, df_2, by = "nh")

cat("--- Bases de datos unidas (merge) ---\n")
glimpse(df_merged)

control1 <- c("famsize", "educhead")
control2 <- c(control1, "agehead")

cat("--- Sumarizacion usando 'macros' ---\n")
print(
  df_hh98 %>%
    select(all_of(control2)) %>%
    summary()
)

cat("--- Bucle 'foreach' en R ---\n")
for (var in control2) {
  media <- df_hh98 %>%
    pull(!!sym(var)) %>%
    mean(na.rm = TRUE)
  cat(sprintf("El promedio de la variable '%s' es: %.2f\n", var, media))
}

save_plot <- function(plot_obj, filename) {
  primary_path <- file.path(script_dir, filename)
  fallback_path <- file.path(tempdir(), filename)

  saved_primary <- tryCatch(
    {
      suppressWarnings(
        ggsave(
          filename = primary_path,
          plot = plot_obj,
          width = 7,
          height = 5,
          dpi = 300
        )
      )
      file.exists(primary_path)
    },
    error = function(e) FALSE
  )

  if (saved_primary) {
    return(primary_path)
  }

  ggsave(
    filename = fallback_path,
    plot = plot_obj,
    width = 7,
    height = 5,
    dpi = 300
  )
  fallback_path
}

hist_path <- save_plot(grafico_hist, "hist_agehead_r.png")
scatter_path <- save_plot(grafico_scatter, "scatter_agehead_educhead_r.png")

cat("Grafico 1 guardado en:", hist_path, "\n")
cat("Grafico 2 guardado en:", scatter_path, "\n")
