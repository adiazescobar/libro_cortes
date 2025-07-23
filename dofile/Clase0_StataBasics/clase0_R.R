
# Configuración inicial del documento
# Cargar las librerías necesarias
# La primera vez que lo uses, tendrás que instalar los paquetes
# install.packages("tidyverse")
# install.packages("haven")
# install.packages("knitr")

library(tidyverse)
library(haven)
library(knitr)

# Desactivar la notación científica para una mejor visualización de números grandes
options(scipen = 999)

# El comando 'use' de Stata se traduce a 'read_dta' de la librería haven.
# Si estás en RStudio, puedes usar una ruta local.
# Si estás en Colab, deberás montar Google Drive o usar una URL de GitHub.
# Para este ejemplo, usaremos una ruta local.
# Reemplaza la ruta con la de tu archivo.

# Ruta local
ruta_base <- "C:/Users/a.diaze/Dropbox/"
df_hh98 <- read_dta(paste0(ruta_base, "hh_98.dta"))

# Si usas una URL de GitHub, el código sería así:
# url_hh98 <- "[https://raw.githubusercontent.com/tu-usuario/tu-repo/main/hh_98.dta](https://raw.githubusercontent.com/tu-usuario/tu-repo/main/hh_98.dta)"
# df_hh98 <- read_dta(url_hh98)

# El comando 'clear' de Stata no es necesario en R, ya que los objetos
# se almacenan en el entorno de trabajo y se sobrescriben al reasignarlos.

# El comando 'use' de Stata se traduce a 'read_dta' de la librería haven.
# Si estás en RStudio, puedes usar una ruta local.
# Si estás en Colab, deberás montar Google Drive o usar una URL de GitHub.
# Para este ejemplo, usaremos una ruta local.
# Reemplaza la ruta con la de tu archivo.

# Ruta local
ruta_base <- "C:/Users/a.diaze/Dropbox/"
df_hh98 <- read_dta(paste0(ruta_base, "hh_98.dta"))

# Si usas una URL de GitHub, el código sería así:
# url_hh98 <- "[https://raw.githubusercontent.com/tu-usuario/tu-repo/main/hh_98.dta](https://raw.githubusercontent.com/tu-usuario/tu-repo/main/hh_98.dta)"
# df_hh98 <- read_dta(url_hh98)

# El comando 'clear' de Stata no es necesario en R, ya que los objetos
# se almacenan en el entorno de trabajo y se sobrescriben al reasignarlos.

# Stata: describe
# En R, las funciones 'glimpse' y 'summary' cubren la funcionalidad de 'describe' y 'codebook'.
print("--- describe y codebook ---")
glimpse(df_hh98)
summary(df_hh98)


# Stata: list in 1/3
print("--- list en R ---")
# Usamos 'head' para ver las primeras filas.
df_hh98 %>% head(3)


# Stata: list famsize educhead if (sexhead == 0 & agehead<45)
print("--- list con condiciones ---")
# Usamos el operador pipe (%>%) de 'dplyr' para encadenar operaciones.
# 'filter' es el equivalente a 'if'.
df_hh98 %>%
  filter(sexhead == 0 & agehead < 45) %>%
  select(famsize, educhead)


# Stata: count y count if agehead>50
print("--- count en R ---")
# Usamos 'nrow' para contar las filas.
n_total <- nrow(df_hh98)
n_mayor_50 <- df_hh98 %>%
  filter(agehead > 50) %>%
  nrow()

cat("Número total de observaciones:", n_total, "\n")
cat("Número de jefes de hogar mayores de 50:", n_mayor_50, "\n")


# Stata: sum famsize educhead, d
print("--- summarize con detalle ---")
# 'summary' proporciona las estadísticas básicas.
df_hh98 %>%
  select(famsize, educhead) %>%
  summary()


# Stata: by dfmfd: sum famsize educhead
print("--- summarize por grupo (by) ---")
# 'group_by' y 'summarise' son los equivalentes a 'by' de Stata.
df_hh98 %>%
  group_by(dfmfd) %>%
  summarise(
    mean_famsize = mean(famsize, na.rm = TRUE),
    mean_educhead = mean(educhead, na.rm = TRUE),
    sd_famsize = sd(famsize, na.rm = TRUE),
    sd_educhead = sd(educhead, na.rm = TRUE)
  )


# Stata: tab dfmfd
print("--- tabulate (tab) ---")
# La función 'table' o 'count' es el equivalente de 'tab'.
df_hh98 %>%
  count(dfmfd)


# Stata: tab dfmfd sexhead, col row
print("--- tabla de contingencia ---")
# 'table' para frecuencias, y 'prop.table' para porcentajes.
tabla_cruzada <- table(df_hh98$dfmfd, df_hh98$sexhead)
print(tabla_cruzada)

# Para porcentajes por columna (col) o fila (row)
print("Tabla con porcentajes por fila:")
prop.table(tabla_cruzada, 1)


# Stata: histogram agehead
print("--- histograma ---")
# Usamos 'ggplot2' para gráficos, que es parte de 'tidyverse'.
df_hh98 %>%
  ggplot(aes(x = agehead)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black") +
  labs(title = "Histograma de la edad del jefe de hogar",
       x = "Edad del jefe de hogar",
       y = "Frecuencia")


# Stata: twoway (scatter educhead agehead)
print("--- gráfico de dispersión (scatter plot) ---")
df_hh98 %>%
  ggplot(aes(x = agehead, y = educhead)) +
  geom_point() +
  labs(title = "Educación vs. Edad del jefe de hogar",
       x = "Edad del jefe de hogar",
       y = "Educación del jefe de hogar")


# Stata: correlate
print("--- correlación ---")
# 'cor' calcula la matriz de correlaciones.
df_hh98 %>%
  select(famsize, educhead, agehead) %>%
  cor(use = "complete.obs") # 'use' es para manejar valores faltantes.

# Stata: label values sexhead sexlabel
# 'haven' mantiene las etiquetas de Stata. Si se pierde, puedes usar 'factor'.
# Por ejemplo: df_hh98$sexhead_label <- factor(df_hh98$sexhead, labels = c("Mujer", "Hombre"))

# Stata: gen oldhead = 1 if agehead >50
# 'mutate' crea o modifica variables, y 'if_else' es el equivalente de 'if'
df_hh98 <- df_hh98 %>%
  mutate(oldhead = if_else(agehead > 50, 1, 0))

# Stata: egen avgagemf = mean(agehead), by(sexhead)
# En 'dplyr', 'group_by' y 'mutate' permiten calcular el promedio por grupo y
# añadirlo como una nueva columna.
df_hh98 <- df_hh98 %>%
  group_by(sexhead) %>%
  mutate(avgagemf = mean(agehead, na.rm = TRUE)) %>%
  ungroup() # Es importante "desagrupar" la base de datos después de la operación.

# Stata: keep if famsize <=6
# 'filter' es el equivalente de 'keep if'
df_hh98_keep <- df_hh98 %>%
  filter(famsize <= 6)


# Stata: drop dmmfd dfmfd
# 'select' es la función para 'keep' variables, y usar el signo '-' para 'drop'
df_hh98_drop <- df_hh98 %>%
  select(-dmmfd, -dfmfd)


# Stata: merge 1:1 nh using hh_98_2
# 'left_join' es una de las funciones para 'merge'
# Para simular la fusión, creamos dos dataframes y luego los unimos.
df_1 <- df_hh98 %>% select(nh, famsize, educhead)
df_2 <- df_hh98 %>% select(nh, dmmfd, dfmfd)
df_merged <- left_join(df_1, df_2, by = "nh")

print("--- Bases de datos unidas (merge) ---")
glimpse(df_merged)

# En R, las listas de caracteres o vectores son el equivalente de las macros.
# Stata: global control1 per001 per011
control1 <- c("famsize", "educhead")
control2 <- c(control1, "agehead")

# Stata: sum $control2
print("--- Sumarización usando 'macros' ---")
df_hh98 %>%
  select(all_of(control2)) %>%
  summary()


# Stata: foreach var in per001 per011 per019
print("--- Bucle 'foreach' en R ---")
for (var in control2) {
  media <- df_hh98 %>%
    pull(!!sym(var)) %>%
    mean(na.rm = TRUE)
  cat(sprintf("El promedio de la variable '%s' es: %.2f \n", var, media))
}