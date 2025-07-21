# PASO 0: Cargar paquete necesario
if (!requireNamespace("bookdown", quietly = TRUE)) {
  stop("El paquete 'bookdown' no está instalado. Usa install.packages('bookdown')")
}

# PASO 1: Borrar carpetas temporales --------------------------------------------------
unlink(c("main_cache", "main_files", "_book", "docs"), recursive = TRUE)
cat("🧹 Cache borrado\n")

# PASO 2: Renderizar el libro ---------------------------------------------------------
bookdown::clean_book()
bookdown::render_book("index.Rmd", "bookdown::gitbook")
cat("📘 Libro renderizado en _book/\n")

# PASO 3: Copiar libro a carpeta docs/ ------------------------------------------------
dir.create("docs", showWarnings = FALSE)
system("rsync -av --delete _book/ docs/")
file.create("docs/.nojekyll")
cat("📂 Archivos copiados a docs/\n")

# PASO 4: Subir a GitHub --------------------------------------------------------------

# Inicializar repo si no existe
if (!dir.exists(".git")) {
  system("git init")
  cat("🚀 Git inicializado\n")
}

# Verificar si remote origin existe
remotes <- system("git remote", intern = TRUE)
if ("origin" %in% remotes) {
  current_url <- system("git remote get-url origin", intern = TRUE)
  correct_url <- "https://github.com/adiazescobar/libro_cortes.git"
  
  if (current_url != correct_url) {
    system("git remote remove origin")
    system(paste("git remote add origin", correct_url))
    cat("🔁 Remote origin corregido\n")
  } else {
    cat("🔍 Remote origin ya está correctamente configurado\n")
  }
} else {
  system("git remote add origin https://github.com/adiazescobar/libro_cortes.git")
  cat("🔗 Remote origin configurado\n")
}

# Agregar y subir cambios
system("git add .")
system('git commit -m "Render y subida del libro completa"')
system("git branch -M main")
system("git push -u origin main")

# PASO 5: Confirmación final ----------------------------------------------------------
cat("✅ ¡Listo! Libro actualizado y subido a GitHub Pages.\n")
