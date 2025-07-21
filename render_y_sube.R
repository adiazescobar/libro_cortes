# render_and_publish_book.R

# Cargar paquete necesario
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

# PASO 3: Copiar libro al folder docs/ para GitHub Pages -----------------------------
dir.create("docs", showWarnings = FALSE)
system("rsync -av --delete _book/ docs/")
cat("📂 Archivos copiados a docs/\n")

# PASO 4: Crear .nojekyll para evitar problemas con carpetas con "_" -----------------
file.create("docs/.nojekyll")

# PASO 5: Subir a GitHub -------------------------------------------------------------
system("git add .")
system('git commit -m "Render y subida del libro completa"')
system("git push origin main")

# PASO 6: Confirmación final ----------------------------------------------------------
cat("✅ ¡Listo! Libro actualizado y subido a GitHub Pages.\n")

