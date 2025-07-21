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

# Solo si .git no existe
if (!dir.exists(".git")) {
  system("git init")
  system("git remote add origin https://github.com/adiazescobar/libro_cortes.git")
}

# Agregar todos los archivos nuevos o modificados
system("git add .")

# Commit con mensaje
system('git commit -m "Render y subida del libro completa"')

# Subir a GitHub
system("git branch -M main")
system("git push origin main")

# PASO 5: Confirmación final ----------------------------------------------------------
cat("✅ ¡Listo! Libro actualizado y subido a GitHub Pages.\n")
