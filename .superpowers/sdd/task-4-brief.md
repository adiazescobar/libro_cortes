### Task 4: Validación integral y vista previa

**Files:**
- Verify: all tracked source files
- Verify: clean render under `/private/tmp/libro_cortes_standardization_review`
- Do not modify: published/rendered artifacts under `docs/`; the approved
  standardization spec and plan are the only process-document exceptions.

**Interfaces:**
- Consumes: Tasks 1–3.
- Produces: evidencia de pruebas, reproducibilidad, privacidad y render lista para aprobación.

- [ ] **Step 1: Ejecutar suite completa**

Run:

```bash
/private/tmp/libro_cortes_rct_venv/bin/python -m pytest -q
```

Expected: cero fallos.

- [ ] **Step 2: Auditar Stata y CSV**

Confirmar que `08_DID.log` termina en `end of do-file`, no contiene errores
`r(...)`, el CSV no incluye el escenario retirado y las tres verificaciones
siguen en `PASS`.

- [ ] **Step 3: Auditar privacidad**

Ejecutar los contratos de privacidad existentes y buscar fragmentos privados
únicamente mediante los helpers que no imprimen tokens sensibles. Expected:
cero exposición en archivos rastreados y HTML.

- [ ] **Step 4: Renderizar desde una copia limpia**

Crear `/private/tmp/libro_cortes_standardization_review` desde `git archive HEAD`
y ejecutar:

```bash
Rscript -e "bookdown::render_book(
  'index.Rmd',
  output_dir='/private/tmp/libro_cortes_standardization_review/_render'
)"
```

Expected: exit 0 y HTML `poder-estadistico-teoria.html`,
`poder-estadistico-stata.html`, `did-teoria.html` y `did-stata.html`.

- [ ] **Step 5: Inspeccionar HTML**

Verificar:

- títulos exactos en H1 y menú;
- una sola numeración Bookdown por encabezado;
- anchors/URLs sin cambio;
- tres filas de métodos DID válidos;
- ausencia de panel ficticio;
- preguntas y tablas visibles sin respuestas;
- materiales al inicio de clases empíricas.

- [ ] **Step 6: Comprobar alcance Git**

Run:

```bash
git diff --check bfa82fc..HEAD
unexpected_docs="$(
  git diff --name-only bfa82fc..HEAD -- docs |
  while IFS= read -r path; do
    case "$path" in
      docs/superpowers/specs/2026-07-17-did-power-standardization-design.md|\
      docs/superpowers/plans/2026-07-17-did-power-standardization.md) ;;
      *) printf '%s\n' "$path" ;;
    esac
  done
)"
test -z "$unexpected_docs"
```

Expected: sin errores de whitespace y cero rutas inesperadas bajo `docs/`; solo
se permiten los dos documentos de proceso aprobados.

- [ ] **Step 7: Commit de correcciones finales si fueran necesarias**

Solo si la validación descubre un defecto, crear primero un contrato que falle,
aplicar la corrección mínima, repetir la validación y comprometer:

```bash
git commit -m "fix: address DID and title standardization review"
```
