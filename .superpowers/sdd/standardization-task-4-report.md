# Task 4 report: validación integral y vista previa

## Alcance y estado inicial

- Directorio de trabajo y raíz Git:
  `/Users/adiazescobar/Dropbox/ClasesR/EconometriaAV/libro_cortes`.
- Baseline y `HEAD` verificados:
  `4d5ca5d1fd5d8934667592e04092761609640200`.
- Se preservaron todos los cambios ajenos preexistentes. No se modificaron
  artefactos publicados o renderizados bajo `docs/`. La especificación y el
  plan bajo `docs/superpowers/{specs,plans}` son las únicas excepciones de
  proceso expresamente autorizadas.
- La memoria compartida solicitada en
  `/Users/adiazescobar/Library/CloudStorage/Dropbox/Claude-Codex/MEMORY.md`
  no existe en este equipo; no se creó ni se sustituyó.

## Suite y contratos

En la raíz Git:

```bash
PYTHONDONTWRITEBYTECODE=1 \
  /private/tmp/libro_cortes_rct_venv/bin/python -m pytest -q
```

Resultado: `202 passed in 11.34s`.

Los contratos focales de privacidad:

```bash
PYTHONDONTWRITEBYTECODE=1 \
  /private/tmp/libro_cortes_rct_venv/bin/python -m pytest -q \
  tests/test_parametros_pedagogy_contract.py \
  tests/test_power_pedagogy_contract.py \
  tests/test_rct_pedagogy_contract.py \
  -k 'private or privacy or privacidad'
```

Resultado: `6 passed, 52 deselected`. Los helpers existentes también se
aplicaron a las rutas rastreadas y a todos los HTML del render limpio, sin
imprimir los fragmentos sensibles: `path_hits=0`, `content_hits=0`.

## Auditoría Stata y CSV

- `dofile/08_DID/08_DID.log` termina en `end of do-file`.
- La búsqueda anclada `^r\([0-9]+\);?$` no encontró códigos de error.
- El log es un artefacto local ignorado/no rastreado y, por tanto, no forma
  parte de `git archive HEAD`; se auditó en el árbol de trabajo sin editarlo.
- `did_resultados.csv` contiene exactamente los diez escenarios canónicos. El
  contrato exige que los únicos escenarios `did_*` sean `did_manual`,
  `did_diff` y `did_regresion`; no existe el escenario de panel retirado.
- `dofile/08_DID/verificar_did.py` produjo:

```text
did_manual PASS dif=0.00e+00
did_regresion PASS dif=2.86e-14
hospdd_atet PASS dif=2.70e-09
```

- La referencia `reg D.y D.D` visible en el log corresponde al estimador
  legítimo de primeras diferencias. No es el panel artificial retirado.

## Copia limpia y reproducibilidad

Se creó `/private/tmp/libro_cortes_standardization_review` exclusivamente con:

```bash
git archive HEAD | tar -x \
  -C /private/tmp/libro_cortes_standardization_review
```

SHA-256 del stream `git archive HEAD`:
`02c668f91a46584d37e5d5af563508526a07f36c70122a98df1e8afc3e604bfb`.
Se registraron hashes SHA-256 para 385 archivos en
`/private/tmp/libro_cortes_standardization_review_file_hashes.txt`.

Hashes fuente y copia limpia, idénticos en ambos árboles:

```text
e54561462bc1c93962789de399cbfbdfcb05ff668a308fcfd1502b4fdf55e4b0  07-POWER-Teoria.Rmd
4146aa01179cd95ae19f8ea39b5bbc657ad166e39ed9b4cafb301f1193f42671  07-POWER.Rmd
938b9c8ccab471dd55f3883f7cafda6b587d1a8103c3c92a16c5abc5043a1e8e  08-DID.Rmd
fc4f691451c24f8ea4533ecb3bb37f1d05d0d12c9871ed3dbcc51ceeb58bd3f1  08-DIDStata.Rmd
```

Como control adicional, la suite en el archive dio `199 passed, 3 failed`.
Los tres únicos fallos son ambientales y reproducibles: los contratos de
privacidad de DID, POWER y RCT ejecutan `git ls-files`, pero `git archive` no
incluye `.git`. Los mismos tres contratos pasan en la raíz Git y el helper
aplicado directamente al archive y sus HTML reporta cero exposiciones. No se
trata de un defecto del producto.

## Render limpio y QA HTML

Desde la copia limpia:

```bash
Rscript -e "bookdown::render_book(
  'index.Rmd',
  output_dir='/private/tmp/libro_cortes_standardization_review/_render'
)"
```

Resultado: exit 0 y `Output created: _render/index.html`. Se generaron:

- `poder-estadistico-teoria.html`
- `poder-estadistico-stata.html`
- `did-teoria.html`
- `did-stata.html`

Hashes SHA-256:

```text
56d67aec0d7c54d2c27eb7f9e9d2bc23909b8af389fae30e178d9c8bb89e542d  poder-estadistico-teoria.html
fb9be4dc4524a9f30da9903a4bc39893909b1ba18148987a83a9f37681e6fbb4  poder-estadistico-stata.html
de1f4cfda0eeab554e3ef4a667d80899bccbd34b87dbc9d951edd550c0e2817a  did-teoria.html
e1fc352ebab6ed90ad96d7d67dc977b14af619150e8b6edc332dd50042fe5b32  did-stata.html
```

QA estructural:

- Los cuatro H1 muestran el título exacto y una sola numeración Bookdown
  (`Capitulo 7` a `Capitulo 10`); los títulos del menú coinciden.
- Los anchors canónicos aparecen una vez cada uno y las URLs permanecen:
  `poder-estadistico-teoria`, `poder-estadistico-stata`, `did-teoria` y
  `did-stata`.
- En H1-H4 no quedó numeración manual después de retirar la numeración
  Bookdown del texto inspeccionado.
- La tabla comparativa DID contiene exactamente tres filas de métodos válidos.
- No aparecen `panel ficticio` ni `panel artificial`.
- `POWER-T1`–`POWER-T3` y `DID-T1`–`DID-T3` aparecen una vez cada uno en sus
  capítulos teóricos; las tablas se renderizan y los contratos de la suite
  confirman que no se exponen respuestas privadas.
- `Materiales para la clase` es el primer H2 de ambas clases empíricas.

## Alcance Git y conclusión

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

La auditoría del rango declarado permite únicamente la especificación y el plan
de proceso aprobados; cualquier otra ruta bajo `docs/` hace fallar el comando.
No se modificó HTML, Markdown generado ni ningún asset publicado. La corrección
de alcance y whitespace se comprometió separadamente, sin cambios de
producción; este informe documenta su verificación.

## Addendum de final review

La revisión final detectó que el chequeo original `HEAD~3..HEAD` no cubría los
dos commits iniciales del rango `bfa82fc..HEAD`. Se corrigieron la especificación,
el plan y este procedimiento para usar siempre el baseline declarado y un
allowlist exacto de las dos rutas de proceso autorizadas.

Verificación previa al commit:

```text
git diff --check bfa82fc
exit 0

allowlist de docs sobre bfa82fc..working-tree
unexpected_docs=NONE

pytest -q tests/test_chapter_title_contract.py \
  tests/test_did_pedagogy_contract.py \
  dofile/08_DID/tests/test_did_results.py
68 passed in 1.64s
```

También se eliminaron el whitespace final y las líneas en blanco al EOF de los
dos documentos de proceso. Los cambios locales ajenos permanecieron intactos.
