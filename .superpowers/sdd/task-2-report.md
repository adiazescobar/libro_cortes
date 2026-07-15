# Task 2 — Informe de implementación

## Alcance

- Archivo modificado: `03-Parametros.Rmd`.
- Contrato consumido: `tests/test_parametros_theory_contract.py` en los commits `1035a40` y `621eefe`.
- No se modificaron otros capítulos ni artefactos generados o sucios.

## Ciclo TDD

### Rojo

Comando:

```bash
/private/tmp/libro_cortes_rct_venv/bin/python -m pytest -q tests/test_parametros_theory_contract.py
```

Resultado inicial: 4 fallas y 1 prueba aprobada. Las fallas correspondían a los encabezados contractuales ausentes, la falta de CATE y de las relaciones de agregación, las definiciones incompletas de parámetros y la permanencia del prompt largo.

### Verde

El mismo comando produjo:

```text
.....                                                                    [100%]
5 passed in 0.04s
```

Verificación adicional:

```bash
Rscript -e "knitr::knit('03-Parametros.Rmd', output='/private/tmp/03-Parametros-task2.md', quiet=TRUE)"
git diff --check -- 03-Parametros.Rmd
```

Ambos comandos terminaron con código 0.

## Revisión académica

- La pregunta causal define primero población elegible, tratamiento binario y salario de seguimiento.
- Toda la notación transversal usa `Y_i(D=1)` y `Y_i(D=0)`; la comparación temporal usa consistentemente `Y_{it}(D=1)` y `Y_{it}(D=0)`.
- ATE, ATT, ATU y CATE están definidos y sus dos identidades de agregación están explicadas.
- En la muestra de ocho perfiles: ATE = ATT = ATU = 0,75; CATE(0) = 1,25; CATE(1) = 0,25; diferencia naïve = 6,75; sesgo respecto del ATT = 6. Los resultados satisfacen ambas identidades del ATE.
- La descomposición de la diferencia observada muestra explícitamente la suma y resta del contrafactual de los tratados y discute sesgo positivo, negativo y nulo.
- Independencia incondicional, independencia condicional y positividad están separadas. IV, RDD y DiD se presentan bajo sus propios supuestos de identificación.
- La sección antes-después distingue tiempo y tratamiento; SUTVA cubre interferencia y tratamiento bien definido con ejemplos de vacunación, redes y dosis.
- Se preservaron exactamente los dos videos y el prompt fue reemplazado por una actividad breve de cuatro respuestas.
- Hay síntesis, tres ejercicios y puente explícito al capítulo de Stata.

## Preocupaciones

Ninguna preocupación bloqueante. La verificación se limitó al test focal y al tejido aislado del capítulo para no regenerar ni alterar los numerosos artefactos sucios ya presentes en el repositorio.
