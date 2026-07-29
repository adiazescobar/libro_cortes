### Task 3: Añadir placebos e inferencia de sensibilidad

**Files:**
- Modify: `dofile/17_SyntheticControls/01_synthetic_controls.do`
- Create: `dofile/17_SyntheticControls/results/synth_placebos.csv`
- Create: `dofile/17_SyntheticControls/results/synth_time_placebo.csv`
- Create: `dofile/17_SyntheticControls/results/synth_leave_one_out.csv`
- Create: `dofile/17_SyntheticControls/synth_placebo_gaps.png`
- Create: `dofile/17_SyntheticControls/synth_rmspe_ratios.png`
- Create: `dofile/17_SyntheticControls/synth_time_placebo.png`
- Create: `dofile/17_SyntheticControls/synth_leave_one_out.png`

**Interfaces:**
- Consumes: misma especificación principal, `pre_rmspe` de California y lista de donantes elegibles.
- Produces: `synth_placebos.csv` con `unit_id,unit,pre_rmspe,post_rmspe,ratio,eligible,optimization`; `synth_time_placebo.csv` con `year,gap`; `synth_leave_one_out.csv` con `omitted_state,year,gap`.

- [ ] **Step 1: Escribir una prueba fallida que exija cobertura completa de placebos y leave-one-out.**

```python
def test_placebos_cover_donors_and_leave_one_out_covers_positive_weights():
    placebos = rows("synth_placebos.csv")
    assert len({r["unit"] for r in placebos}) == 39
    assert sum(r["unit"] == "California" for r in placebos) == 1
    positive = {r["state"] for r in rows("synth_weights.csv") if float(r["weight"]) > 1e-8}
    loo = {r["omitted_state"] for r in rows("synth_leave_one_out.csv")}
    assert loo == positive
```

- [ ] **Step 2: Ejecutar la prueba y confirmar falla por CSV inexistentes.**

Run: `python3 -m pytest -q tests/test_synthetic_controls_contract.py -k "placebo or leave_one_out"`

Expected: FAIL por `synth_placebos.csv` y `synth_leave_one_out.csv`.

- [ ] **Step 3: Programar placebos espaciales con la misma especificación.** Iterar cada `state_id` como unidad tratada, excluirla del donor pool de su propia corrida, mantener `trperiod(1989)` y los mismos predictores/años, calcular sus RMSPE y guardar una fila por unidad. Después definir:

```stata
scalar placebo_cutoff = 5*pre_rmspe_california
gen byte eligible = pre_rmspe <= placebo_cutoff
assert eligible == (pre_rmspe <= 5*pre_rmspe_california)
```

El bucle debe registrar fallas de convergencia por unidad y terminar con error si falta cualquiera de las 39 unidades; no debe omitir silenciosamente placebos.

**Excepción autorizada para StataNow 19.5:** Utah debe intentar primero la corrida canónica `nested` y registrar su `rc=430`. Solo para Utah, después de ese error, se autoriza reestimar con exactamente los mismos predictores, años y donor pool sin `nested`. El CSV debe marcar `optimization=nested` para 38 asignaciones y `optimization=default_fallback_after_rc430` únicamente para Utah.

- [ ] **Step 4: Calcular la proporción placebo descriptiva.** Crear en el log y el CSV una comparación de la razón de California frente a todas las unidades elegibles. Etiquetarla como `proporción de placebos elegibles con razón al menos tan grande`, no como p-valor convencional.

- [ ] **Step 5: Estimar el placebo temporal de 1980 sin fuga de información.** Reestimar con `trperiod(1980)`, usando `xperiod(1972(1)1979)` y únicamente predictores/resultados observados hasta 1979; exportar la brecha 1970–1988 a `synth_time_placebo.csv` y dibujar `synth_time_placebo.png` con línea en 1980.

- [ ] **Step 6: Ejecutar leave-one-out sobre cada donante con peso positivo.** Para cada estado positivo, reestimar California excluyéndolo del donor pool, exportar la brecha anual y verificar que el conjunto de `omitted_state` coincide exactamente con los pesos positivos de `synth_weights.csv`.

- [ ] **Step 7: Generar las cuatro gráficas de inferencia y sensibilidad.** Mostrar: brechas de todos los placebos con California destacada; distribución de razones RMSPE elegibles; placebo temporal; abanico leave-one-out con especificación principal destacada.

- [ ] **Step 8: Ejecutar Stata y validar esquemas y finitud.**

Run: `/Applications/StataNow/StataSE.app/Contents/MacOS/stata-se -b do 01_synthetic_controls.do`

Expected: 39 unidades en placebos, una fila de California, `eligible` consistente con el umbral 5×, ningún RMSPE no finito y un leave-one-out por donante de peso positivo.

- [ ] **Step 9: Ejecutar el contrato focalizado y commit.**

Run: `python3 -m pytest -q tests/test_synthetic_controls_contract.py -k "rmspe or placebo or leave_one_out"`

```bash
git add dofile/17_SyntheticControls tests/test_synthetic_controls_contract.py
git commit -m "feat: add synthetic-control placebos and sensitivity"
```
