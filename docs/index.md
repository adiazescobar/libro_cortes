---
title: "Cortes Transversales"
subtitle: " Curso Avanzado de Microeconometría Aplicada"
author: "Ana María Díaz"
date: "2026-02-11"
site: bookdown::bookdown_site
documentclass: book
output:
  bookdown::gitbook:
    css: style.css
    split_by: chapter
---



# Programa {-}

Este curso pretende que el estudiante obtenga un conocimiento básico de los métodos econométricos de corte transversal. El estudiante aprenderá las principales técnicas y metodologías econométricas para realizar inferencia causal, útiles para evaluar programas y políticas públicas o analizar problemas económicos complejos.

También se busca familiarizar a los estudiantes con herramientas computacionales. Se utilizará el paquete estadístico **Stata**, y se realizará un proyecto de investigación empleando al menos dos técnicas aprendidas en el curso.

## Información general {-}

- **Docente:** Ana María Díaz  
- **Correo:** a.diaze@javeriana.edu.co  
- **Oficina:** Séptimo Piso Edificio 20  
- **Atención:** Lunes 9–11am (con cita previa o por Teams)  
- **Página web:** [adiazescobar.com](http://adiazescobar.com)

- **Días de clase:** Martes y Jueves  
- **Horario:** 7–9 am  
- **Lugar:** Por definir  
- **Monitoría:** Por definir  

## Requisitos {-}

- Econometría Avanzada

## Evaluación {-}

| Componente           | Porcentaje |
|----------------------|------------|
| Parcial 1            | 25%        |
| Parcial 2            | 25%        |
| Examen Final         | 25%        |
| Talleres de Clase    | 10%        |
| Trabajo Final        | 15%        |

**Trabajo Final:**
- Primera entrega: 10% (Presentación de la idea)
- Segunda entrega: 20% (Introducción + Descriptiva + Metodología)
- Documento final: 30%
- Sustentación: 40%

## Bibliografía {-}

### Libros Obligatorios {-}

- Cunningham, Scott (2020). *Causal Inference: The Mixtape*. [Enlace](http://scunning.com/cunningham_mixtape.pdf)
- Bernal, R. y Peña, X. (2011). *Guía Práctica para la Evaluación de Impacto*. Universidad de los Andes.

### Libros Recomendados {-}

1. Wooldridge (2002). *Econometric Analysis of Cross Section and Panel Data*. MIT Press.  
2. Angrist & Pischke (2009). *Mostly Harmless Econometrics*. Princeton.  
3. Cameron & Trivedi (2009). *Microeconometrics Using Stata*.  
4. Baker (2000). *Evaluating the Impact of Development Projects on Poverty*. World Bank.  
5. Heckman et al. (2000). *The Economics and Econometrics of Active Labor Market Programs*.  

## Inclusión {-}

Este curso da la bienvenida a personas de todas las edades, géneros, orientaciones, etnias, creencias y capacidades. Se espera un ambiente respetuoso, acogedor e inclusivo.

## Integridad Académica {-}

No se permite el uso de inteligencia artificial, internet o ayudas externas en evaluaciones. El incumplimiento será sancionado conforme al reglamento de la Universidad.

## Programa del Curso {-}

A continuación se presenta el programa semanal del curso, organizado por módulo.
<table class="table table-striped table-hover table-condensed table-responsive" style="width: auto !important; margin-left: auto; margin-right: auto;">
<caption>(\#tab:programa_tabla)Programa del curso de microeconometría aplicada</caption>
 <thead>
  <tr>
   <th style="text-align:center;font-weight: bold;color: white !important;background-color: rgba(31, 119, 180, 255) !important;"> Semana </th>
   <th style="text-align:center;font-weight: bold;color: white !important;background-color: rgba(31, 119, 180, 255) !important;"> Módulo </th>
   <th style="text-align:center;font-weight: bold;color: white !important;background-color: rgba(31, 119, 180, 255) !important;"> Temas </th>
   <th style="text-align:center;font-weight: bold;color: white !important;background-color: rgba(31, 119, 180, 255) !important;"> Lecturas </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:center;font-weight: bold;color: rgba(68, 68, 68, 255) !important;"> 1 </td>
   <td style="text-align:center;font-weight: bold;color: rgba(68, 68, 68, 255) !important;background-color: rgba(234, 234, 242, 255) !important;"> 1: Introducción a la Inferencia Causal </td>
   <td style="text-align:center;width: 20em; "> • Inferencia causal  
   • Contrafactual  
   • Parámetros de impacto </td>
   <td style="text-align:center;width: 20em; "> Bernal y Peña (2011), Cap. 2-3  
   Heckman (2008)  
   Angrist y Pischke </td>
  </tr>
  <tr>
   <td style="text-align:center;font-weight: bold;color: rgba(68, 68, 68, 255) !important;"> 2 </td>
   <td style="text-align:center;font-weight: bold;color: rgba(68, 68, 68, 255) !important;background-color: rgba(234, 234, 242, 255) !important;"> 1: Introducción a la Inferencia Causal </td>
   <td style="text-align:center;width: 20em; "> • Sesgo de selección </td>
   <td style="text-align:center;width: 20em; "> Angrist &amp; Krueger (2000) </td>
  </tr>
  <tr>
   <td style="text-align:center;font-weight: bold;color: rgba(68, 68, 68, 255) !important;"> 3 </td>
   <td style="text-align:center;font-weight: bold;color: rgba(68, 68, 68, 255) !important;background-color: rgba(234, 234, 242, 255) !important;"> 2: Métodos Experimentales </td>
   <td style="text-align:center;width: 20em; "> • Aleatorización  
   • Contrafactual  
   • Sesgo de selección </td>
   <td style="text-align:center;width: 20em; "> Bernal y Peña (2011), Cap. 4  
   Duflo et al. (2008)  
   Heckman et al. (1997) </td>
  </tr>
  <tr>
   <td style="text-align:center;font-weight: bold;color: rgba(68, 68, 68, 255) !important;"> 4 </td>
   <td style="text-align:center;font-weight: bold;color: rgba(68, 68, 68, 255) !important;background-color: rgba(234, 234, 242, 255) !important;"> 2: Métodos Experimentales </td>
   <td style="text-align:center;width: 20em; "> • Impacto con aleatorización  
   • Poder estadístico  
   • Problemas de aleatorización </td>
   <td style="text-align:center;width: 20em; "> Duflo et al. (2008) </td>
  </tr>
  <tr>
   <td style="text-align:center;font-weight: bold;color: rgba(68, 68, 68, 255) !important;"> 5 </td>
   <td style="text-align:center;font-weight: bold;color: rgba(68, 68, 68, 255) !important;background-color: rgba(234, 234, 242, 255) !important;"> 3: Métodos Cuasi-Experimentales </td>
   <td style="text-align:center;width: 20em; "> • Regresión lineal  
   • Supuestos de independencia condicional  
   • Variable dependiente binaria </td>
   <td style="text-align:center;width: 20em; "> Angrist y Pischke (2009), Cap. 3  
   Cameron &amp; Trivedi (2005), Cap. 14  
   Heckman (1990) </td>
  </tr>
  <tr>
   <td style="text-align:center;font-weight: bold;color: rgba(68, 68, 68, 255) !important;"> 6-7 </td>
   <td style="text-align:center;font-weight: bold;color: rgba(68, 68, 68, 255) !important;background-color: rgba(234, 234, 242, 255) !important;"> 4: Diferencias en Diferencias y Panel </td>
   <td style="text-align:center;width: 20em; "> • DID simple y de panel  
   • Efectos fijos  
   • Heterogeneidad en DID </td>
   <td style="text-align:center;width: 20em; "> Gertler et al. (2010), Cap. 6  
   Bertrand et al. (2004) </td>
  </tr>
  <tr>
   <td style="text-align:center;font-weight: bold;color: rgba(68, 68, 68, 255) !important;"> 8 </td>
   <td style="text-align:center;font-weight: bold;color: rgba(68, 68, 68, 255) !important;background-color: rgba(234, 234, 242, 255) !important;"> 4: Diferencias en Diferencias y Panel </td>
   <td style="text-align:center;width: 20em; "> • Efectos fijos y aleatorios  
   • Ventajas y desventajas del panel </td>
   <td style="text-align:center;width: 20em; "> Bernal y Peña (2011), Cap. 5  
   Angrist y Pischke (2009) </td>
  </tr>
  <tr>
   <td style="text-align:center;font-weight: bold;color: rgba(68, 68, 68, 255) !important;"> 9 </td>
   <td style="text-align:center;font-weight: bold;color: rgba(68, 68, 68, 255) !important;background-color: rgba(234, 234, 242, 255) !important;"> 5: Propensity Score Matching </td>
   <td style="text-align:center;width: 20em; "> • Estimación del PS  
   • Balanceo  
   • Soporte común </td>
   <td style="text-align:center;width: 20em; "> Bernal y Peña (2011), Cap. 6  
   Caliendo &amp; Kopeining (2008) </td>
  </tr>
  <tr>
   <td style="text-align:center;font-weight: bold;color: rgba(68, 68, 68, 255) !important;"> 10 </td>
   <td style="text-align:center;font-weight: bold;color: rgba(68, 68, 68, 255) !important;background-color: rgba(234, 234, 242, 255) !important;"> 5: Propensity Score Matching </td>
   <td style="text-align:center;width: 20em; "> • Algoritmos de emparejamiento  
   • Errores estándar  
   • Falsificación </td>
   <td style="text-align:center;width: 20em; "> Bernal y Peña (2011), Cap. 6  
   Caliendo &amp; Kopeining (2008) </td>
  </tr>
  <tr>
   <td style="text-align:center;font-weight: bold;color: rgba(68, 68, 68, 255) !important;"> 11 </td>
   <td style="text-align:center;font-weight: bold;color: rgba(68, 68, 68, 255) !important;background-color: rgba(234, 234, 242, 255) !important;"> 6: Variables Instrumentales </td>
   <td style="text-align:center;width: 20em; "> • Definición de IV  
   • LATE  
   • Estimación con variables continuas </td>
   <td style="text-align:center;width: 20em; "> Angrist y Pischke (2009), Cap. 4  
   Gertler et al. (2010), Cap. 7  
   Bernal y Peña (2011), Cap. 7 </td>
  </tr>
  <tr>
   <td style="text-align:center;font-weight: bold;color: rgba(68, 68, 68, 255) !important;"> 12 </td>
   <td style="text-align:center;font-weight: bold;color: rgba(68, 68, 68, 255) !important;background-color: rgba(234, 234, 242, 255) !important;"> 6: Variables Instrumentales </td>
   <td style="text-align:center;width: 20em; "> • IV con variables discretas  
   • PSM con IV  
   • Problemas del IV </td>
   <td style="text-align:center;width: 20em; "> Khandker et al. (2010), Cap. 6-7 </td>
  </tr>
  <tr>
   <td style="text-align:center;font-weight: bold;color: rgba(68, 68, 68, 255) !important;"> 13 </td>
   <td style="text-align:center;font-weight: bold;color: rgba(68, 68, 68, 255) !important;background-color: rgba(234, 234, 242, 255) !important;"> 7: Regresión Discontinua </td>
   <td style="text-align:center;width: 20em; "> • RDD lineal  
   • RDD no paramétrica  
   • Sharp y Fuzzy RDD </td>
   <td style="text-align:center;width: 20em; "> Gertler et al. (2010), Cap. 5  
   Bernal y Peña (2011), Cap. 8 </td>
  </tr>
  <tr>
   <td style="text-align:center;font-weight: bold;color: rgba(68, 68, 68, 255) !important;"> 14 </td>
   <td style="text-align:center;font-weight: bold;color: rgba(68, 68, 68, 255) !important;background-color: rgba(234, 234, 242, 255) !important;"> 8: Varios </td>
   <td style="text-align:center;width: 20em; "> • Función de control  
   • Machine Learning </td>
   <td style="text-align:center;width: 20em; "> Bernal y Peña (2011), Cap. 9-10 </td>
  </tr>
</tbody>
</table>






