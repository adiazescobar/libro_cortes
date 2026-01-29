*Introduccion a Stata

/*Tipos de archivos
En Stata existen cuatro tipos de archivos:
.dta contiene la base de datos
.do  contiene los programas desarrollados por los usuarios
.log contiene la información de lo que se realiza en Stata
.ado contiene los programas desarrollados por especialistas (por ahora nos vamos a olvidar de ellos)

Para mantener los archivos organizados es recomendable crear una estructura para guardar los archivos
c:\eval\
c:\eval\data
c:\eval\do
c:\eval\log
*/

/*Descripción de los datos que vamos a emplear

Vamos a emplear una base de datos de la encuesta de hogares de Bangladesh realizada por el Banco Mundial 
y un centro de Investigación local 

hh_91.dta 	este archivo tiene información que contiene 826 datos de hogares en 1991. Tiene 24 variables sobre información del hogar
			(educación de la cabeza de familia, propiedad del lugar de vivienda, gastos, entre otros) e información sobre el lugar de
			residencia (infraestructura, precios de los bienes de consumo principales, entre otros) 
hh_98.dta	este es la versión panel de hh_91.dta. incluye 303 nuevos hogares, haciendo una muestra de 1129. Esta base incluye las
			mismas variables que hh_91.dta
hh_9198.dta este es un datos de panel con 826 hogares 
*/

*****************************************************************************
*Comandos Básicos
*****************************************************************************
*update all
*cd		    cambiar el directorio
*set mem	modificar la memoria empleada
*use		abrir el documento
*insheet	importar datos de otros documentos
*edit 		puede importar los datos desde la ventana (copiar pegar)
*compress	si necesita más memoria
*clear  	limpiar la base
*log using  guarda los resultados en un archivo de texto
*help 		AYUDA
*lookup 	AYUDA
*serach 	AYUDA
*findit		buscar comandos creados por usuarios
*display 	displays strings and values of scalar expressions
*exit 		cierra el programa
*****************************************************************************
*Ejemplos
clear
cd  "C:\Users\a.diaze\Dropbox\" 
set logtype text, perm
log using resultados.txt
use hh_98.dta, clear

set mem 30m
use hh_98.dta, clear

set mem 10m

clear

set mem 10m
use hh_98.dta, clear
compress
save hh_98c.dta, replace /* si quiere remplazar su base de datos puede escribir simplemente save,replace */

help memory
help reg
search _all psmatch2
findit psmatch2
display "HOLA MUNDO"
display 2+2
display "2+2"

*Salir de Stata
*exit
*exit, clear
log close

*****************************************************************************
*****************************************************************************
*Análisis descriptivo
*****************************************************************************
*describe	describir la base de datos
*list		hacer una lista de los contenidos de la base
*codebook 	una descripción detallada de los contenidos
*count		contar
*summarize	estadisticas descriptivas
*tabstat	tabla de estadísticas descriptivas
*tabulate	hace tablas de frecuencia
*table		tabla de estadísticas descriptivas
*graph	    graficar
*twoway		graficar
*kdensity	grafica de la densidad
*sort 	    ordena los datos	
*histogram  histograma
*correlate	correlaciones
*****************************************************************************
clear
set mem 10m
use hh_98.dta, clear

describe 
des nh villid
describe nh-famsize
describe exp*

edit

list in 1/3
list famsize educhead if (sexhead == 0 & agehead<45)
codebook

count
count if agehead>50

sum famsize educhead
sum famsize educhead, d
*sum famsize educhead [fw =weight] *error porque los pesos no son enteros!
sum famsize educhead [aw =weight]
*sum famsize educhead [pw =weight] *pw no es válido para este comando
sum famsize educhead [iw =weight]

/*Mirar abajo para una descripción de los pesos fw aw pw iw */

sort dfmfd
by dfmfd: sum famsize educhead 

sort dfmfd
by dfmfd: sum famsize educhead [aw=weight]

tabstat famsize educhead, stats(mean sd) by(dfmfd)
tabstat famsize educhead [aw=weight], stats(mean sd) by(dfmfd)

tab dfmfd
tab sexhead if dfmfd == 1
tab educhead sexhead
tab dfmfd sexhead, col row
tab dfmfd sexhead, miss

table dfmfd, c(mean famsize mean educhead)
des educhead

format educhead %3.2f
table dfmfd, c(mean famsize mean educhead)
table dfmfd sexhead, c(mean famsize mean educhead)

histogram agehead
kdensity agehead
histogram agehead, kdens
twoway (scatter educhead agehead), ytitle(Education of head) xtitle(age of head) title(education by Age)

*****************************************************************************
*Modificar la base de datos						
*****************************************************************************
*label data	 	darle un nombre a la base de datos
*order	 		ordenar las variables de la base de datos
*label variable	darle un nombre a una variable
*label define	
*label values	aplicar los nombres a una variables 
*rename		    renombrar una variable
*recode		    recodificar los valores de una variable
*notes			poner notas a la base de datos
*generate		crear una variable nueva
*egen			tiene funciones especiales
*replace		reemplazar un valor 
*by				ejecutar el comando basado en valores de varlist
*if				condición al final del comando
*in 			condición al final del comando
*****************************************************************************
/****************************************************************************
Operadores 
*****************************************************************************
> 			mayor a
< 			menor a 
== 			igual a
>= 			mayor o igual
<= 			menor o igual
!= 			diferente a
&			Y
|			o
!			no
*****************************************************************************/
/****************************************************************************
Operadores Aritméticos
*****************************************************************************
+ 			Suma
- 			Resta
* 			Multiplicación
/ 			División
^			Exponencial
*****************************************************************************/
*****************************************************************************
/* Funciones matemáticas para usar con generate
*****************************************************************************
abs(x)		Valor absoluto
sqrt(x)		Raíz cuadrada
ln(x) 		Log natural
log10(x)	Log en base 10
exp(x)		Exponencial
round(x) 	Redondea al entero mas cercano
uniform()	Varible de una distribución uniforme
rnormal()	Variable de una distribución normal estándar
para ver todas las funciones help math functions
*****************************************************************************/

*Labeling
label data "Base de Datos Bangladesh 1998"
label variable oldhead "Jefe de Hogar mayor a 50: 1=Si 0=No"
des oldhead

tab sexhead

label define sexlabel 0 "Mujer" 1 "Hombre"
label values sexhead sexlabel

tab sexhead
tab sexhead, nolabel

gen oldhead = 1 if agehead >50
replace oldhead = 0 if agehead <=50

gen oldhead2 = [agehead>50] if agehead !=.

tab oldhead
tab oldhead2

egen avgage = mean(avgage)
egen avgagemf = mean(agehead), by(sexhead)


*****************************************************************************
*Manipular la base de datos
*****************************************************************************
*keep if		mantenga las observaciones si ...
*keep 			mantenga las variables
*drop			borre las variables
*merge using	incluya variables adicionales
*append using 	adjunte observaciones
*sort			ordene las variables
*order
*destring		convertir string var en numero
*collapse		genera sumas, promedios etc sobre algunas observaciones
*****************************************************************************
*eliminar/mantener observaciones
preserve
drop if agehead>=80
keep if famsize <=6
drop in 1/20
restore

*Combinar bases de datos
use hh_98, clear
drop dmmfd dfmfd
save hh_98_1.dta, replace

use hh_98, clear
keep nh dmmfd dfmfd
save hh_98_2.dta, replace

use hh_98_1, clear
merge 1:1 nh using hh_98_2

tab _merge

*****************************************************************************
/* Weights:
*****************************************************************************
fweight (fw) = 	pesos de frecuencia, indican cuántas observaciones en 
				la población están representadas por cada observación
				en la muestra, la variable de pesos debe tomar valores 
				enteros!
aweight (aw) = 	pesos analíticos, son apropiados al trabajar con datos 
				que contienen promedios (ejemplo: ingreso per capita 
				promedio del hogar). La variable de pesos es proporcional 
				al numero de personas sobre las cuales el promedio fue 
				calculado. 
				Tecnicamente, los pesos analíticos son inversamente 
				proporcionales a la varianza de una observación, esto 
				implica que si una observación tiene un peso mayor es porque 
				la observación está basada en mayor información y por lo 
				tanto es una mejor aproximación ya que tiene menor varianza.
pweight (pw)  = pesos muestrales, son la inversa de la probabilidad de 
				selección dadas por el diseño muestral
iweight (iw)  = pesos de importancia, indican la importancia relativa de 
				cada observación
*****************************************************************************/
*****************************************************************************
*Análisis de los datos
****************************************************************************
*ttest	    t-test
*regress	Regression
*predict	Predicts after model estimation
*kdensity	Kernel density estimates and graphs
*pnorm		Graphs a standardized normal plot
*qnorm		Graphs a quantile plot
*rvfplot	Graphs a residual versus fitted plot
*rvpplot	Graphs a residual versus individual predictor plot
*xi			Creates dummy variables during model estimation
*test		Test linear hypotheses after model estimation
*****************************************************************************
/****************************************************************************
Lista de Variables
*****************************************************************************
var1 			Solo una variable
var1 var2 var3 	Tres variables
var*			Tadas las variables que comienzan por var
*var			Todas las variables que terminan en var
mi*var			Todas las variables que comienzan en mi y terminan en var
esto-eso		Tadas las variables en el orden de la ventana de variables 
				entre esto y eso
*****************************************************************************/

*****************************************************************************
/* Funciones matemáticas para usar con egen
*****************************************************************************
egen <new variable>= <function>(<expression(s)> or <variable(s)>) [, by (<variables>)]
mean()
rowmean()
sum()
rowtotal()
max()
rowmax()
min()
rowmin()
group()
concat()

*****************************************************************************/

*****************************************************************************
*****************************************************************************
*                       MACROS EN STATA
*****************************************************************************
*****************************************************************************
/*
Las macros en Stata son herramientas para almacenar texto que luego puede 
reutilizarse en comandos posteriores. No son variables, no almacenan datos 
numéricos como tal, sino texto que puede ser evaluado o invocado más adelante. 
Se usan con frecuencia para simplificar código, automatizar tareas repetitivas, 
o construir loops.

Stata tiene dos tipos principales de macros:
- local: válidas solo dentro del entorno donde se definieron (programa o loop)
- global: válidas en todo el entorno de trabajo mientras dure la sesión 
         (⚠️ desaconsejadas para la mayoría de tareas por riesgo de sobreescritura)
*/

*****************************************************************************
* 1. MACRO LOCAL: definición y expansión
*****************************************************************************
/* 
Usa `local` por defecto. Las macros locales solo existen dentro del 
do-file o programa donde se definen.
*/

local uno 1
display `uno'

* Esto imprimirá 1 en la consola. La macro `uno` se expande y sustituye 
* por su contenido (1) antes de ejecutar el comando.

*****************************************************************************
* 2. EVALUAR EXPRESIONES dentro de macros
*****************************************************************************
/*
Si queremos que Stata EVALUE una expresión, usamos el signo igual = 
luego del nombre de la macro.
*/

local suma = 2 + 2
display `suma'

* Stata calcula 2 + 2 y guarda el resultado 4 como texto dentro de `suma`.
* Al hacer display, se imprime el número 4.

*****************************************************************************
* 3. MACRO CON TEXTO
*****************************************************************************
/*
Cuando una macro contiene texto, siempre encierra su invocación entre 
comillas para evitar errores de interpretación.
*/

local saludo "¡Hola, mundo!"
display "`saludo'"

* Esto mostrará: ¡Hola, mundo!

*****************************************************************************
* 4. MACROS LOCALES CON LISTAS DE VARIABLES
*****************************************************************************
/*
Puedes usar macros locales para almacenar listas de variables, lo cual 
es muy útil para regresiones y comandos repetitivos.
*/

local control1 per001 per011
local control2 `control1' per019

* Nota: para combinar macros locales, usa la sintaxis `nombre'

sum `control1'
sum `control2'

*****************************************************************************
* 5. MACRO GLOBAL: uso y precaución
*****************************************************************************
/*
Al usar global, la invocación se hace con signo dólar ($). El contenido 
se mantiene accesible en toda la sesión.

⚠️ ADVERTENCIA: Se recomienda evitar nombres obvios en global ya que 
puede generar conflictos si se reutilizan nombres con comandos establecidos.
Usa `local` siempre que sea posible.
*/

global pi 3.1416
display $pi

* Ejemplo de ruta con global (uno de los pocos usos recomendados)
global dataruta "/Users/usuario/Documentos/datos/"

* Para usar la ruta:
* use "${dataruta}archivo.dta", clear

*****************************************************************************
* 6. SCALAR vs MACRO
*****************************************************************************
/*
Diferencia clave:
- macro: almacena texto (puede ser número, pero como cadena de caracteres)
- scalar: almacena un número real que puede usarse en operaciones matemáticas

Un scalar almacena valores numéricos (reales), no texto. Se usa para 
cálculos matemáticos, estadísticas o comparaciones numéricas.
*/

scalar x = 2 + 3
display x
* Esto imprime 5

scalar area = 3.1416 * (2^2)
display area
* Resultado: 12.5664

* Ejemplo práctico: guardar estadísticas de un summarize
sysuse auto, clear
sum price
scalar media_precio = r(mean)
display "El precio promedio es: " media_precio

*****************************************************************************
* 7. BUENAS PRÁCTICAS CON MACROS
*****************************************************************************
/*
✅ Usa `local` por defecto.
✅ Usa nombres descriptivos para evitar confusión.
✅ Cierra comillas cuando el contenido tiene espacios o texto.
✅ Borra scalars con `scalar drop nombre` si ya no los necesitas.
✅ Para texto con comillas dentro, usa comillas compuestas: `" "'
*/

* Ejemplo limpiando scalar
scalar drop area

*****************************************************************************
* 8. EJERCICIO: CREAR MACROS PERSONALIZADAS
*****************************************************************************
/*
Ejercicio: Crea una macro con tu nombre y otra con tu año de nacimiento. 
Luego muestra una frase concatenando ambas.
*/

local nombre "Ana"
local nacimiento 1980
display "Hola, mi nombre es `nombre' y nací en `nacimiento'"

*****************************************************************************
*****************************************************************************
*                       LOOPS EN STATA
*****************************************************************************
*****************************************************************************
/*
Los loops en Stata permiten automatizar tareas repetitivas, iterando sobre 
listas de elementos o rangos numéricos. Son muy útiles cuando necesitas 
aplicar un mismo comando a varias variables, realizar simulaciones o crear 
múltiples gráficos/tablas de forma eficiente.
*/

*****************************************************************************
* 1. LOOP CON foreach
*****************************************************************************
/*
foreach itera sobre una lista de elementos, que pueden ser:
- nombres de variables
- palabras clave
- números, si se combinan con `of numlist`
*/

* a) Iterar sobre nombres arbitrarios
foreach color in rojo azul verde {
    display "El color es `color'"
}

* b) Iterar sobre variables específicas con regresiones
sysuse auto, clear
foreach var in mpg price displacement {
    regress `var' weight
}

* c) Iterar sobre variables en la base usando `of varlist`
foreach v of varlist price weight length {
    summarize `v'
}

* d) Iterar con condición y mostrar resultado
foreach s of varlist price weight {
    quietly summarize `s' if foreign == 1
    display "Promedio de `s' para foreign = 1: " r(mean)
}

*****************************************************************************
* 2. LOOP CON forvalues
*****************************************************************************
/*
forvalues itera sobre una secuencia numérica definida por un rango o paso.
*/

* a) Secuencia simple del 1 al 5
forvalues i = 1/5 {
    display "Iteración `i'"
}

* b) Incrementos diferentes (de 10 a 20, de 2 en 2)
forvalues j = 10(2)20 {
    display "`j'"
}

* c) Anidar loops con forvalues
forvalues i = 1/3 {
    forvalues j = 1/3 {
        display "Fila: `i', Columna: `j'"
    }
}

*****************************************************************************
* 3. LOOP CON while
*****************************************************************************
/*
while permite ejecutar código mientras una condición sea verdadera. 
Es útil para estructuras de control más manuales o condicionales complejas.
*/

local i = 1
while `i' <= 5 {
    display "`i'"
    local ++i
}

*****************************************************************************
*****************************************************************************
*               PROGRAMAS DEFINIDOS POR EL USUARIO
*****************************************************************************
*****************************************************************************
/*
En Stata puedes definir tus propios programas usando el comando 
`program define`. Esto es especialmente útil para empaquetar comandos 
que usas con frecuencia o para crear rutinas más limpias.

Notas importantes:
- Siempre usar `capture program drop nombre` antes de definir uno nuevo.
- Usa `syntax` cuando quieras controlar argumentos y prevenir errores.
- Dentro del programa, puedes acceder a estadísticas con r(), e(), etc.
- Usa `quietly` para ejecutar comandos sin mostrar su salida completa.
*/

*****************************************************************************
* 1. ESTRUCTURA BÁSICA DE UN PROGRAMA
*****************************************************************************

capture program drop saludo
program define saludo
    display "Hola, FELIZ día"
end

saludo

*****************************************************************************
* 2. PROGRAMAS CON ARGUMENTOS usando args
*****************************************************************************

capture program drop cuadrado
program define cuadrado
    args x
    display "El cuadrado de `x' es: " = `x'^2
end

cuadrado 4
* Esto imprimirá: El cuadrado de 4 es: 16

*****************************************************************************
* 3. PROGRAMAS CON syntax (más robusto)
*****************************************************************************
/*
`syntax` verifica que se cumpla una estructura específica de argumentos.
*/

capture program drop promedio
program define promedio
    syntax varlist(min=1 max=1)
    summarize `varlist'
end

sysuse auto, clear
promedio mpg

*****************************************************************************
* 4. PROGRAMA PERSONALIZADO: mysum
*****************************************************************************

capture program drop mysum
program define mysum
    syntax varlist(min=1 max=1)
    quietly summarize `varlist'
    display "Variable: `varlist'"
    display "Promedio: " %6.2f r(mean)
    display "Desviación estándar: " %6.2f r(sd)
end

mysum weight

*****************************************************************************
*****************************************************************************
*         ALMACENAMIENTO DE RESULTADOS CON postfile Y LOOPS
*****************************************************************************
*****************************************************************************
/*
Cuando queremos guardar resultados generados dentro de un loop para 
analizarlos después (por ejemplo, coeficientes, medias, errores estándar), 
Stata nos ofrece una herramienta poderosa: postfile.

¿Qué hace postfile?
Permite crear una tabla temporal (como una mini base de datos) en la que 
puedes ir guardando los resultados de cada iteración del loop. Al final, 
puedes abrirla como si fuera cualquier base.

Pasos:
1. tempname crea un alias para el objeto de postfile.
2. tempfile genera una ruta temporal para almacenar los resultados.
3. postfile define las variables a guardar.
4. Dentro del loop, usamos post para guardar cada fila.
5. Cerramos con postclose.
6. Cargamos el archivo resultante con use y lo exploramos.

Buenas prácticas:
- Usa tempfile para evitar escribir archivos por accidente.
- Usa tipos de datos adecuados: str para nombres, numeric para estadísticas.
- Siempre cierra el objeto con postclose antes de usar el archivo.
*/

sysuse auto, clear

tempname resultados
tempfile archivo

postfile `resultados' str15 variable media sd using `archivo'

foreach var in price weight length {
    quietly summarize `var'
    post `resultados' ("`var'") (r(mean)) (r(sd))
}

postclose `resultados'

use `archivo', clear
list

*****************************************************************************
*                         FIN DEL ARCHIVO
*****************************************************************************
