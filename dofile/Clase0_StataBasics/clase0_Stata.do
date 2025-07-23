*Introduccion a Stata

/*Tipos de archivos
En Stata existen cuatro tipos de archivos:
.dta contiene la base de datos
.do  contiene los programas desarrollados por los usuarios
.log contiene la informaciÛn de lo que se realiza en Stata
.ado contiene los programas desarrollados por especialistas (por ahora nos vamos a olvidar de ellos)

Para mantener los archivos organizados es recomendable crear una estructura para guardar los archivos
c:\eval\
c:\eval\data
c:\eval\do
c:\eval\log
*/

/*Descripción de los datos que vamos a emplear

Vamos a emplear una base de datos de la encuesta de hogares de Bangladesh realizada por el Banco Mundial 
y un centro de InvestigaciÛn local 

hh_91.dta 	este archivo tiene informaciÛn que contiene 826 datos de hogares en 1991. Tiene 24 variables sobre informaciÛn del hogar
			(educaciÛn de la cabeza de familia, propiedad del lugar de vivienda, gastos, entre otros) e inforamciÛn sobre el lugar de
			residencia (infraestructura, precios de los bienes de consumo principales, entre otros) 
hh_98.dta	este es la versiÛn panel de hh_91.dta. incluye 303 nuevos hogares, haciendo una muestra de 1129. Esta base incluye las
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
*codebook 	una descripciÛn detallada de los contenidos
*count		contar
*summarize	estadisticas descriptivas
*tabstat	tabla de estadÌsticas descriptivas
*tabulate	hace tablas de frecuencia
*table		tabla de estadÌsticas descriptivas
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
*sum famsize educhead [pw =weight] *pw no es v·lido para este comando
sum famsize educhead [iw =weight]

/*Mirar abajo para un descripciÛn de los pesos fw aw pw iw */

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
*notes			poenr notas a la base de datos
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
fweight (fw) = 	pesos de frecuencia, indican cu·ntas observaciones en 
				la población están representadas por cada observación
				en la muestra, la variable de pesos debe tomar valores 
				enteros!
aweight (aw) = 	pesos análiticos, son apropiados al trabajar con datos 
				que contienen promedios (ejemplo: ingreso per capita 
				promedio del hogar). La variable de pesos es proporcional 
				al numero de personas sobre las cuales el promedio fue 
				calculado. 
				Tecnicamente, los pesos análiticos son inversamente 
				proporcionales a la varianza de una observación, esto 
				implica que si una observación tiene un peso mayor es porque 
				la observaciÛn est· basada en mayor información y por lo 
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

*Macros
*1. SCALAR
scalar x1 = 3
di x1

use "/Users/adiazescobar/Downloads/Bases_Sisben_IV_Mar_2021/SISBEN_PERS_SIV_2021.dta", clear
recode per001 (2=0)
sum per001
scalar perhom = r(mean)*100
di "El porcentaje de hombres de la muestra es: " %9.2f perhom "%"

*2. GLOBALS

global dataruta "/Users/adiazescobar/Downloads/Bases_Sisben_IV_Mar_2021/"

*use $dataruta/SISBEN_PERS_SIV_2021.dta, clear

cd $dataruta

use SISBEN_PERS_SIV_2021.dta, clear
recode per001 (2=0) 
recode per011 (2=0) (9=.)
recode per019 (1=.) (2=0) (3=1) (4/9 =.)
global control1 per001 per011
global control2 $control1 per019

sum $control1
sum $control2

*3. LOCAL

local control1 per001 per011
local control2 `control1' per019
 

sum `control1'
sum `control2'

foreach color in rojo negro blanco {
display "`color'"
 }

foreach var in per001 per011 per019{
qui	{
sum `var'
scalar media_`var' = r(mean)
local `var'Lab: variable label `var'
}
display "el promedio de ``var'Lab': " media_`var'

}


foreach j of numlist 1/15 {
qui {
	sum i`j'
scalar media_i`j' = r(mean)
local `j'Lab: variable label i`j'
}
display "el promedio de ``j'Lab': " media_i`j'

}
