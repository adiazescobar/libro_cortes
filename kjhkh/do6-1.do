* TWFE y Event Studies

webuse set www.damianclarke.net/stata/
webuse bacon_example.dta, clear

xtset stfips year 
tab year 
tab year post 

ren asmrs Y 
ren post D

* Estimar el efectos fijos.

xtreg Y D i.year, fe cluster(stfips)
reghdfe Y D, abs(year stfips) cluster(stfips)

* El estimador cambia en el tiempo
xtreg Y D##i.year, fe cluster(stfips)

* El estimador cambia por cohorte 
egen D_sum = sum(D), by(stfips)
tab D_sum
xtreg Y D##i.D_sum  i.year, fe cluster(stfips)

*Prueba
bacondecomp Y D,  ddetail


*********************************
*Solución 1: EVENTOS
*********************************

*Generar la fecha al evento

gen timeToTreat = year - _nfd

*Event studies sin controles
eventdd Y i.year, timevar(timeToTreat) method(fe, cluster(stfips)) graph_op(ytitle("Suicidios por 1m Mujeres") xlabel(-20(5)25))

*Event studies con controles
eventdd Y pcinc asmrh cases i.year, timevar(timeToTreat) method(fe, cluster(stfips)) graph_op(ytitle("Suicidios por 1m Mujeres") xlabel(-20(5)25))

**************************************************
* La creatividad de los colegas -->
*********************************
// supporting packages
ssc install schemepack, replace
ssc install avar, replace 
ssc install reghdfe, replace
ssc install event_plot, replace
ssc install palettes, replace
ssc install colrspace, replace

// DiD packages
ssc install drdid, replace
ssc install csdid, replace
ssc install did_imputation, replace
ssc install eventstudyinteract, replace
ssc install did_multiplegt, replace
ssc install stackedev, replace
ssc install did2s, replace



*************
*** csdid ***
*************

csdid Y, ivar(stfips) time(year) gvar(_nfd) notyet

estat event, window(-10 10) estore(csdd) 



***********************
*** did_imputation  ***
***********************

did_imputation Y stfips year _nfd, horizons(0/10) pretrend(10) minn(0) 

estimates store didimp	
