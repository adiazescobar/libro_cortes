*Propiedades del Estimador de Variables Instrumentales en muestras finitas



clear
set obs 1000
gen z2 = rnormal()
gen z1 = rnormal()
gen w = rnormal()
gen D = z1*.3 +z2*.7 +  rnormal() + w
gen u = rnormal()
gen y = D*0.5 + w + u
reg y D
ivreg2 y (D=z1 z2), first
reg D z

*Montecarlo 
cap program drop monteiv
program monteiv, rclass
  clear
  set obs `1' 
  * El primer argumento de¡l comando monteiv es el numero de observaciones 
  gen z = rnormal()
  gen w = rnormal()
  gen D = z*.3 + rnormal() + w
  gen u = rnormal()
  gen y = D*0.5 + w + u*5
  reg y D
  return scalar b_mco = _b[D]
  return scalar se_mco = _se[D]
  ivreg y (D=z)
  return scalar b_iv = _b[D]
  return scalar se_iv = _se[D]
end 

* Solo 30 observaciones
simulate b_mco=r(b_mco)     se_mco=r(se_mco)  /// 
         b_iv=r(b_iv) se_iv=r(se_iv)    /// 
   , rep(1000): monteiv 1000
sum

*El sesgo del IV es ENORME. En este caso es mejor usar MCO

*Aumentar la muestra a 100
simulate b_mco=r(b_mco)     se_mco=r(se_mco)  /// 
         b_iv=r(b_iv) se_iv=r(se_iv)    /// 
   , rep(1000): monteiv 100
sum

*Aumentar la muestra a 300
simulate b_mco=r(b_mco)     se_mco=r(se_mco)  /// 
         b_iv=r(b_iv) se_iv=r(se_iv)    /// 
   , rep(1000): monteiv 300
sum

*Aumentar la muestra a 750

simulate b_mco=r(b_mco)     se_mco=r(se_mco)  /// 
         b_iv=r(b_iv) se_iv=r(se_iv)    /// 
   , rep(1000): monteiv 750
sum



*Aumentar la muestra a 10000
simulate b_mco=r(b_mco)     se_mco=r(se_mco)  /// 
         b_iv=r(b_iv) se_iv=r(se_iv)    /// 
   , rep(1000): monteiv 10000
sum
