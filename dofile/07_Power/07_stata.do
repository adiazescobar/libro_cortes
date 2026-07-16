********************************************************************************
********************************************************************************
********************************************************************************

** Calculos de Poder Estadístico
*Created by: Sabhya Gupta with input from Jack Cavanagh, Maya Duru, Mike Gibson, Sarah Kopper
*To report errors/make suggestions or ask questions contact: Sabhya Gupta - sagupta@povertyactionlab.org
*Last edited: 06/24/2021
* Modified by: adiazescobar
********************************************************************************


* Pipeline canónico reproducible (no requiere datos de clase)
clear all
set more off
local alpha 0.05
local target_power 0.80
local allocation 0.50
capture mkdir "dofile/07_Power/results"
tempname results
postfile `results' str40 escenario str12 familia str28 comando str20 cantidad_solicitada double valor_stata double N_total double N_por_brazo double N1 double N2 str24 unidad_resultado double alpha double power str12 colas double asignacion_tratada double delta double sd double p0 double p1 double takeup_tratamiento double takeup_control double retencion double icc double tamano_cluster double K1 double K2 double K_total double N_realizable str24 fuente using "dofile/07_Power/results/power_resultados.dta", replace

power twomeans 0 0.30, sd(1) power(`target_power') alpha(`alpha')
local stata_N = r(N)
local stata_N1 = r(N1)
local stata_N2 = r(N2)
post `results' ("continuo sin controles") ("continua") ("power twomeans") ("N_total") (`stata_N') (`stata_N') (`stata_N1') (`stata_N1') (`stata_N2') ("individuos") (`alpha') (`target_power') ("bilateral") (`allocation') (0.30) (1) (.) (.) (.) (.) (.) (.) (.) (.) (.) (.) (.) ("07_stata.do")

power twomeans 0 0.30, sd(0.70) power(`target_power') alpha(`alpha')
local stata_N = r(N)
local stata_N1 = r(N1)
local stata_N2 = r(N2)
post `results' ("continuo con controles") ("continua") ("power twomeans") ("N_total") (`stata_N') (`stata_N') (`stata_N1') (`stata_N1') (`stata_N2') ("individuos") (`alpha') (`target_power') ("bilateral") (`allocation') (0.30) (0.70) (.) (.) (.) (.) (.) (.) (.) (.) (.) (.) (.) ("07_stata.do")

power twoproportions 0.08 0.05, power(`target_power') alpha(`alpha')
local stata_N = r(N)
local stata_N1 = r(N1)
local stata_N2 = r(N2)
post `results' ("binario") ("binaria") ("power twoproportions") ("N_total") (`stata_N') (`stata_N') (`stata_N1') (`stata_N1') (`stata_N2') ("individuos") (`alpha') (`target_power') ("bilateral") (`allocation') (.) (.) (0.08) (0.05) (.) (.) (.) (.) (.) (.) (.) (.) (.) ("07_stata.do")

local takeup_delta = 0.30*(0.90-0.10)
power twomeans 0 `takeup_delta', sd(1) power(`target_power') alpha(`alpha')
local stata_N = r(N)
local stata_N1 = r(N1)
local stata_N2 = r(N2)
post `results' ("take-up") ("continua") ("power twomeans") ("N_total") (`stata_N') (`stata_N') (`stata_N1') (`stata_N1') (`stata_N2') ("individuos") (`alpha') (`target_power') ("bilateral") (`allocation') (`takeup_delta') (1) (.) (.) (0.90) (0.10) (.) (.) (.) (.) (.) (.) (.) ("07_stata.do")

power twomeans 0 0.30, sd(1) power(`target_power') alpha(`alpha')
local stata_N = r(N)
local stata_N1 = r(N1)
local stata_N2 = r(N2)
local attrition_N = ceil(`stata_N'/0.80)
post `results' ("atrición") ("continua") ("power twomeans") ("N_total") (`stata_N') (`stata_N') (`stata_N1') (`stata_N1') (`stata_N2') ("individuos") (`alpha') (`target_power') ("bilateral") (`allocation') (0.30) (1) (.) (.) (.) (.) (0.80) (.) (.) (.) (.) (.) (`attrition_N') ("07_stata.do")

power twoproportions 0.07203 0.06, power(`target_power') alpha(`alpha')
local stata_N = r(N)
local stata_N1 = r(N1)
local stata_N2 = r(N2)
post `results' ("tasa") ("tasa") ("power twoproportions") ("N_total") (`stata_N') (`stata_N') (`stata_N1') (`stata_N1') (`stata_N2') ("individuos") (`alpha') (`target_power') ("bilateral") (`allocation') (.) (.) (0.07203) (0.06) (.) (.) (.) (.) (.) (.) (.) (.) (.) ("07_stata.do")

power twomeans 0 0.30, cluster m1(50) mratio(1) kratio(1) power(`target_power') sd(1) rho(0.05) alpha(`alpha')
local stata_N = r(N)
local stata_N1 = r(N1)
local stata_N2 = r(N2)
local stata_K1 = r(K1)
local stata_K2 = r(K2)
local stata_M1 = r(M1)
local stata_Ktotal = `stata_K1' + `stata_K2'
post `results' ("clúster") ("clúster") ("power twomeans, cluster") ("K_por_brazo") (`stata_K1') (`stata_N') (`stata_N1') (`stata_N1') (`stata_N2') ("clusters_por_brazo") (`alpha') (`target_power') ("bilateral") (`allocation') (0.30) (1) (.) (.) (.) (.) (.) (0.05) (`stata_M1') (`stata_K1') (`stata_K2') (`stata_Ktotal') (`stata_N') ("07_stata.do")

postclose `results'
use "dofile/07_Power/results/power_resultados.dta", clear
export delimited using "dofile/07_Power/results/power_resultados.csv", replace
erase "dofile/07_Power/results/power_resultados.dta"
exit, clear

* Material histórico de clase (requiere que el usuario cargue su base y globals).
global outcome ha_nchs                                           //SPECIFY the outcome and treatment 


****************************************************************************************
*********************************** 1. No covariates ***********************************
****************************************************************************************

* The following code assumes the unit of randomization is the same as the unit of observation 
****************************************************************************************
************************* 1a. Sample size for a given effect size **********************
****************************************************************************************
local power = 0.8
//SPECIFY - desired power
local nratio = 1                                                //SPECIFY - the ratio of experimental group to control group (1=equal allocation)
local alpha = 0.05                                                              //SPECIFY - the significance level
global outcome call
sum $outcome  if !missing($outcome)                         
local sd = `r(sd)'
local baseline = `r(mean)'

local effect = `sd'*0.3                                           //SPECIFY - EFECTO MINIMO DETECTABLE Aquí especificamos 0.3 desviaciones estánadar, pero aquí ustede lo debe modificar de acuerdo a su análisis 
local treat = `baseline' + `effect'

power twomeans `baseline' `treat', power(`power') sd(`sd') nratio(`nratio') table

power twomeans `baseline', power(`power') sd(`sd') nratio(`nratio') n(4870) table            //SPECIFY diff range to indicate the different possible effect sizes













local effect = round(`effect',0.0001)

local samplesize = r(N)

di as error "La muestra requerida es `samplesize' para detectar un efecto de  `effect' con una probabilidad de `power' si el effecto es verdadero y la razon de individuos en el grupo de tratamiento versus el grupo de control es  `nratio'"


* ¿Cómo cambia el efecto a medida que las desviaciones estándar cambian ?


power twomeans `baseline', power(`power') sd(`sd') nratio(`nratio') diff(0.1(0.15)2) table         //SPECIFY diff range to indicate the different possible effect sizes


power twomeans `baseline' `treat', power(`power') sd(0.5(0.1)2) nratio(`nratio') table                //SPECIFY sd range




****************************************************************************************
**************************** 1b. Efecto mínimo detectable para un N dado ****************************
****************************************************************************************

local power = 0.8
local nratio = 1
local alpha = 0.05
local N = _N                                                                    //SPECIFY - the total sample size. Aquí tomamos el de la base2, pero se debe modificar según sus necesidades


quietly sum $outcome if !missing($outcome)                                      
local sd = `r(sd)'
local baseline = `r(mean)'

power twomeans `baseline', n(`N') power(`power') sd(`sd') nratio(`nratio') table

local mde= round(`r(delta)',0.0001)

di as error "El Efecto mínimo detectables es `mde' dado un tamaño de muestra  `N', la razon de unidades tratadas vs unidades en el grupo de control es  `nratio', y el poder es `power'"


* Cambios en el MDS ante cambios en N y cambios en la razon de tratados a controles

power twomeans `baseline', power(`power') sd(`sd') n(10000(2000)20000) nratio(`nratio') table           //SPECIFY N 

power twomeans `baseline', n(`N') power(`power') sd(`sd') nratio(1(-0.2)0.1) table                   //SPECIFY range of ratios of treatment to sample size
                                                                                                        //NRatio = 1 implica que el tamaño de tratados y controles es el mismo. Una disminución en la razón implica qie una proporción mayor de la muestra se le asigna al grupo de control

                                                                                                       
****************************************************************************************
**************************** 1c. Efecto mínimo detectable para un N dado y una variable
**************************** dependiente binaria ****************************
****************************************************************************************


global outcome2 desn_cr     
local power = 0.8
local nratio = 1
local alpha = 0.05
local N = _N


quietly sum $outcome2 if !missing($outcome2)                                      
local sd = `r(sd)'
local baseline = `r(mean)'


power twoproportions 0.08 (0.01(0.005)0.1), power(0.8 0.9) graph

local mde= round(`r(delta)',0.0001)



**** PAUSA ******


                                                                                           

    
****************************************************************************************
********************************* 2. Adding covariates *********************************
****************************************************************************************

/* To see how potential controls affect power,  we would ideally have access to a sample data set 
 (e.g. historical or pilot data). With these data, we would want to:
     1. Regress Y_i (the outcome) on X_i (the controls) 
     2. Use the residual standard deviation of the outcome variable from this regression to evaluate 
     how much variance is explained by the set of covariates we plan to include
         - In practice, this residual SD becomes the new SD we include in our parametric power calculations
 With access to historical data, for example, this would involve regressing last year's test scores 
 on test scores from the year before. Using balsakhi data, this would be as follows. 
 Note that this section is not applicable for power calculations with a binary outcome variable. 
 See McConnell and Vera-Hernandez 2015 (https://www.ifs.org.uk/uploads/publications/wps/WP201517_update_Sep15.pdf)
 for a discussion of covariates for binary outcomes and accompanying sample code */


****************************************************************************************
************** 2a. Sample size for a given effect size - with covariates  **************
****************************************************************************************

local power = 0.8                                                               //SPECIFY - desired power
local nratio = 1                                                                //SPECIFY - the ratio of experimental group to control group
local alpha =0.05                                                               //SPECIFY - the significance level
    
local covariates $X                                       //SPECIFY the covariates - use baseline values of covariates
local number_covariates: word count `covariates'                                                 

regress $outcome `covariates'                                                     //SPECIFY outcome and control variables

local res_sd =round(sqrt(`e(rss)'/`e(df_r)'),0.0001)                          //this is the new standard deviation for the power calculation or the residual sd not explained by the control(s). 
                                                                                //This will be used for power calculation.
    
quietly sum $outcome if  !missing($outcome)                                      //sum the outcome at baseline and record the mean and the standard deviation
local baseline = `r(mean)'
local sd = `r(sd)'
local effect_cov = `sd'*0.3                                                     //SPECIFY - the expected effect. Here we specify 0.3 standard deviations, but this should be updated based on what is reasonable for the study
    
local treat = `baseline' + `effect_cov'

power twomeans `baseline' `treat', power(`power') sd(`res_sd') nratio(`nratio') alpha(`alpha') table
    
local effect_cov = round(`effect_cov',0.0001)
local samplesize_cov = `r(N)'
    
 di as error "The minimum sample size needed is `samplesize_cov' to detect an effect of `effect_cov' with a probability of `power' if the effect is true, the ratio of units in treatment and control is `nratio', and the residual standard deviation is `res_sd' after accounting for covariates: `covariates'"

    

****************************************************************************************
****************** 2b. MDE for a given sample size - with covariates  ******************
****************************************************************************************

local power = 0.8                                                               //SPECIFY - desired power
local nratio = 1                                                                //SPECIFY - the ratio of experimental group to control group
local alpha =0.05                                                               //SPECIFY - the significance level
local N_cov= _N                                                                 //SPECIFY - the total sample size. 
                                                                                //This is taken from the Balsakhi dataset but can be changed based on the study

local covariates  $X                                           //SPECIFY the covariates - use baseline values of covariates
regress $outcome `covariates'                                                     //SPECIFY outcome and control variables

local res_sd = round(sqrt(`e(rss)'/`e(df_r)'),0.0001)                          //this is the new standard deviation for the power calculation or the residual sd not explained by the control(s). 
                                                                                //This will be used for power calculation.
    
quietly sum $outcome if  !missing($outcome)                                         //sum the outcome at baseline and record the mean and the standard deviation
local baseline = `r(mean)'
    
power twomeans `baseline', n(`N_cov') power(`power') sd(`res_sd') nratio(`nratio') alpha(`alpha')  table 
    
local mde_cov= round(`r(delta)',0.0001)

 di as error "The MDE is `mde_cov' given a sample size of `N_cov', ratio of treatment and control group of `nratio', power `power', and the residual standard deviation of `res_sd' after accounting for covariates: `covariates'"
    

****************************************************************************************
********************* 3. Sample size with partial take-up   ****************************
****************************************************************************************

/* When there is inperfect compliance in the treatment or the control group, the expected effect is 
reduced by a factor of the effective take-up, where effective take-up = take-up in treatment - take-up in control */
    
local power = 0.8                                                               //SPECIFY - desired power
local nratio = 1                                                                //SPECIFY - the ratio of experimental group to control group
local alpha = 0.05                                                               //SPECIFY - the significance level
    
local takeup_treat = 0.9                                                        //SPECIFY - take-up in the treatment
local takeup_control =  0.1                                                     //SPECIFY - take-up in the control
    
quietly sum $outcome if !missing($outcome)                                      //sum the outcome at baseline and record the mean and the standard deviation
local sd_tu = `r(sd)'
local baseline = `r(mean)'

local effect_tu= `sd'*0.3                                                       //SPECIFY - the expected effect with perfect take-up. Here we specify 0.3 standard deviations, but this should be updated based on what is reasonable for the study

local tu = `takeup_treat' - `takeup_control'                                    //effective take-up
local effect_tu = `effect_tu'*`tu'                                              //effect size after adjusting for take-up
local treat_tu = `baseline' + `effect_tu'                                      //treatment mean after adjusting for take-up

power twomeans `baseline' `treat_tu', power(`power') sd(`sd_tu') nratio(`nratio') table
local samplesize_tu = `r(N)'
local effect_tu = round(`effect_tu',0.01)
    
 di as error "A minimum sample size of `samplesize_tu' is needed to detect an effect of `effect_tu' with a probability of `power' if the effect is true and the ratio of units in treatment and control is `nratio'"



****************************************************************************************
* 4. Overview of how MDE and sample size change as we add covariates and take-up changes
**************************************************************************************** 

*Note: This module calls on locals in modules 1-3, so you'll have to run them too

//how sample_size changes when we change the design
matrix input sample_size = (1,0,`effect',`samplesize' `sd'\ 1,`number_covariates',`effect_cov',`samplesize_cov' `res_sd' \ `tu',0,`effect_tu',`samplesize_tu' `sd_tu')
matrix colnames sample_size = take_up_rate number_covariates effect_given_take_up sample_size standard_dev
matrix list sample_size

//how MDE changes when we add more covariates
matrix input mde = (0,`mde',`N' `sd'\ `number_covariates',`mde_cov',`N_cov' `res_sd')
matrix colnames mde = number_covariates MDE N standard_dev
matrix list mde

****************************************************************************************
********************************* 5. Clustered designs *********************************
****************************************************************************************

/* The code presented so far has been for when the unit of randomization is the same
as the unit of observation. The following code is for clustered designs, when there are
multiple units of observation contained in a single unit of randomization (e.g., randomization
is at the school level but outcomes measured at the student level) */

global cluster_var  educa_jefe                                                //SPECIFY - the cluster variable


****************************************************************************************
****** 5a.Compute number of clusters for a given effect size and size of clusters ******
****************************************************************************************

local power = 0.8                                                               //SPECIFY - desired power
local nratio = 1                                                                //SPECIFY - the ratio of experimental group to control group
local alpha = 0.05                                                               //SPECIFY - the significance level
    
quietly sum $outcome if  !missing($outcome)                                      //sum the outcome at baseline and record the mean and the standard deviation
local sd = `r(sd)'
local baseline = `r(mean)'

local cluster_size_control = 50                                                 //SPECIFY - number of people in each cluster. 
                                                                                //This should be specified by the researcher
local mratio=1                                                                  //SPECIFY - the ratio of the cluster size in the treatment and the control
local cluster_size_treatment = `cluster_size_control'*`mratio'

local kratio = 1                                                                //SPECIFY - The ratio of the number of treatment clusters to the number of control clusters

local effect_cluster = `sd'*0.3                                                 //SPECIFY - the expected effect. Here we specify 0.3 standard deviations, but this should be updated based on what is reasonable for the study
                                                                                //Here the expected change in the treatment group is half of the standard deviation
local treat= `baseline' + `effect_cluster'                                         //define treatment mean

loneway $outcome $cluster_var                                                    //The loneway command calculates the one-way ANOVA by a group variable. 
                                                                                //It gives the within-group variation and the between group variation of a variable. 
                                                                                //It also produces the intra-cluster correlation coefficient (ICC)
    
local rho = `r(rho)'

power twomeans `baseline' `treat', cluster m1(`cluster_size_control') mratio(`mratio') kratio(`kratio') power(`power') sd(`sd') rho(`rho') alpha(`alpha') table
    
local effect_cluster = round(`effect_cluster',0.0001)

local n_clus_t = `r(K2)'
local n_clus_c = `r(K1)'
    
 di as error "A minimum of `n_clus_c' control clusters and `n_clus_t' treatment clusters is needed to detect an effect of `effect_cluster' with a probability of `power' if the effect is true, given the size of each control cluster as `cluster_size_control' units and ratio of the treatment to control cluster size of `mratio'"
    
    
****************************************************************************************
********* 5b. Compute cluster size given the number of clusters and effect size ********
****************************************************************************************

local power = 0.8                                                               //SPECIFY - desired power
local nratio = 1                                                                //SPECIFY - the ratio of experimental group to control group
local alpha = 0.05                                                               //SPECIFY - the significance level
    
quietly sum $outcome if  !missing($outcome)                                      //sum the outcome at baseline and record the mean and the standard deviation
local sd = `r(sd)'
local baseline = `r(mean)'

bysort $cluster_var: gen control_cluster = _n==1                                  
count if control_cluster & D ==0                                        //count the number of control clusters

local num_clusters_control = `r(N)'                                                //SPECIFY number of clusters in the control group - 
                                                                                //Taken from Balsakhi but can be specified by researcher 
    
local kratio = 1                                                                //SPECIFY - The ratio of the number of treatment clusters to the number of control clusters
local num_clusters_treatment = `num_clusters_control'*`kratio'    

local effect_cluster = `sd'*0.3                                                 //SPECIFY - the expected effect. Here we specify 0.3 standard deviations, but this should be updated based on what is reasonable for the study
                                                                                //Here the expected change in the treatment group is half of the standard deviation
                                                                                
local treat = `baseline' + `effect_cluster'                                     //define treatment mean

loneway $outcome $cluster_var                                    
                                                                                //The loneway command calculates the one-way ANOVA by a group variable. 
                                                                                //It gives the within-group variation and the between group variation of a variable. 
                                                                                //It also produces the intra-cluster correlation coefficient (ICC)
    
local rho = `r(rho)'
    
power twomeans `baseline' `treat', cluster  k1(`num_clusters_control') kratio(`kratio') power(`power') sd(`sd') rho(`rho')
        
local clus_size_t = `r(M2)'
local clus_size_c = `r(M1)'
    
 di as error "The minimum size of each cluster should be `clus_size_c' in the control and `clus_size_t' in the treatment to etect an effect of `effect' with a probability of `power' if the effect is true, given `num_clusters_control' clusters in the control and the ratio of the number of treatment and control clusters as `kratio'"
    
 drop control_cluster

    
****************************************************************************************
******* 5c. Compute effect size for a given cluster size and number of clusters ********
****************************************************************************************

local power = 0.8                                                               //SPECIFY - desired power
local nratio = 1                                                                //SPECIFY - the ratio of experimental group to control group
local alpha =0.05                                                               //SPECIFY - the significance level
    
quietly sum $outcome if !missing($outcome)                                      //sum the outcome at baseline and record the mean and the standard deviation
local sd = `r(sd)'
local baseline = `r(mean)'

bysort $cluster_var: gen control_cluster = _n==1                                    
count if control_cluster & D==0                                        //count the number of control clusters

local num_clusters_control=`r(N)'                                                //SPECIFY number of clusters in the control group - Taken from dataset but can be specified by researcher 
    
local kratio = 1                                                                //SPECIFY - The ratio of the number of treatment clusters to the number of control clusters

local cluster_size_control = 50                                                 //SPECIFY - number of people in each cluster. This should be specified by the researcher
local mratio=1                                                                  //SPECIFY - the ratio of the cluster size in the treatment and the control

loneway $outcome $cluster_var                                                    //The loneway command calculates the one-way ANOVA by a group variable. 
                                                                                //It gives the within-group variation and the between group variation of a variable. 
                                                                                //It also produces the intra-cluster correlation coefficient (ICC)
    
local rho = `r(rho)'
    
power twomeans `baseline', cluster k1(`num_clusters_control') kratio(`kratio') mratio(`mratio') m1(`cluster_size_control') power(`power') sd(`sd') rho(`rho')  alpha(`alpha') table
    
local mde_cluster = round(`r(delta)',0.0001)
    
 di as error "The MDE is `mde_cluster' given `num_clusters_control' clusters in the control, ratio of the number of treatment and control clusters as `kratio', `cluster_size_control' units in the control, the ratio of units in each treatment and control cluster of `mratio', and power `power'."

 drop control_cluster

 cap log close
