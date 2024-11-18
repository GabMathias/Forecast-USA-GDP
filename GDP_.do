***********************************************************
********** DETERMINE THE FOLDER
***********************************************************
cd "C:\Users\gabri\OneDrive\Área de Trabalho\Mestrado\Curso Econometria 2\Trabalho"

***********************************************************
********** START LOG - RECORD RESULTS
***********************************************************
log using "RESULTADOS_VECM.smcl", replace


***********************************************************
* EXTRACT INFORMATION FROM FRED
***********************************************************
* 1) Get a FRED API Key: https://fredaccount.stlouisfed.org/apikeys

* 2) Set your API Key in Stata:
  set fredkey f9e0a245481fb94190ff1fd32b7a064a, permanently
* 3) Search for variables
  * Ex.: fredsearch consumer index monthly  
* 4) Import desired information
	import fred gdp, clear // import the Gross Domestic Product (GDP) series
	
	***********************************************************    	***********************************************************    
	
  * Delete the variables that we are not going to use
  drop datestr 

	* Delete missing information
	drop if missing(GDP)

	* Keep the last 120 observations 
	keep in -120/L

	* Define time variable without gaps
	gen data=_n
	tset data

	* Save
	save GDP, replace

***********************************************************
********** SEPARATE INTO TRAINING AND TESTING
***********************************************************
* WE WILL USE PART OF THE SAMPLE TO ESTIMATE THE MODEL AND ANOTHER AS A COMPARISON.
  * TRAINING - 84 quarters (70% of the sample)
  * TEST - 36 quarters (30% of the sample)
	keep in 1/84
	

***********************************************************
*********** CHOOSING THE MODEL USING TRAINING SET
***********************************************************

***********************************************************
*********** DETERMINING THE TIME SERIES
***********************************************************
* comand: tset var_date
tset data

***********************************************************
*********** GRAPHICAL ANALYSIS
***********************************************************
***********************************************************
* INSTALL GRAPHICS STYLE PACKS (examples: white_w3d, tab1, tab2, gg_tableau, white_tableau, swift_red)
* for more styles see: https://github.com/asjadnaqvi/stata-schemepack
*ssc install schemepack, replace
*ssc install grstyle, replace
***********************************************************
tsline GDP, ///
     scheme(gg_tableau) /// 
	 ytitle("Quarterly GDP ") ///
	 xtitle("") ///
	 xlabel(, angle(45)) ///
     title("{bf} GDP - 01/1994 a 03/2014", pos(11) size(2.75))

	 
* INCLUDE AVERAGE GDP IN THE GRAPH
* Create variable with the average
egen GDP_medio = mean(GDP)

*Include average in graph
twoway (tsline GDP, legend(label(1 "GDP"))) (tsline GDP_medio, legend(label(2 "Average GDP"))), ///
     scheme(gg_tableau) /// 
	 ytitle("Quarterly GDP") ///
	 xtitle("") ///
	 xlabel(, angle(45)) ///
     title("{bf}GDP - 01/1994 a 03/2014", pos(11) size(2.75))

*From the graphical analysis we can speculate that the series is probably not stationary; it appears to have a positive deterministic trend. Let's use the unit root statistical tests to be sure.

* Export graph as png	
graph export "GDP.png", as(png)
	
	
***********************************************************
********** 1) CHECK STATIONARITY
***********************************************************

***********************************************************
* 1.1) DICKEY-FULLER	
*The DF test is one of the most widely used tests to determine whether a time series has a unit root, that is, whether it is stationary.
***********************************************************
* 1.1.1) Estimate: Dy =  alpha*y + drift + trend*t + e
*** H0: alpha = drift = trend = 0 (The series is not stationary)
dfuller GDP, trend 
	* Do not reject H0, Z(t)=-1.288>Z(5%)=-3.467 | P-Value = 89.09%>5%

* 1.1.2) Estimate: Dy =  alpha*y + drift + e
*** H0: alpha = drift = 0 (The series is not stationary)
dfuller GDP, drift 
	* Do not reject H0, Z(t)= 1.002>Z(5%)=-1.664 | P-Value = 84.04%>5%

* 1.1.3) Estimate: Dy =  alpha*y  + e
*** H0: alpha = 0 (The series is not stationary)
dfuller GDP, nocons 
	* Do not reject H0, Z(t)=12.802>Z(5%)=--1,950 

*In all tests we do not reject the null hypothesis, therefore the series is not stationary.
*When there is an autocorrelation process in the series, the Dickey-Fuller test tends not to reject more often that the series has a unit root. 
*To consider the autocorrelation process that the series may have, we will consider the augmented Dickey-Fuller test.

***********************************************************
* 1.2) AUGMENTED DICKEY-FULLER 
*The ADF test is a more robust version of the original Dickey-Fuller test, as it includes
*additional terms in the regression to control for autocorrelation and seasonality
*present in the data.
***********************************************************
* comand: dfuller var_name, lags(p)
*To find the maximum number of lags, we use the formula:
* p_max = int[12*(T/100)^(1/4)] = 11,  since the sample has 84 observations.

* 1.2.1) Estimate: Dy =  alpha*y + drift + trend*t + e
*** H0: alpha = drift = trend = 0 (The series is not stationary)
dfuller GDP, trend lags(11) regress		
	* Do not reject H0, Z(t)=-2.641 >Z(5%)=-3.478 | P-Value = 26.15%>5%

	*We can visualize that the p-value of the constant and the trend variable have p-values ​​within the significance level, so it makes sense to consider them.
	*trend_ P-Value = 0.01
	*cons_ P-Value = 0.006
	
* 1.2.2) Estimate: Dy =  alpha*y + drift + e
*** H0: alpha = drift = 0 (The series is not stationary)
dfuller GDP, drift lags(11) regress
	* Do not reject H0, Z(t)=0.330>Z(5%)=-3.467 | P-Value = 62.88%>5%
	

* 1.2.3) Estimate: Dy =  alpha*y  + e
*** H0: alpha = 0 (The series is not stationary)
dfuller GDP, nocons lags(11) regress
	* Do not reject H0, Z(t)=2.448>Z(5%)=-1.950


*We can conclude from the tests and graphical analysis that the series is not stationary.
*To take other issues into account, we will again test whether the series has a unit root with the Phillips-Perron test.

***********************************************************
* 1.3) PHILLIPS-PERRON
*The Phillips-Perron test, also known as the PP test, is another method used to check the stationarity of time series. It works in a similar way to the Dickey-Fuller (ADF) test, but has some advantages:
*Greater power for series with structural breaks: The PP test is more robustfor detecting unit roots in time series that have undergone structural breaks (abrupt changes in the level or trend of the series).
*Less sensitive to the choice of lags: The PP test is less sensitive to the choice of the number of autocorrelation terms (lags) included in the regression, which can be an advantage in some cases.
***********************************************************

* 1.3.1) Estimate: Dy =  alpha*y + drift + trend*t + e
*** H0: alpha = drift = trend = 0 (The series is not stationary)
pperron GDP, trend 
* Do not reject H0, Z(t)=-1.925>Z(5%)=-3.467 | P-Value = 64.14%>5%
	 
* 1.3.2) Estimate: Dy =  alpha*y + drift + e
*** H0: alpha = drift = 0 (The series is not stationary)
pperron GDP
* Do not reject H0, Z(t)=0.616>Z(5%)=-2.904| P-Value = 98.80%>5%

* 1.3.3) Estimate: Dy =  alpha*y  + e
*** H0: alpha = 0 (The series is not stationary)
pperron GDP, nocons 
* Do not reject H0, Z(t)= 8.785>Z(5%)=-1.950

*The PHILLIPS-PERRON tests corroborate the results of the DICKEY-FULLER test, so we can confidently conclude that the series is not stationary.
   
***********************************************************
********** 2) TRANSFORM INTO STATIONARY
***********************************************************
*To extract short- and long-term trends from a time series, simply differentiate it, i.e. subtract the first lag from both sides of the equation.
***********************************************************
* 2.1) CALCULATE THE FIRST DIFFERENCE
***********************************************************
gen dGDP = GDP - L.GDP

***********************************************************
*********** GRAPHICAL ANALYSIS
***********************************************************
* Graphic
tsline dGDP, ///
     scheme(gg_tableau) /// 
	 ytitle("") ///
	 xtitle("") ///
	 xlabel(, angle(45)) ///
     title("{bf}GDP - EUA", pos(11) size(2.75))
	
	 
* INCLUDE AVERAGE VARIATION IN THE GRAPH
* Create variable with the average
egen dGDP_medio = mean(dGDP)

* Include average in graph
twoway (tsline dGDP, legend(label(1 "dGDP"))) (tsline dGDP_medio, legend(label(2 "dGDP Avg"))), ///
     scheme(gg_tableau) /// 
	 ytitle("Quarterly GDP") ///
	 xtitle("") ///
	 xlabel(, angle(45)) ///
	 title("{bf}GDP - EUA", pos(11) size(2.75))
	 
	* We can infer from the graphic that the series presents stable behavior around the average.

	 
* Visualize GDP and GDP Variation on same Graphic	 
	 twoway (tsline GDP, legend(label(1 "GDP"))) (tsline dGDP, legend(label(2 "GDP Variation"))), ///
     scheme(gg_tableau) /// 
	 ytitle("") ///
	 xtitle("") ///
	 xlabel(, angle(45)) ///
     title("{bf}GDP Variation - USA", pos(11) size(2.75))


***********************************************************
******* 2.2) CHECK STATIONARITY OF THE FIRST DIFFERENCE
***********************************************************
	 
	 * 2.2.1) DICKEY-FULLER	
***********************************************************
* 2.2.1.1) Estima: Dy =  alpha*y + drift + trend*t + e
*** H0: alpha = drift = trend = 0 (The series is not stationary)
dfuller dGDP, trend 
	* Rejects H0, Z(t)=-5.825<Z(5%)=-3.468 | P-Value = 0.00%<5%

* 2.2.1.2) Estima: Dy =  alpha*y + drift + e
*** H0: alpha = drift = 0 (The series is not stationary)
dfuller dGDP, drift 
	* Rejects H0, Z(t)=-5.823<Z(5%)=-1.664  | P-Value = 0.00%<5% 

* 2.2.1.3) Estima: Dy =  alpha*y  + e
*** H0: alpha = 0 ((The series is not stationary)
dfuller dGDP, nocons 
	* Rejects H0, Z(t)=-2.863 <Z(5%)=-1.950 
	
*In all tests we reject the null hypothesis, therefore the series is stationary

***********************************************************
* 2.2.2) AUGMENTED DICKEY-FULLER 
***********************************************************

* 2.2.2.1) Estimate: Dy =  alpha*y + drift + trend*t + e
*** H0: alpha = drift = trend = 0 (The series is not stationary)
dfuller dGDP, trend lags(11) regress
	* Do not reject H0, Z(t)=-2.866>Z(5%)=-3.468 | P-Value = 17.37%>5%

* 2.2.2.2) Estimate: Dy =  alpha*y + drift + e
*** H0: alpha = drift = 0 (The series is not stationary)
dfuller dGDP, drift lags(11) regress
	* Rejects H0, Z(t)=-2.870<Z(5%)= -1.672  | P-Value = 00.29%<5%

* 2.2.2.3) Estimate: Dy =  alpha*y  + e
*** H0: alpha = 0 (The series is not stationary)
dfuller dGDP, nocons lags(11) regress
	* Do not reject H0, Z(t)=-0.610>Z(5%)=-3.468

***********************************************************
* 2.2.3) PHILLIPS-PERRON
***********************************************************

* 2.2.3.1) Estima: Dy =  alpha*y + drift + trend*t + e
*** H0: alpha = drift = trend = 0 (The series is not stationary)
pperron dGDP, trend 
	* Rejects H0, Z(t)=-5.914<Z(5%)=-3.468  | P-Value = 0.00%<5%
	 
* 2.2.3.2) Estima: Dy =  alpha*y + drift + e
*** H0: alpha = drift = 0 (The series is not stationary)
pperron dGDP
	* Rejects H0, Z(t)=-5.901<Z(5%)=-2.904  

* 2.2.3.3) Estima: Dy =  alpha*y  + e
*** H0: alpha = 0 (The series is not stationary)
pperron dGDP, nocons 
	* Rejects H0, Z(t)=-2.464<Z(5%)=-1.950 

* EVEN THOUGH THERE IS EVIDENCE THAT THE SERIES IS NOT STATIONARY USING THE AUGMENTED DICKEY-FULLER, THE OTHERS INDICATE STATIONARITY, AND THE GRAPH INDICATES A STATIONARY SERIES.
* WE HAVE EVIDENCE THAT VARIATION IN GDP IS STATIONARY
***********************************************************
* 3.1) GRAPH ACF (AUTOCORRELATION FUNCTION): ac var
***********************************************************
ac dGDP, ///
	scheme(white_w3d) ///
	ytitle("Autocorrelation (ACF)") ///
	title("ACF - GDP ", pos(11) size(2.75))
* We can see that the ACF truncates at 2. The order of the MA is possibly 2.

***********************************************************
* 3.2) GRAPH PACF (PARTIAL AUTOCORRELATION FUNCTION): pac var
***********************************************************
pac dGDP, ///
	scheme(white_w3d) ///
	ytitle("ParciaL Autocorrelation  (PACF)") ///
	title("FACP - GDP", pos(11) size(2.75)) 
* We can see that the PACF truncates at 1. The order of the AR is possibly 1.

***********************************************************
* 3.3) CORRELOGRAM: corrgram var
***********************************************************
corrgram dGDP

*According to the correlogram analysis, we will consider pmax = 3 and qmax = 2.

***********************************************************
*Information criteria
***********************************************************
arimasoc L.GDP, maxar(3) maxma(2)

* POSSIBILITIES: 
* ARMA(P_MAX,Q_MAX)= ARIMA(0,1,0), ARIMA(1,1,0), ARIMA(0,1,1), ARIMA(1,1,1), ARIMA(2,1,0), ARIMA(0,1,2), ARIMA(2,1,1),ARIMA(1,1,2), ARIMA(2,1,2), ARIMA(3,1,0), ARIMA(3,1,1), 
* TEST ARIMA(2,1,1), ARIMA(3,1,0), ARIMA(3,1,1)

***********************************************************
* ESTIMATION: arima var, arima (p i q)
***********************************************************
* p: order of ar
* i: order of integration (i=0 for stationary series)
* q: order of ma
***********************************************************
* ESTIMATING ARIMA(2,1,1)
arima dGDP, arima(2 1 1)
  *SAVING THE ESTIMATED MODEL
    estimates store arima_211
 * SAVING THE MODEL RESIDUAL
    predict erro_211, residuals
 
* ESTIMATING ARIMA(3,1,0)
arima dGDP, arima(3 1 0)
 * SAVING THE ESTIMATED MODEL
    estimates store arima_310
 * SAVING THE MODEL RESIDUAL	
    predict erro_310, residuals
	
* ESTIMATING ARIMA(3,1,1)
arima dGDP, arima(3 1 1)
  * SAVING THE ESTIMATED MODEL
    estimates store arima_311
  * SAVING THE MODEL RESIDUAL	
    predict erro_311, residuals


***********************************************************
* RESIDUAL DIAGNOSIS
***********************************************************

***********************************************************
********** 5) RESIDUAL DIAGNOSIS
***********************************************************
***********************************************************
* 5.1) ACF and PACF: CHECK IF THE ERROR HAS NO MEMORY
***********************************************************
  * 5.1.1) ACF GRAPH OF RESIDUALS
  
  	*ARIMA(2,1,1)
	ac erro_211, ///
		scheme(white_w3d) ///
		ytitle("Autocorrelation (ACF)") ///
		title("ACF - Residual of ARIMA(2,1,1)", pos(11) size(2.75))
		
	*ARIMA(3,1,0)
	ac erro_310, ///
		scheme(white_w3d) ///
		ytitle("Autocorrelation (ACF)") ///
		title("ACF - Residual of ARIMA(3,1,0)", pos(11) size(2.75))
		
	*ARIMA(3,1,1)
	ac erro_311, ///
		scheme(white_w3d) ///
		ytitle("Autocorrelation (ACF)") ///
		title("ACF - Residual of ARIMA(3,1,1)", pos(11) size(2.75))

		
	* 5.1.2)  PACF GRAPH OF RESIDUALS
	
	*ARIMA(2,1,1)
	pac erro_211, ///
		scheme(white_w3d) ///
		ytitle("Parcial Autocorrelation (PACF)") ///
		title("PACF - Residual of ARIMA(3,1,1)", pos(11) size(2.75))
	
	*ARIMA(3,1,0)
	pac erro_310, ///
		scheme(white_w3d) ///
		ytitle("Parcial Autocorrelation (PACF)") ///
		title("PACF - Residual of ARIMA(3,1,0)", pos(11) size(2.75))
		
	*ARIMA(3,1,1)
	pac erro_311, ///
		scheme(white_w3d) ///
		ytitle("Parcial Autocorrelation (PACF)") ///
		title("PACF - Residual of ARIMA(3,1,1)", pos(11) size(2.75))

		
    * 1.3) CORRELOGRAM
	
	*ARIMA(2,1,1)
	corrgram erro_211
	
	*ARIMA(3,1,0)
	corrgram erro_310
	
	*ARIMA(3,1,1)
	corrgram erro_311


***********************************************************	
* TEST 2 - LJUNG-BOX TEST
* CHECK IF THE ERROR HAS NO MEMORY
***********************************************************
* H0: ERROR NO MEMORY
* H1: ERROR HAS MEMORY

************** ARIMA(2,1,1)
	wntestq erro_211
  * Since the p-value=39.60%>10%, H0 is not rejected (and H1 is validated), we have no evidence that the error has memory
************** ARIMA(3,1,0)
	wntestq erro_310
  * Since the p-value=39.60%>10%, H0 is not rejected (and H1 is validated), we have no evidence that the error has memory	
************** ARIMA(3,1,1)
	wntestq erro_311
  * Since the p-value=39.60%>10%, H0 is not rejected (and H1 is validated), we have no evidence that the error has memory	

	
***********************************************************
* TEST 3 - LM TEST
* CHECK IF THE ERROR HAS NO MEMORY
***********************************************************
* H0: ERROR HAS NO MEMORY
* H1: ERROR HAS MEMORY
	
************** ARIMA(2,1,1)
	reg erro_211 L.erro_211 L2.erro_211 L3.erro_211 L4.erro_211 L5.erro_211
    estat bgodfrey, lags(5)  
  * Since p-value=47.68%>10%, H0 is not rejected (and H1 is not validated), we have no evidence that the error has memory	
	
************** ARIMA(3,1,0)
	reg erro_310 L.erro_310 L2.erro_310 L3.erro_310  L4.erro_310  L5.erro_310
    estat bgodfrey, lags(5)  
  * Since p-value=45.19%>10%, H0 is not rejected (and H1 is not validated), we have no evidence that the error has memory	
	
************** ARIMA(3,1,1)
	reg erro_311 L.erro_311 L2.erro_311 L3.erro_311 L4.erro_311 L5.erro_311
    estat bgodfrey, lags(5)  
	* Since the p-value=61.32%>10%, H0 is not rejected (and H1 is not validated), we have no evidence that the error has memory
	


***********************************************************
* TEST 4 - ARCH-LM TEST
* CHECK IF ERRORS ARE HOMOSCEDASTIC
***********************************************************
* HO: ERRORS ARE HOMOSCEDASTIC
* H1: ERRORS ARE HETEROSCEDASTIC

* INSTALL THE COMMAND:
* ssc install archlm
    

************** ARIMA(2,1,1)
    * 3.1) RUN A MODEL TO DEFINE Y, IN THIS CASE IS THE RESIDUAL
	 reg erro_211
    * 3.2) RUN THE TEST (a total of 11 time lags were chosen here)
    estat archlm, lags(1/11)
	
	
************** ARIMA(3,1,0)
    * 3.1) RUN A MODEL TO DEFINE Y, IN THIS CASE IS THE RESIDUAL
	 reg erro_310
    * 3.2) RUN THE TEST (a total of 11 time lags were chosen here)
    estat archlm, lags(1/11)
	
************** ARIMA(3,1,1)
    * 3.1) RUN A MODEL TO DEFINE Y, IN THIS CASE IS THE RESIDUAL
	 reg erro_311
    * 3.2) RUN THE TEST (a total of 11 time lags were chosen here)
    estat archlm, lags(1/11)


		
	
***********************************************************
* TEST 5 - JARQUE-BERA TEST
* CHECK IF ERRORS ARE NORMAL
***********************************************************
  * HO: ERRORS ARE NORMAL
  * H1: ERRORS ARE NOT NORMAL

  * INSTALL THE COMMAND:
  * ssc install jb

  * RUN THE TEST

************** ARIMA(2,1,1)
	jb erro_211
	
************** ARIMA(3,1,0)
	jb erro_310
	
************** ARIMA(3,1,1)
	jb erro_311
	
	
***********************************************************
********** INFORMATION CRITERIA
***********************************************************

estimates stat arima_211 arima_310 arima_311

***********************************************************
*********** FORECAST
**********************************************************
*******************************************************
* ESTIMATE USING TRAINING, TEST USING TEST
***********************************************************
use GDP, clear
tset data

* ESTIMATE THE MODEL WITH THE FIRST 84 DATES
arima GDP in 1/84, arima(3 1 1) 
 
* PREDICT ERROR - DYNAMIC ESTIMATION
predict GDP_prev_d, y dynamic(84)
replace GDP_prev_d=. in 1/84


* PREDICT ERROR - STATIC ESTIMATION
predict GDP_prev_e in 85/120, y

*** DYNAMIC FORECAST ERROR
* CALCULATE ERROR - FOR OBSERVATIONS NOT USED IN ESTIMATION
gen erro_d = GDP - GDP_prev_d 
* SQUARED ERROR
gen erro2_d = erro_d^2 
* ABSOLUTE VALUE OF ERROR
gen abs_erro_d = abs(erro_d) 
* PERCENTAGE OF ERROR IN RELATION TO THE REAL VALUE
gen p_erro_d = erro_d/GDP 
* ABSOLUTE VALUE OF THE PERCENTAGE OF ERROR IN RELATION TO THE REAL VALUE
gen abs_p_erro_d = abs(p_erro_d) 

* CALCULATE MSE - mean square (prediction) error 
egen mse_d = mean(erro2_d) in 85/120
replace mse_d = sqrt(mse_d)
* CALCULATE MAE -  mean absolute (prediction) error
egen mae_d = mean(abs_erro_d) in 85/120
* CALCULATE MAPE - mean absolute percentual error
egen mape_d = mean(abs_p_erro_d) in 85/120



*** STATIC FORECAST ERROR
* CALCULATE ERROR - FOR OBSERVATIONS NOT USED IN ESTIMATION
gen erro_e = GDP - GDP_prev_e 
* SQUARED ERROR
gen erro2_e = erro_e^2 
* ABSOLUTE VALUE OF ERROR
gen abs_erro_e = abs(erro_e) 
* PERCENTAGE OF ERROR IN RELATION TO THE REAL VALUE
gen p_erro_e = erro_e/GDP
* ABSOLUTE VALUE OF THE PERCENTAGE OF ERROR IN RELATION TO THE REAL VALUE
gen abs_p_erro_e = abs(p_erro_e) 

* CALCULATE MSE - mean square (prediction) error 
egen mse_e = mean(erro2_e) in 85/120
replace mse_e = sqrt(mse_e)
* CALCULATE MAE -  mean absolute (prediction) error
egen mae_e = mean(abs_erro_e) in 85/120
* CALCULATE MAPE - mean absolute percentual error
egen mape_e = mean(abs_p_erro_e) in 85/120

* TABLE
tabstat mse_d mae_d mape_d mse_e mae_e mape_e, stat(mean)



* GRAPHIC - Static Forecast vs Dynamic Forecast:
twoway (tsline GDP) ///
       (tsline GDP_prev_e) ///
       (tsline GDP_prev_d) in 80/90, ///
	   scheme(white_tableau) ///
	   xlabel(, angle(45)) ///
	   tline(85, lcolor(red) lpattern(dash)) ///
	   title("{bf}Previsão - GDP", pos(11) size(2.75)) ///
       legend(label(1 "GDP") label(2 "Previsão Estática") label(3 "Previsão Dinâmica")) 


* GRAPH - Static Forecast and CI:
* Create the limits for the confidence interval
scalar z_5 = invnormal(0.95) // z_5 = 1.96
 * Lower Limit of IC
   gen lim_inf_95 = GDP_prev_e - z_5*sqrt(mse_e) 
 * Upper Limit of IC
   gen lim_sup_95 = GDP_prev_e + z_5*sqrt(mse_e)  

twoway (tsline GDP) ///
       (tsline GDP_prev_e) ///
       (tsrline lim_inf_95 lim_sup_95,lpattern(vshortdash)) in 80/90, ///
	   scheme(white_tableau) ///
	   xlabel(, angle(45)) ///
	   tline(85, lcolor(red) lpattern(dash)) ///
	   title("{bf}Forecast - GDP", pos(11) size(2.75)) ///
       legend(label(1 "GDP") label(2 "Static Forecast") label(3 "IC - 95%")) 
