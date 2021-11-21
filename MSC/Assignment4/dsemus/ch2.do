* Path.do

use path, clear
***********************************************************************
* Math & Reading at age 21 using SEM
***********************************************************************
* Section 2.3 simple path model
sem (math7 <- attention4) 											///
    (read7 <- attention4) 											///     
    (math21 <- attention4 math7 read7), 							///
    method(mlmv) standardize
estat eqgof
estat gof, stats(all)
estat teffects, standardize
estat mindices
* simple path model with correlated residuals
sem (math7 <- attention4) 											///
    (read7 <- attention4) 											///     
    (math21 <- attention4 math7 read7), 							///
    method(mlmv) cov(e.math7*e.read7) standardize  
estat eqgof  
estat gof, stats(all)
estat teffects, standardize
estat mindices
***********************************************************************
* Adding covariates
***********************************************************************    
sem (math7 <- attention4 vocab4 adopted male momed) 				///
    (read7 <- attention4 vocab4 adopted male momed) 				///     
    (math21 <- attention4 vocab4 adopted male momed math7 read7), 	///
    method(mlmv) covstr(attention4 vocab4 adopted male attention4,	///
    unstructured) cov(e.math7*e.read7) standardize 
estat eqgof  
estat gof, stats(all)
estat teffects, standardize
estat mindices

***********************************************************************
* Testing specific indirect effects
***********************************************************************
sem (math7 <- attention4) 											///
    (read7 <- attention4) 											///     
    (math21 <- attention4 math7 read7), 							///
    method(mlmv) cov(e.math7*e.read7) standardize 
sem, coeflegend
estat stdize: 				///
    nlcom (attn_m7_m21: _b[math7:attention4]*_b[math21:math7]) 		///
    (attn_r7_m7: _b[read7:attention4]*_b[math21:read7])
estat teffects, nodirect nototal standardize    
 

***********************************************************************
* Bootstrap Estimation
***********************************************************************
sktest attention4 math7 read7 math21
sem (math7 <- attention4 vocab4 adopted male momed) 				///
    (read7 <- attention4 vocab4 adopted male momed) 				///     
    (math21 <- attention4 vocab4 adopted male momed math7 read7), 	///
    method(mlmv) vce(bootstrap, reps(1000) seed(111)) standardize 	///
    covstr(attention4 vocab4 adopted male attention4,				///
    unstructured) cov(e.math7*e.read7) 
estat eqgof  
estat gof, stats(all)
estat teffects, standardized

***********************************************************************
* Listwise solution
***********************************************************************
sem (math7 <- attention4 vocab4 adopted male momed) 				///
    (read7 <- attention4 vocab4 adopted male momed) 				///     
    (math21 <- attention4 vocab4 adopted male momed math7 read7), 	///
    method(ml) vce(bootstrap, reps(1000) seed(111)) standardize 	///
    covstr(attention4 vocab4 adopted male attention4,				///
    unstructured) cov(e.math7*e.read7) 
estat eqgof  
estat gof, stats(all)
estat teffects, standardized

***********************************************************************
* Testing equality of coefficients
***********************************************************************
sem (math7 <- attention4) 											///
    (read7 <- attention4) 											///     
    (math21 <- attention4 math7 read7), 							///
    method(mlmv) cov(e.math7*e.read7) standardize 
sem, coeflegend
estat stdize: test _b[math21:math7] == _b[math21:read7]
    
zscore math7 read7 , stub(z_)
zscore math21, stub(z_)
gen math7r = 100 + (z_math7*15)
gen read7r = 100 + (z_read7*15)
gen math21r = 100 + (z_math21*15)
sum math7 read7 math7r read7r math21 math21r

sem (math7r <- attention4) 											///
    (read7r <- attention4) 											///     
    (math21r <- attention4 math7r read7r), 							///
    method(mlmv) cov(e.math7r*e.read7r) 
    estat eqgof
sem, coeflegend
test _b[math21r:math7r] == _b[math21r:read7r]

***********************************************************************
* Moderation
***********************************************************************
* simple path model

sysuse auto, clear
gen wgt1000s = weight/1000
gen wgt1000sXforeign = wgt1000s*foreign
regress mpg wgt1000s
regress mpg wgt1000s foreign
regress mpg c.wgt1000s##i.foreign
margins, at( wgt1000s=( 2 3 4 5) foreign =(0 1))
marginsplot
sem (mpg <- wgt1000s)
estat eqgof
predict mpgp
sem (mpg <- wgt1000s foreign)
estat eqgof

sem (mpg <- wgt1000s foreign wgt1000sXforeign)
estat eqgof
predict mpgpf
twoway (scatter mpgpf wgt1000s if foreign==1, mcolor(black) msymbol(diamond_hollow)) ///
   (scatter mpgpf wgt1000s if foreign==0, mcolor(black) msymbol(circle_hollow)), ///
   ytitle(Estimated Miles Per Gallon) xtitle(Car's Weight in 1000s of pounds) legend(order(1 ///
   "Foreign" 2 "Domestic"))

************************************************************************
* Nonrecursive Model
************************************************************************
use http://www.stata-press.com/data/r12/sem_sm1.dta, clear
sem (r_occasp <- f_occasp r_intel r_ses f_ses)		///
    (f_occasp <- r_occasp f_intel f_ses r_ses), 	///
    cov( e.r_occasp*e.f_occasp) standardized 
estat stable
sem, coeflegend
test (_b[r_occasp:r_intel ]==_b[f_occasp:f_intel ]) 
test (_b[r_occasp:r_intel ]==_b[f_occasp:f_intel ])  ///
	 (_b[r_occasp:r_ses ]==_b[f_occasp:f_ses ]) 	///
	 (_b[r_occasp:f_ses ]==_b[f_occasp:r_ses ]) 	///
	 (_b[r_occasp:f_occasp]==_b[f_occasp:r_occasp])

sem (r_occasp <- f_occasp@b1 r_intel@b2 r_ses@b3 f_ses@b4)		///
    (f_occasp <- r_occasp@b1 f_intel@b2 f_ses@b3 r_ses@b4), 	///
    cov( e.r_occasp*e.f_occasp) 
estat eqgof


