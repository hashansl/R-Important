* Ch4 growth curve.do
clear
use bmiworking.dta
sort id
keep id bmi01-bmi09 
reshape long bmi0, i(id) j(year)
label define age 1 "20" 2 "21" 3 "22" 4 "23" 5 "24" 6 "25" 7 "26" 8 "27" 9 "28"
label values year age
regress bmi0 year, cluster(id)
predict yhat



/* This command will only show 2 of the 10 lines when run

   in Small Stata */
twoway (lfit yhat year, lwidth(thick))					///
  (line bmi0 year if id == 14, 							///
    lcolor(black) lwidth(medthin) lpattern(longdash))	///
  (line bmi0 year if id == 137,							///
    lcolor(black) lwidth(medthin) lpattern(longdash))	///
  (line bmi0 year if id == 153,							///
    lcolor(black) lwidth(medthin) lpattern(longdash))	///
  (line bmi0 year if id == 212, 						///
    lcolor(black) lwidth(medthin) lpattern(longdash))	///
  (line bmi0 year if id == 216,							///
     lcolor(black) lwidth(medthin) lpattern(longdash))	///
  (line bmi0 year if id == 260,							///
     lcolor(black) lwidth(medthin) lpattern(longdash))	///
  (line bmi0 year if id == 287,							///
     lcolor(black) lwidth(medthin) lpattern(longdash))	///
  (line bmi0 year if id == 1488,						///
     lcolor(black) lwidth(medthin) lpattern(longdash))	///
  (line bmi0 year if id == 2403,						///
     lcolor(black) lwidth(medthin) lpattern(longdash))	///
  (line bmi0 year if id == 8924,						///
     lcolor(black) lwidth(medthin) lpattern(longdash)),	///
     ytitle(Body mass index (BMI)) xtitle(Age) xlabel(1(1)9, ///
     valuelabel) legend(off)

* If you are familiar with xtmixed this would have advantages
xtmixed bmi0 year || id: year
predict bmihat

/* This command will only show two of the ten lines when run 

   in small Stata */
twoway (lfit bmihat year, lwidth(thick))					///
  (line bmi0 year if id == 14, 							///
    lcolor(black) lwidth(medthin) lpattern(longdash))	///
  (line bmi0 year if id == 137,							///
    lcolor(black) lwidth(medthin) lpattern(longdash))	///
  (line bmi0 year if id == 153,							///
    lcolor(black) lwidth(medthin) lpattern(longdash))	///
  (line bmi0 year if id == 212, 						///
    lcolor(black) lwidth(medthin) lpattern(longdash))	///
  (line bmi0 year if id == 216,							///
     lcolor(black) lwidth(medthin) lpattern(longdash))	///
  (line bmi0 year if id == 260,							///
     lcolor(black) lwidth(medthin) lpattern(longdash))	///
  (line bmi0 year if id == 287,							///
     lcolor(black) lwidth(medthin) lpattern(longdash))	///
  (line bmi0 year if id == 1488,						///
     lcolor(black) lwidth(medthin) lpattern(longdash))	///
  (line bmi0 year if id == 2403,						///
     lcolor(black) lwidth(medthin) lpattern(longdash))	///
  (line bmi0 year if id == 8924,						///
     lcolor(black) lwidth(medthin) lpattern(longdash)),	///
     ytitle(Body mass index (BMI)) xtitle(Age) xlabel(1(1)9, ///
     valuelabel) legend(off)

***************************************************************************
* Figure 4.2 Identification
***************************************************************************
clear
input time y2 y3 y4
0 2 2 2
1 . 6 6 
2 . . 4
4 6 6 6
end
twoway (scatter y2 time, mcolor(black) msize(large)) ///
  (lfit y2 time, lcolor(black) lpattern(longdash)), yscale(range(0 7)) ylabel(0(2)7) name(a, replace)
twoway (scatter y3 time, mcolor(black) msize(large)) ///
  (lfit y3 time, lcolor(black) lpattern(longdash)), yscale(range(0 7)) ylabel(0(2)7) name(b, replace)
twoway (scatter y4 time, mcolor(black) msize(large)) ///
  (lfit y4 time, lcolor(black) lpattern(longdash)), yscale(range(0 7)) ylabel(0(2)7) name(c, replace)
graph combine a b c
***************************************************************************
* ICC
***************************************************************************
clear
use bmiworking.dta, clear
keep id bmi01-bmi09 
reshape long bmi0, i(id) j(year)
label define age 1 "20" 2 "21" 3 "22" 4 "23" 5 "24" 6 "25" 7 "26" 8 "27" 9 "28"
label values year age
xtmixed bmi0 || id:
estat icc
* alternatives for estimating ICC
xtreg bmi0, i(id) mle
icc bmi0 id
clear
***************************************************************************
* Basic Growth Curve Model  with listwise deletion, that disappoints
***************************************************************************
use bmiworking.dta, clear
sem (Intercept@1 Slope@0 -> bmi01)										///
    (Intercept@1 Slope@1 -> bmi02)										///
    (Intercept@1 Slope@2 -> bmi03)										///
    (Intercept@1 Slope@4 -> bmi05)										///
    (Intercept@1 Slope@5 -> bmi06)										///
    (Intercept@1 Slope@6 -> bmi07)										///
    (Intercept@1 Slope@7 -> bmi08)										///
    (Intercept@1 Slope@8 -> bmi09)
use bmiworking.dta, clear
sem (Intercept@1 Slope@0 -> bmi01)										///
    (Intercept@1 Slope@1 -> bmi02)										///
    (Intercept@1 Slope@2 -> bmi03)										///
    (Intercept@1 Slope@4 -> bmi05)										///
    (Intercept@1 Slope@5 -> bmi06)										///
    (Intercept@1 Slope@6 -> bmi07)										///
    (Intercept@1 Slope@7 -> bmi08)										///
    (Intercept@1 Slope@8 -> bmi09), 									///
    method(mlmv) noconstant means(Intercept Slope)

estat gof, stats(all)

estat mindices
sum bmi01-bmi09 if !missing(bmi01, bmi02, bmi03, bmi05, bmi06, bmi07, 	///
    bmi08, bmi09)
sum bmi01-bmi09 if e(sample)
clear
input year bmi
0 25.41234
1 26.099
2 26.61132
4 27.03011
5 27.63835
6 27.8068
7 28.31192
8 28.32258
end
twoway (connected bmi year) (lfit bmi year, lcolor(black) ///
         lwidth(medthick) lpattern(longdash))
         
***************************************************************************
* Correlate adjacent error terms
***************************************************************************
use bmiworking.dta, clear   

sem (Intercept@1 Slope@0 -> bmi01)										///
    (Intercept@1 Slope@1 -> bmi02)										///
    (Intercept@1 Slope@2 -> bmi03)										///
    (Intercept@1 Slope@4 -> bmi05)										///
    (Intercept@1 Slope@5 -> bmi06)										///
    (Intercept@1 Slope@6 -> bmi07)										///
    (Intercept@1 Slope@7 -> bmi08)										///
    (Intercept@1 Slope@8 -> bmi09), 									///
    method(mlmv) noconstant means(Intercept Slope)
estimates store ind_errors
sem (Intercept@1 Slope@0 -> bmi01)										///
    (Intercept@1 Slope@1 -> bmi02)										///
    (Intercept@1 Slope@2 -> bmi03)										///
    (Intercept@1 Slope@4 -> bmi05)										///
    (Intercept@1 Slope@5 -> bmi06)										///
    (Intercept@1 Slope@6 -> bmi07)										///
    (Intercept@1 Slope@7 -> bmi08)										///
    (Intercept@1 Slope@8 -> bmi09), 									///
    method(mlmv) noconstant means(Intercept Slope)						///
    cov(e.bmi01*e.bmi02) cov(e.bmi02*e.bmi03) 							///
    cov(e.bmi05*e.bmi06) cov(e.bmi06*e.bmi07)							///
    cov(e.bmi07*e.bmi08)
estimates store corr_errors
estat gof, stats(all)
estat mindices
lrtest ind_errors corr_errors
sem (Intercept@1 Linear@0 Quadratic@0 -> bmi01)                 ///
    (Intercept@1 Linear@1 Quadratic@1 -> bmi02)                   ///
    (Intercept@1 Linear@2 Quadratic@4 -> bmi03)                   ///
    (Intercept@1 Linear@4 Quadratic@16 -> bmi05)                  ///
    (Intercept@1 Linear@5 Quadratic@25 -> bmi06)                  ///
    (Intercept@1 Linear@6 Quadratic@36 -> bmi07)                  ///
    (Intercept@1 Linear@7 Quadratic@49 -> bmi08)                  ///
    (Intercept@1 Linear@8 Quadratic@64 -> bmi09),                 ///
    method(mlmv) noconstant means(Intercept Linear Quadratic)
estat gof, stats(all)
estat mindices
***************************************************************************
* Quadratic 
***************************************************************************
sem (Intercept@1 Linear@0 Quadratic@0 -> bmi01)							///
    (Intercept@1 Linear@1 Quadratic@1 -> bmi02)							///
    (Intercept@1 Linear@2 Quadratic@4 -> bmi03)							///
    (Intercept@1 Linear@4 Quadratic@16 -> bmi05)						///
    (Intercept@1 Linear@5 Quadratic@25 -> bmi06)						///
    (Intercept@1 Linear@6 Quadratic@36 -> bmi07)						///
    (Intercept@1 Linear@7 Quadratic@49 -> bmi08)						///
    (Intercept@1 Linear@8 Quadratic@64 -> bmi09), 						///
    method(mlmv) noconstant means(Intercept Linear Quadratic)
estat eqgof  
estat gof, stats(all)
estat mindices
***************************************************************************
* Correlated Adjacent Errors Plus a Quadratic Term
***************************************************************************

/* This model cannot be fit in Small Stata */
sem (Intercept@1 Linear@0 Quadratic@0 -> bmi01)							///
    (Intercept@1 Linear@1 Quadratic@1 -> bmi02)							///
    (Intercept@1 Linear@2 Quadratic@4 -> bmi03)							///
    (Intercept@1 Linear@4 Quadratic@16 -> bmi05)						///
    (Intercept@1 Linear@5 Quadratic@25 -> bmi06)						///
    (Intercept@1 Linear@6 Quadratic@36 -> bmi07)						///
    (Intercept@1 Linear@7 Quadratic@49 -> bmi08)						///
    (Intercept@1 Linear@8 Quadratic@64 -> bmi09), 						///
    method(mlmv) noconstant 											///
    means(Intercept Linear Quadratic)									///
    cov(e.bmi01*e.bmi02) cov(e.bmi02*e.bmi03) 							///
    cov(e.bmi05*e.bmi06) cov(e.bmi06*e.bmi07)							///
    cov(e.bmi07*e.bmi08)
estat eqgof
estat gof, stats(all)
estat mindices
***************************************************************************
* Drawing a graph for quadratic growth curve
***************************************************************************
predict I L Q, latent(Intercept Linear Quadratic)
summarize I L Q if bmi01~=. | bmi02~=. | bmi03~=. | bmi05~=. | bmi06~=. | bmi07~=. ///
         | bmi08~=. | bmi09~=.
histogram I, normal saving(I)
histogram L, normal saving(L)
histogram Q, normal saving(Q)
graph combine I.gph L.gph Q.gph
estat eqgof  
estat gof, stats(all)
estat mindices
clear
input year
0
1
2
3
4
5
6
7
8
end
generate year_sq = year*year
generate bmiest = 25.488 + 0.494*year -0.018*year_sq
twoway (line bmiest year), ytitle(BMI) ylabel(25(.5)28.5) xtitle(Age) ///
     xlabel(0 "20" 1 "21" 2 "22" 3 "23" 4 "24" 5 "25" 6 "26" 7 "27" 8 "28")

***************************************************************************
* Time invariant covariate 
***************************************************************************
use bmiworking.dta, clear   
fre male deswgt97
generate deswgt0_97 = deswgt97-1
label define labels 0 "Very underweight" 1 "Slightly underweight" 2 "About right" ///
   3 "Slightly overweight" 4 "Very overweight"
label values deswgt0_97 labels
fre deswgt0_97
egen wgtmean = mean(deswgt97)
generate wgt_ctr = deswgt97-wgtmean
fre wgt_ctr
center deswgt97
fre c_deswgt97
sum deswgt97 c_deswgt97
tab deswgt97 c_deswgt97
sum male c_deswgt97 bmi*
sem   (Intercept@1 Slope@0 -> bmi01) ///
     (Intercept@1 Slope@1 -> bmi02) ///
     (Intercept@1 Slope@2 -> bmi03) ///
     (Intercept@1 Slope@4 -> bmi05) ///
     (Intercept@1 Slope@5 -> bmi06) ///
     (Intercept@1 Slope@6 -> bmi07) ///
     (Intercept@1 Slope@7 -> bmi08) ///
     (Intercept@1 Slope@8 -> bmi09) ///
     (Intercept Slope <- male c_deswgt _cons)     ///
     if bmi01~=.|bmi02~=.|bmi03~=.|bmi05~=.|bmi06~=.|  ///
     bmi07~=.|bmi08~=.|bmi09~=.,  ///
     var(e.Intercept*e.Slope)  ///
     method(mlmv) noconstant				
estat gof, stats(all)
estat eqgof
estat mindices



sem (Intercept@1 Slope@0 -> bmi01)										///
    (Intercept@1 Slope@1 -> bmi02)										///
    (Intercept@1 Slope@2 -> bmi03)										///
    (Intercept@1 Slope@4 -> bmi05)										///
    (Intercept@1 Slope@5 -> bmi06)										///
    (Intercept@1 Slope@6 -> bmi07)										///
    (Intercept@1 Slope@7 -> bmi08)										///
    (Intercept Slope <- male deswgt97 _cons),							///
    method(ml) noconstant cov(e.Intercept*e.Slope) standardized
estat eqgof  
estat gof, stats(all)
estat mindices, minchi(10)
estat teffects, standardized

***************************************************************************
* Time invariant covariate for random coefficient model
***************************************************************************
use bmiworking.dta, clear   
generate descwgt = deswgt97-1
label define d 0 "Very underweight" 1 "Slightly underweight" 		///
    2 "About the right weight" 3 "Slightly overweight" 4 "Very overweight"
label values descwgt d
fre descwgt
sem (Intercept@1 Slope@0 _cons@0 -> bmi01)							///
    (Intercept@1 Slope@1 _cons@0 -> bmi02)							///
    (Intercept@1 Slope@2 _cons@0 -> bmi03)							///
    (Intercept@1 Slope@4 _cons@0 -> bmi05)							///
    (Intercept@1 Slope@5 _cons@0 -> bmi06)							///
    (Intercept@1 Slope@6 _cons@0 -> bmi07)							///
    (Intercept@1 Slope@7 _cons@0 -> bmi08)							///
    (Intercept@1 Slope@8 _cons@0 -> bmi09)							///
    (Intercept Slope <- male deswgt97 _cons),						///
    latent(Intercept Slope) var(e.Intercept*e.Slope)
estat eqgof  
estat gof, stats(all)
estat mindices
input year
0
1
2
3
4
5
6
7
8
end
generate bmiest = 25.502 if year == 0
replace bmiest = 25.502 + 0.494*1 - 0.010*1  if year == 1
replace bmiest = 25.502 + 0.494*2 - 0.010*4  if year == 2
replace bmiest = 25.502 + 0.494*3 - 0.010*9  if year == 3
replace bmiest = 25.502 + 0.494*4 - 0.010*16   if year == 4
replace bmiest = 25.502 + 0.494*5 - 0.010*25   if year == 5
replace bmiest = 25.502 + 0.494*6 - 0.010*36   if year == 6
replace bmiest = 25.502 + 0.494*7 - 0.010*49   if year == 7
replace bmiest = 25.502 + 0.494*8 - 0.010*64   if year == 8
twoway (qfit bmiest year), ytitle(BMI) ylabel(25(.5)30) xtitle(Age) ///
     xlabel(0 "20" 1 "21" 2 "22" 3 "23" 4 "24" 5 "25" 6 "26" 7 "27" 8 "28")

/* figure 4.10 */

clear
input y Wave
2.0 0
2.0 1
2.4 2
2.8 3
2.8 4
end

twoway scatter y Wave, msymbol(diamond) mcolor(black) || ///
line y Wave if _n==1 | _n==5, ///
lcolor(gs7) ytitle("") ylabel(1(.5)3) legend(off)

***************************************************************************
* Time Invariant plus Time Varying Covariates
***************************************************************************
use bmiworking.dta, clear   
recode drkdays* (.a/.e=.)
center drkdays* deswgt97

/* This model cannot be fit in Small Stata */
sem   (Intercept@1 Slope@0 c_drkdays01   -> bmi01) ///
      (Intercept@1 Slope@1 c_drkdays02   -> bmi02) /// 
      (Intercept@1 Slope@2 c_drkdays03   -> bmi03) ///                                                                     
      (Intercept@1 Slope@4 c_drkdays05   -> bmi05) ///                                                                     
      (Intercept@1 Slope@5 c_drkdays06   -> bmi06) ///                                                                     
      (Intercept@1 Slope@6 c_drkdays07   -> bmi07) ///                                                                     
      (Intercept@1 Slope@7 c_drkdays08   -> bmi08) ///
      (Intercept@1 Slope@8 c_drkdays09   -> bmi09) ///
      (Intercept Slope <- male c_deswgt _cons)           ///
      if bmi01~=.|bmi02~=.|bmi03~=.|bmi05~=.|bmi06~=.|  ///
      bmi07~=.|bmi08~=.|bmi09~=.,  ///         
      cov(e.Intercept*e.Slope)  ///
      method(mlmv) noconstant 
estat eqgof
estat gof, stats(all)
estat mindices 
**************************************************************************
* Time Invariant plus Time Varying Covariates plus Equal Errors
**************************************************************************
use bmiworking.dta, clear   
recode drkdays* (.a/.e=.)
center drkdays* deswgt97

/* This model cannot be fit in Small Stata */
sem   (Intercept@1 Slope@0 c_drkdays01   -> bmi01) ///
      (Intercept@1 Slope@1 c_drkdays02   -> bmi02) /// 
      (Intercept@1 Slope@2 c_drkdays03   -> bmi03) ///                                                                     
      (Intercept@1 Slope@4 c_drkdays05   -> bmi05) ///                                                                     
      (Intercept@1 Slope@5 c_drkdays06   -> bmi06) ///                                                                     
      (Intercept@1 Slope@6 c_drkdays07   -> bmi07) ///                                                                     
      (Intercept@1 Slope@7 c_drkdays08   -> bmi08) ///
      (Intercept@1 Slope@8 c_drkdays09   -> bmi09) ///
      (Intercept Slope <- male c_deswgt _cons)     ///
      if bmi01~=.|bmi02~=.|bmi03~=.|bmi05~=.|bmi06~=.|  ///
      bmi07~=.|bmi08~=.|bmi09~=.,  ///         
      var(e.Intercept*e.Slope)  ///
      method(mlmv) noconstant    ///
      var(e.bmi01@v e.bmi02@v e.bmi03@v e.bmi05@v		///
      e.bmi06@v e.bmi07@v e.bmi08@v e.bmi09@v)
estat eqgof
estat gof, stats(all)
estat mindices 
/* This model cannot be fit in Small Stata */
sem   (Intercept@1 Slope@0 c_drkdays01   -> bmi01) ///
      (Intercept@1 Slope@1 c_drkdays02   -> bmi02) /// 
      (Intercept@1 Slope@2 c_drkdays03   -> bmi03) ///                                                                     
      (Intercept@1 Slope@4 c_drkdays05   -> bmi05) ///                                                                     
      (Intercept@1 Slope@5 c_drkdays06   -> bmi06) ///                                                                     
      (Intercept@1 Slope@6 c_drkdays07   -> bmi07) ///                                                                     
      (Intercept@1 Slope@7 c_drkdays08   -> bmi08) ///
      (Intercept@1 Slope@8 c_drkdays09   -> bmi09) ///
      (Intercept Slope <- male c_deswgt _cons)           ///
      if bmi01~=.|bmi02~=.|bmi03~=.|bmi05~=.|bmi06~=.|  ///
      bmi07~=.|bmi08~=.|bmi09~=.,  ///         
      cov(e.Intercept*e.Slope)  ///
      method(mlmv) noconstant 
estat eqgof
estat gof, stats(all)
estat mindices 


