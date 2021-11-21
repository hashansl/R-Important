* cfa.do
* problems with alpha
clear
* Section 1.2
matrix c =(1., .6, .6, .3, .3, .3 \   ///
           .6, 1., .6, .3, .3, .3 \   ///
           .6, .6, 1., .3, .3, .3 \   ///
           .3, .3, .3, 1., .6, .6 \   ///
           .3, .3, .3, .6, 1., .6 \   ///
           .3, .3, .3, .6, .6, 1. )   
corr2data x1 x2 x3 x4 x5 x6, n(1000) corr(c)
alpha x1-x6
* Section 1.3
use "nlsy97cfa.dta", clear
codebook x1-x10, compact

factor x1-x10, pcf

display .6064^2+.5810^2+.7221^2+.7174^2+.5780^2+.6091^2+.6050^2 +.5994^2+.7330^2+.4543^2

factor x1-x9, pcf

* Section 1.4
alpha x1-x9, item label asis

egen conserve = rowmean(x1-x9)
summarize conserve, detail
histogram conserve, norm freq

* Section 1.5

factor x1-x9, pcf
predict conservf1
histogram conserve, norm freq name(A, replace) ///
xtitle(Mean Conservatism Score) ylabel(0(25)175)
histogram conservf1, norm freq name(B, replace) ///
xtitle(Factor Score on Conservatism) ylabel(0(25)175)
graph combine A B

corr conserve conservf1
* Section 1.6-1.9
sem (Conservative -> x1-x9) 
* bootstrap, reps(1000) seed(111): sem (Conservative -> x1-x9), method(mlmv)

sem (Conservative -> x1-x9)

sem, standardized

estimates store hold
estat gof, stats(all) 

sem x1-x9
estat framework, fitted


estimates restore hold
estat mindices
codebook x3 x4, compact

sem (Conservative -> x1 x3-x7 x9), covariance(e.x3*e.x4) 
sem, standardized
estat gof, stats(all)
estat mindices
sem (Conservative->x1 x3-x7 x9), covariance(e.x3*e.x4) variance(Conservative@1)
* Section 1.10 A Two Factor Model

codebook x11-x13, compact

sem(Depress-> x11-x13)
sem, standardized
estat gof, stats(all)
estat mindices
* Box 1.2
sem (Depress-> x11-x13) 
sem (Depress-> x11-x13) , method(mlmv) allmiss
         
sem (Depress-> x11-x13) if !missing(x1, x2, x3, x4, x5, x6, x7, x8, x9)
sem (Depress-> x11-x13) if x1 != . | x2 != . | x3 !=. | x4 !=. ///
    | x5 !=. | x6 !=. | x7 !=. | x8 !=. | x9 !=., method(mlmv) allmiss


* Section 1.10.2 
sem (Depress-> x11-x13)
sem (Conservative -> x1-x9)
sem (Conservative -> x1 x3-x7 x9), 				///
   covariance(e.x3*e.x4) 

sem (Depress-> x11-x13)    						///
    (Conservative -> x1 x3-x7 x9), 				///
    covariance(e.x3*e.x4)
 
sem, standardized
estat gof, stats(all)
estat mindices


