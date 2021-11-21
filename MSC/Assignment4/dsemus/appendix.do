
use http://www.stata-press.com/data/r12/sem_sm2.dta

ssd describe

clear
 ssd init c1 c2 c3 m1 m2 m3 i1 i2 i3 math
 ssd set observations 500
 ssd  set sd                                    ///
 1.5 1.7 1.6 2.0 1.8 1.8 1.9 1.8 1.7 5.6
 #delimit ;
 ssd set correlations
 1.0 \
 .60 1.0 \
 .50 .55 1.0 \
 .20 .25 .22 1.0 \
 .25 .28 .29 .70 1.0 \
 .22 .25 .24 .60 .60 1.0 \
 .33 .30 .29 .34 .33 .29 1.0\
 .31 .31 .32 .33 .29 .30 .50 1.0\
 .30 .29 .29 .29 .30 .30 .61 .59 1.0\
 .40 .35 .37 .25 .28 .27 .15 .22 .18 1.0 ;
#delimit cr

save ssdmath.dta
clear
use ssdmath
ssd list


sem (ExecFunction -> c1 c2 c3 m1 m2 m3 i1 i2 i3)              ///
        (math<-ExecFunction),                                                   ///
        standardized
estat eqgof
estat gof, stats(all)

sem (C-> c1 c2 c3)                      ///
    (M-> m1 m2 m3)                      ///
    (I-> i1 i2 i3)                      ///
    (math<- C M I),                     ///
    standardized
estat eqgof
estat gof, stats(all)

