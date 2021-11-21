* Chapter 3 Full SEM Model
* ch3semmodel.do
use http://www.stata-press.com/data/r12/sem_sm2.dta, clear
sem 										/// 
	(anomia67 pwless67 <- Alien67) 			/// measure Alien67
	(anomia71 pwless71 <- Alien71)			/// measure Alien71
	(SES66 -> educ66 occstat66) 				/// measurement piece
	(Alien67 <- SES66) 						/// structural piece
 	(Alien71 <- Alien67 SES66), 				/// structural piece
	method(ml) standardize					//  Options
estat eqgof							// R-squares
estat gof, stats(all)  // Goodness of Fit
estat teffects							// Direct, Indirect, Total
estat mindices	
* adding correlated error terms
sem			 										/// 
	(anomia67 pwless67 <- Alien67) 					/// measure Alien67
	(anomia71 pwless71 <- Alien71) 					/// measure Alien71
	(SES66 -> educ66 occstat66) 						/// measurement piece
	(Alien67 <- SES66) 								/// structural piece
 	(Alien71 <- Alien67 SES66), 						/// structural piece
	cov(e.anomia67*e.anomia71)						/// correlated error
	cov(e.pwless67*e.pwless71)						/// correlated error
	method(ml) standardized							//  options
estat eqgof
estat gof, stats(all)
estat teffects, nodirect standardize
estat mindices

sem ///
  (anomia67 pwless67 <- Alien67) /// measurement piece
  (anomia71 pwless71 <- Alien71) /// measurement piece
  (Alien67 <- SES66) /// structural piece
  (Alien71 <- Alien67 SES66) /// structural piece
  ( SES66 -> educ66 occstat66) // measurement piece

* Testing for equality of coefficients using unstandardized variables

use http://www.stata-press.com/data/r12/sem_sm2.dta, clear

sem			 							/// 
	(anomia67 pwless67 <- Alien67) 		/// measure Alien67
	(anomia71 pwless71 <- Alien71) 		/// measure Alien71
	(SES66 -> educ66 occstat66) 			/// measurement piece
	(Alien67 <- SES66) 					/// structural piece
 	(Alien71 <- Alien67 SES66), 			/// structural piece
	cov(e.anomia67*e.anomia71)			/// correlated error
	cov(e.pwless67*e.pwless71)			/// correlated error
	method(ml)  						//  options NO standardize option

estat gof, stats(chi2 rmsea indices)						

sem, coeflegend
test (_b[pwless67:Alien67]==_b[pwless71:Alien71])
* Estimating model with equality constraints imposed
* tau equivalent model
sem			 							/// 
	(anomia67 pwless67@a1 <- Alien67) 	/// measure Alien67
	(anomia71 pwless71@a1 <- Alien71) 	/// measure Alien71
	(SES66 -> educ66 occstat66) 			/// measurement piece
	(Alien67 <- SES66) 					/// structural piece
 	(Alien71 <- Alien67 SES66), 			/// structural piece
	cov(e.anomia67*e.anomia71)			/// correlated error
	cov(e.pwless67*e.pwless71)			/// correlated error
	method(ml)							//  options NO standardize
estat gof, stats(chi2 rmsea indices)
*parallel model
sem			 							/// 
	(anomia67 pwless67@a1 <- Alien67) 	/// measure Alien67
	(anomia71 pwless71@a1 <- Alien71) 	/// measure Alien71
	(SES66 -> educ66 occstat66) 			/// measurement piece
	(Alien67 <- SES66) 					/// structural piece
 	(Alien71 <- Alien67 SES66), 			/// structural piece
	cov(e.anomia67*e.anomia71)			/// correlated error
	cov(e.pwless67*e.pwless71)			/// correlated error
	method(ml)							//  options NO standardize
estat gof, stats(chi2 rmsea indices)
estat scoretests, min(0)

use http://www.stata-press.com/data/r12/sem_sm2.dta, clear
sem                                           /// 
        (Alien67 -> anomia67 pwless67@a1)       /// Measure Alien67, note label @a1
        (Alien71 -> anomia71 pwless71@a1)       /// Measure Alien71, note label @a1
        (SES66 -> educ66 occstat66)               /// Measure SES 66
        (Alien67 <- SES66)                      /// Structural piece
        (Alien71 <- Alien67 SES66),             /// Structural piece
        cov(e.anomia67*e.anomia71)              /// Correlated error
        cov(e.pwless67*e.pwless71)              // Correlated error

estat gof, stats(chi2 rmsea indices)
* constructed variables

display "p = " chi2tail(1,0.88)

sem                                                                   ///
        (Alien67 -> anomia67 pwless67)  /// measure Alien67
        (Alien71 -> anomia71 pwless71)  /// measure Alien71
        (SES66 -> educ66 occstat66)               /// measurement piece
        (Alien67 <- SES66)                              /// structural piece
        (Alien71 <- Alien67 SES66),             /// structural piece
        cov(e.anomia67*e.anomia71)              /// correlated error
        cov(e.pwless67*e.pwless71)              //  correlated error
estimates store level1
estat gof, stats(chi2 rmsea indices)


sem                                                                   ///
        (Alien67 -> anomia67 pwless67@a1)       /// measure Alien67
        (Alien71 -> anomia71 pwless71@a1)       /// measure Alien71
        (SES66 -> educ66 occstat66)               /// measurement piece
        (Alien67 <- SES66)                              /// structural piece
        (Alien71 <- Alien67 SES66),             /// structural piece
        cov(e.anomia67*e.anomia71)              /// correlated error
        cov(e.pwless67*e.pwless71)              // correlated error
estimates store level3 
estat gof, stats(chi2 rmsea indices)
lrtest level1 level3

sem                                                     ///
       (Alien67 -> anomia67 pwless67@a1)  /// Measure Alien67
       (Alien71 -> anomia71 pwless71@a1)  /// Measure Alien71
       (SES66 <- educ66@1 occstat66)      /// Formative indicators
       (Alien67 <- SES66)                 /// Structural piece
       (Alien71 <- Alien67 SES66),        /// Structural piece
       var(e.SES66@0)                     /// Fix error variance for SES66
       cov(e.anomia67*e.anomia71)         /// Correlated error
       cov(e.pwless67*e.pwless71)         /// Correlated error
       standardized

sem  									  ///
       (anomia67 pwless67@a1 <- Alien67)  /// measure Alien67
       (anomia71 pwless71@a1 <- Alien71)  /// measure Alien71
       (SES66 <- educ66@1 occstat66)          /// measurement piece for SES
       (Alien67 <- SES66)                   /// structural piece
       (Alien71 <- Alien67 SES66),          /// structural piece
       var(e.SES66@0)                       /// fix error variance for SES
       cov(e.anomia67*e.anomia71)         /// correlated error
       cov(e.pwless67*e.pwless71)         /// correlated error
       method(ml) standardized
estat eqgof
estat gof, stats(all)
estat teffects, standardize
estat mindices




