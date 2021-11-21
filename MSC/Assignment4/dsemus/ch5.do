use multgrp_cfa, clear
sem (Depress -> x1 x2 x3)			///
   	(Gov_Resp -> x4 x9 x10 x12),	///
   	standardized group(female)		/// The grouping variable is female 
	ginvariant(all)					//  All parameters constrained equal
estat gof, stats(all)

sem (Depress -> x1 x2 x3) 			///
	(Gov_Resp -> x4 x9 x10 x12),		/// 
	 group(female) ginvariant(none)		/// no constraints on groups
	 mean(Depress@0 Gov_Resp@0)	//  means not estimated, fixed at zero
estat gof, stats(all)

display chi2tail(22, 74.319)

sem (Depress -> x1 x2 x3) 		///
	(Gov_Resp -> x4 x9 x10 x12), 	/// 
	 group(female) ginvariant(mcoef)	/// Invariant loadings
      mean(Depress@0 Gov_Resp@0)		//  Means not estimated, fixed at zero
estat gof, stats(all)

sem (Depress -> x1 x2 x3)	///
	 (Gov_Resp -> x4 x9 x10 x12), 		/// 
	 group(female) ginvariant(none)	/// no constraints on groups
	 mean(Depress@0 Gov_Resp@0)		//  means not estimated, fixed at zero
estimates store form			//  Store estimates for first model
sem (Depress -> x1 x2 x3)	///
	 (Gov_Resp -> x4 x9 x10 x12), 		/// 
    group(female) ginvariant(mcoef)	/// invariant loadings
    mean(Depress@0 Gov_Resp@0)		//  means not estimated, fixed at zero
estimates store loadings			//  Store estimates for second model
lrtest form loadings			//  Likelihood ratio test

estat ginvariant, showpclass(mcoef)

sem (Depress -> x1 x2 x3)			/// 
	(Gov_Resp -> x4 x9 x10 x12),			/// 
	 group(female) ginvariant(mcoef merrvar)	/// Equal loadings and errors
     mean(Depress@0 Gov_Resp@0)

estat gof, stats(all)


sem (Depress -> x1 x2 x3)		        ///
 	(Gov_Resp -> x4 x9 x10 x12),		  /// 
	group(female) ginvariant(mcoef merrvar cov) /// Equal loadings, var., & covariances 
    mean(Depress@0 Gov_Resp@0)
	
estat gof, stats(all)

tabstat x1 x2 x3 x4 x9 x10 x12 if e(sample), statistics(mean) by(female)

sem(Depress -> x1 x2 x3)			///
     (Gov_Resp -> x4 x9 x10 x12), 			/// 
     group(female) ginvariant(mcoef mcons)	/// equal loadings & coefficients
     mean(Depress@0 Gov_Resp@0)  // 
estat gof, stats(all)


sem (Depress -> x1 x2 x3)			///
	 (Gov_Resp -> x4 x9 x10 x12), 				/// 
	 group(female) ginvariant(mcoef mcons)	// Equal loading & intercepts 
estat gof, stats(all)


sem(Depress -> x1 x2 x3)		///
	 (Gov_Resp -> x4 x9 x10 x12), 		///  Mean Comparison with
	 group(female) ginvariant(mcons)	//   Same form equivalence
estat gof, stats(all)

sem(Depress -> x1 x2 x3)		///	Mean Comparison with 
	 (Gov_Resp -> x4 x9 x10 x12), 	///  Equal loadings and
	group(female) 					///	Equal errors
	ginvariant(mcoef merrvar mcons)	// 
estat gof, stats(all)

sem (Depress -> x1 x2 x3) 			 ///
	(Gov_Resp -> x4 x9 x10 x12),			 	 /// 
	 group(female) ginvariant(mcoef merrvar) /// Equal loading/errors
      mean(Depress@0 Gov_Resp@0)			 //  Equal means
sem, standardized

sem (Gov_Resp -> x4 x9 x10 x12) 			 ///
	 (Depress -> x1 x2 x3), 			 	 /// 
 group(female) ginvariant(mcoef merrvar) /// Equal loading/errors
 mean(Depress@0 Gov_Resp@0)	 		 /// Equal means
	 cov(Depress*Gov_Resp@a)			 	 //  Equal covariance 
sem, standardized 

use multgrp_path, clear
sem (bk <- qual tcr)				///
      (b1 <- qual tcr bk), 			///
      group(grp) cov(tcr*qual)
estat ggof	

estat ginvariant

sem (0: bk <- qual tcr@a1 )			///
      (1: bk <- qual tcr@a1 )			///
      (0: b1 <- qual@b1 tcr@c1 bk@d1) 	///
      (1: b1 <- qual@b1 tcr@c1 bk@d1),	///
      group(grp) cov(tcr*qual)

estat eqgof

/* The command in the book should read 

sem, standardized

there is a bug that will be fixed.  For now to get the 
proper output, type */

sem, standardized showginvariant


use http://www.stata-press.com/data/r12/sem_2fmmby, clear
ssd describe
notes

sem (Peerrel -> peerrel1 peerrel2 peerrel3 peerrel4)	/// Measurement
    (Appear -> appear1 appear2 appear3 appear4)		/// Measurement
    (Physical -> phyab1 phyab2 phyab3 phyab4)		/// Measurement
    (Appear Physical -> Peerrel), group(grade)		//  Structural

estat gof, stats(all)
estat ggof
estat eqgof

sem, standardized

sem (Peerrel -> peerrel1 peerrel2 peerrel3 peerrel4)	///
    (Appear -> appear1 appear2 appear3 appear4)		///
    (Physical -> phyab1 phyab2 phyab3 phyab4)		///
    (Appear@a1 Physical@b1 -> Peerrel), group(grade)	//  a1/b1 constraints		  
estat gof, stats(all) 
estat ggof
estat eqgof

estat ginvariant



