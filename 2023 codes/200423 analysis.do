/////////////////////////////////////////////////////////////////////////////////////////
//
//                                    ANALYSIS
//                                 ==================
/////////////////////////////////////////////////////////////////////////////////////////

//

// USE THESE ROUTES TO WORK WITH THE DATA
//____________________________________________________________________________________________________
//
global location_data "C:\Users\edika\Desktop\GITHUB\Dominance training\Coalescing-Training\2023 data"
global location_cleaned_data "C:\Users\edika\Desktop\GITHUB\Dominance training\Coalescing-Training\2023 cleaned" 
global location_output "C:\Users\edika\Desktop\GITHUB\Dominance training\Coalescing-Training\2023 output" 
global location_code "C:\Users\edika\Desktop\GITHUB\Dominance training\Coalescing-Training\2023 codes" 
//___________________________________________________________________________________________________



cd "$location_cleaned_data"

use "Data_experiment_cleaned_IP_masked", clear


// violation of SD

gen violation_SD=1 if ///
response=="gamble0" & ///
(coalesced_gamble=="G_minus.jpeg" | ///
 coalesced_gamble=="F_minus.jpeg" | ///
 coalesced_gamble=="FS_minus.jpeg" | ///
 coalesced_gamble=="GS_minus.jpeg")

 
 
  replace violation_SD=1 if ///
response=="gamble1" & ///
(coalesced_gamble1=="G_minus.jpeg" | ///
 coalesced_gamble1=="F_minus.jpeg" | ///
 coalesced_gamble1=="FS_minus.jpeg" | ///
 coalesced_gamble1=="GS_minus.jpeg")
 
 
replace violation=0 if missing(vio)

tab coalesced_gamble response if violation==0

tab coalesced_gamble response if violation==1

// violation of SD in trial 1
gen violation_SD_trial1=1 if order==1 & viola==1 
bysort sub (order): egen violation_SD_trial1_=sum(violation_SD_trial1)
drop violation_SD_trial1
rename violation_SD_trial1 violation_SD_trial1



// violation of SD in training

gen violation_SD_training=1 if order==2 & violation_SD==1 
bysort sub (order): egen violation_SD_training_=sum(violation_SD_training)
drop violation_SD_training
rename violation_SD_training violation_SD_training



bysort condition order: sum violation_SD

// We focus on studing violations of SD during trials 1 and 3 (trial 2 corresponded to the training)

drop if order==2

// The following lines of code create variables with labels that are easy to interpret!

codebook sub_no //1309 unique values (2180 obs)
codebook any
// any_training:  
//		0  Choice w/o training
//		1  Choice w/ training

codebook condition

/*We have three conditions:
(A) Coalesced - Identical	1 trial G- vs G+ → Instruction G- vs G+ → 1 trial G- vs G+
(B) Coalesced - Different	1 trial F- vs F+ → Instruction F- vs F+ → 1 trial G- vs G+
(C) Transparent	1 trial GS- vs GS+
*/


tab gamble_played_condition
replace gamble_played_condition="(Coalesced - Identical) G+ vs G-" if gamble_played_condition=="(A) G+ vs G-"
replace gamble_played_condition="(Coalesced - Different) F+ vs F-" if gamble_played_condition=="(B) F+ vs F-"
replace gamble_played_condition="(Coalesced - Different) G+ vs G-" if gamble_played_condition=="(B) G+ vs G-"
replace gamble_played_condition="(Transparent) GS+ vs GS-" if gamble_played_condition=="(C) GS+ vs GS-"

bysort any gamble_played_condition: sum violation_SD 

display 435+436+438 //= 1309 obs before training 

// counting obs
/*
-> any_training = Choice w/o training, gamble_played_condition = (Coalesced - Different) F+ vs F-
435 
-> any_training = Choice w/o training, gamble_played_condition = (Coalesced - Identical) G+ vs G-
436
-> any_training = Choice w/o training, gamble_played_condition = (Transparent) GS+ vs GS-
438 
-> any_training = Choice w/ training, gamble_played_condition = (Coalesced - Different) G+ vs G-
435 
-> any_training = Choice w/ training, gamble_played_condition = (Coalesced - Identical) G+ vs G-
436 

*/


// The variable gamble_cond_n is identical to gamble_played_condition, but the former is a numerical variable, while the latter is a string variable

bysort gamble_cond_n: sum violation_SD if any==0

gen satis=1-violation_SD

tab gamble_cond_n any if any==0, nolabel
tab gamble_cond_n

capture drop new_condition_detailed
gen new_condition_detailed="C, GS+ vs GS-, no training" if any==0 & gamble_cond_n==1
replace new_condition_detailed="B, F+ vs F-, before training" if any==0 & gamble_cond_n==2
replace new_condition_detailed="B, G+ vs G-, after training" if any==1 & gamble_cond_n==3
replace new_condition_detailed="A, G+ vs G-, before training" if any==0 & gamble_cond_n==4
replace new_condition_detailed="A, G+ vs G-, after training" if any==1 & gamble_cond_n==4

tab gamble_con new_condition, missing
tab  new_condition gamble_con, missing

//proportion of satisfaction of dominance over 
proportion satis if any==0, over(gamble_cond_n, nolabel) 
proportion satis if any==1, over(gamble_cond_n, nolabel) 

encode new_condition_detailed, gen(n_condition)
label list
tab n_condition
tab n_condition, nolabel
tab gamble_con n_condition, missing


// satisfaction of SD before training per gamble
proportion satis if n_condition==5 | n_condition==2 | n_condition==3 , over(n_condition, nolabel) 
estimates store result_before
// satisfaction of SD after training per gamble
proportion satis if  n_condition==1 | n_condition==4, over(n_condition, nolabel) 
estimates store result_after


//_________________________________________________________
//
//PLOT: Proportion of Violations of Stochastic Dominance (before deleting outliers)
//_________________________________________________________

cd "$location_output"

coefplot result_before result_after, xtitle(Proportion of Violations of Stochastic Dominance) ///
xlabel(0 (.1) .8)  grid(none) ///
      p1(label(First choice, without training) pstyle(p13))       ///
    p2(label(Second choice, after training) pstyle(p14) ) ///
	scheme(plotplainblind) ///
	coeflabels( 5=   "GS+ vs GS-, no training"  ///
           3= "F+ vs F-, before training" ///
           4= "G+ vs G-, after training" ///
           2= "G+ vs G-, before training" ///
		   1= "G+ vs G-, after training" ) ///
		   order(5 2 1 3 4) ///
	headings( 5=  "{bf:Transparent Choice}" ///
	           3= "{bf:Coalesced - Different Choices}"  ///
           2= "{bf:Coalesced - Identical Choices}" ) ///
		   legend( position(6) col(2))

graph export "Figure_Violations_SD_before_dropping_outliers.png", replace


// We create "delete" equal to 1 for duplicated IDs as well as for outliers in reaction times

bysort condition: sum RT, detail 

// we exclude obs below/above the 5 and 95 percentiles on each condition

tab dup omit // omit is equal to 1 for duplpicated IDs


gen delete=1 if omit==1

replace delete=1 if (RT<2272 | RT> 34354) & condition=="A"
replace delete=1 if (RT<2626 | RT> 35018) & condition=="B"
replace delete=1 if (RT<4394  | RT> 31740) & condition=="C" 


bysort sub_no: egen deletes=sum(delete)
drop delete
drop if deletes>0
drop deletes





// counting subjects
bysort any gamble_played_condition: sum violation_SD 

display 344 + 343 + 385 //subjects before training 
codebook sub_no //1072 unique values


// Satisfaction of SD after deleting outliers

tab gamble_cond_n
proportion satis if any==0, over(gamble_cond_n, nolabel) 
proportion satis if any==1, over(gamble_cond_n, nolabel) 

// satisfaction of SD before training per gamble
proportion satis if n_condition==5 | n_condition==2 | n_condition==3 , over(n_condition, nolabel) 
estimates store result_before
// satisfaction of SD after training per gamble
proportion satis if  n_condition==1 | n_condition==4, over(n_condition, nolabel) 
estimates store result_after


//_________________________________________________________
//
//PLOT: Proportion of Violations of Stochastic Dominance (after deleting outliers)
// The results remain unchanged from the previous plot, which was before removing outliers
//_________________________________________________________


cd "$location_output"

coefplot result_before result_after, xtitle(Proportion of Violations of Stochastic Dominance) ///
xlabel(0 (.1) .8)  grid(none) ///
      p1(label(First choice, without training) pstyle(p13))       ///
    p2(label(Second choice, after training) pstyle(p14) ) ///
	scheme(plotplainblind) ///
	coeflabels( 5=   "GS+ vs GS-, no training"  ///
           3= "F+ vs F-, before training" ///
           4= "G+ vs G-, after training" ///
           2= "G+ vs G-, before training" ///
		   1= "G+ vs G-, after training" ) ///
		   order(5 2 1 3 4) ///
	headings( 5=  "{bf:Transparent Choice}" ///
	           3= "{bf:Coalesced - Different Choices}"  ///
           2= "{bf:Coalesced - Identical Choices}" ) ///
		   legend( position(6) col(2)) 
		   
graph export "Figure_Violations_SD.png", replace


//_________________________________________________________
//
// Chi2 test 
// violations of SD before training - comparisons against the control condition (i.e., transparent gambles GS)
//_________________________________________________________

 tabulate violation_SD condition
 tab gamble_played_condition any
 tab gamble_played_d condition if any==0, missing 
 tab gamble_played_d condition if any==1, missing

 tabulate gamble_played_d condition if any==0 & condition!="B", missing 
 tabulate violation_SD condition if any==0 & condition!="B", chi2 
 
 // Pearson chi2(1) = 186.7571   Pr = 0.000
  prtest violation_SD if any==0 & condition!="B", by(condition)

 // Chi-squared test for independence of rows and columns
 // Testing if proportions from two conditions are the same
 //		The proportion of participants in A who violated was 0.68 whereas the proportion from the control group C was only 0.18. 
 //		The difference in proportions is significant, χ²(1, N = 728) = 186.7571, p < 0.001.
 
 
 
  tabulate violation_SD condition if any==0 & condition!="A", chi2 
 
 //  Pearson chi2(1) = 206.1521   Pr = 0.000
 
 prtest violation_SD if any==0 & condition!="A", by(condition)
 //		The proportion of participants in B who violated was 0.71 whereas the proportion from the control group C was only 0.18. 
 //		The difference in proportions is significant, χ²(1, N = 729) = 206.1521 , p < 0.001.

 
//_________________________________________________________
//
// Histogram of the number of times participants split and coalesced gambles’ branches during their training on detection of dominant gambles. 
// The histogram excludes the top 1% percentile of the distribution (frequencies above 19 clicks).
//_________________________________________________________
 
  des toggle*
 sort condition sub_no order
 sum toggle_count if toggle_count>0, detail // max 40. But 19 is 99%
 sum toggle_count if !missing(training_version) & toggle_count>0, d
 hist toggle_count if !missing(training_version) & toggle_count>0 & toggle_count<=19 , discrete ///
 xlabel(6 (1) 19) scheme(plotplainblind) xtitle("Practice (number of clicks)")

 graph export "Figure_toggle_count_during_training.png", replace

// toggle counts per condition and rate of violations per toggle counts 
tab toggle_count_u
tab toggle_count_u n_cond, missing
bysort toggle_count_u:  sum violation_SD 


//_________________________________________________________
//
// Regressions
// =============
// We estimate three regressions in "SDlogit.doc2"
// The lnsig2u in the table is the log of the panel-level variance for each regression
// (lnsig2u estimates are displayed in a separate column next to each regression)
//
// Columns 1 to 4 in "SDlogit.doc2 correspond to estimates presented in Table 3 of the paper
// Columns 5 to 6 in "SDlogit.doc2 correspond to estimates presented in Table A1 of the paper
//_________________________________________________________

 
 global DV violation_SD


tab gamble_played_d condition, nolabel
tab gamble_played_d condition

replace new_condition_detailed="1: B, F+ vs F-, before training" if new_co=="B, F+ vs F-, before training"
replace new_condition_detailed="2: A, G+ vs G-, before training" if new_co=="A, G+ vs G-, before training"
replace new_condition_detailed="3: A, G+ vs G-, after training" if new_co=="A, G+ vs G-, after training"
replace new_condition_detailed="4: B, G+ vs G-, after training" if new_co=="B, G+ vs G-, after training"
replace new_condition_detailed="5: C, GS+ vs GS-, no training" if new_co=="C, GS+ vs GS-, no training"

drop n_cond
encode new_condition, gen(n_cond)



tab n_cond, missing

cd "$location_output"


 xtlogit $DV ///
ib2.n_cond ///
if condition!="C" ///
, re
outreg2 using SDlogit.doc, replace ctitle(RE)   ///
addstat ("ICC",e(rho)) ///
 label

 

xtlogit $DV ///
i.any ///
if condition!="C" ///
, re
estimates store re
outreg2 using SDlogit.doc, append ctitle(RE)   ///
addstat ("ICC",e(rho)) ///
 label

 

 xtlogit $DV ///
ib0.toggle_count_u ///
if condition!="C" ///
, re
outreg2 using SDlogit.doc, append ctitle(RE)  ///
addstat ("ICC",e(rho)) ///
 label

 
 
//Linear models to ease the interpretation of arginal effects

 reg $DV ///
ib2.n_cond ///
if condition!="C" ///
, cluster(sub_no)
outreg2 using SDlm.doc, replace ctitle(LM)   ///
 label

 

reg $DV ///
i.any ///
if condition!="C" ///
,  cluster(sub_no)
estimates store re
outreg2 using SDlm.doc, append ctitle(LM)   ///
 label

 

 reg $DV ///
ib0.toggle_count_u ///
if condition!="C" ///
, cluster(sub_no)
outreg2 using SDlm.doc, append ctitle(LM)  ///
 label
 
 
//_________________________________________________________
//
// Histogram of reaction times 
// 	(1) Across conditions
//  (2) By violation or satisfaction of stochastic dominance (after deleting outliers)
//_________________________________________________________
//
hist RT, scheme(plotplainblind)
tab violation_SD

 bysort gamble_played_condition any violation_SD: sum RT , detail 
 
  hist RT , by( gamble_played_condition any , col(1	) note("") )  ///
  scheme(plotplainblind)
  
    graph export "Histogram_RT_across_conditions.png", replace

  

    hist RT if violation_SD==0,  ///
	by( gamble_played_condition any , col(1) note("")  title({bf:Satisfaction of SD})  )  ///
  scheme(plotplainblind)name(z1, replace)

    hist RT if violation_SD==1, by( gamble_played_condition any , col(1) note("")   title({bf:Violation of SD})   )  ///
  scheme(plotplainblind)name(z2, replace)
  
       graph combine z1 z2 , col(2) ///
title("") ///
 scheme(plotplainblind) name(z12, replace) 
 
  graph export "Histogram_RT_by_violation_or_satisfaction_SD.png", replace

 
  

