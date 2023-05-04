

// USE THESE ROUTES TO WORK WITH THE DATA
//____________________________________________________________________________________________________

//
global location_data "C:\Users\edika\Desktop\GITHUB\Dominance training\Coalescing-Training\2023 data"
global location_cleaned_data "C:\Users\edika\Desktop\GITHUB\Dominance training\Coalescing-Training\2023 cleaned" 
global location_output "C:\Users\edika\Desktop\GITHUB\Dominance training\Coalescing-Training\2023 output" 
global location_code "C:\Users\edika\Desktop\GITHUB\Dominance training\Coalescing-Training\2023 codes" 


//___________________________________________________________________________________________________


/////////////////////////////////////////////////////////////////////////////////////////
//
//                                 CLEANING RAW DATA
//                                 ==================
/////////////////////////////////////////////////////////////////////////////////////////


*The raw data is stored in a file called "MTurk_batch_results_IP_masked.txt". 
*To work with the data, we use the text import wizard from Excel, which will allow you to specify the appropriate settings such as column delimiters. 
*In this case, the columns are delimited by spaces, we use this option to split the data into columns for further analysis.
*The resulting file is then saved as "data_excel_split_IP_masked.csv".

*Below are the codes used to clean the data and create relevant variables before conducting data analysis

cd "$location_data"
clear
import delimited "data_excel_split_IP_masked.csv"
gen order=_n
gen group=0
replace group=1 if v1=="end"
gen s_group=sum(group)
drop group

gen sub_no=v2 if v1=="sub_no:"
destring sub_no, replace


bysort s_group: egen sub_no_=mean(sub_no)
drop sub_no
drop if missing(v2)

*condition

gen condition=v2 if v1=="condition:"
encode condition, gen (condition_d)
bysort s_group: egen condition_=mean(condition_d)
drop condition
label values condition_ condition_d
decode condition_, gen (condition)
drop condition_
drop condition_d


*ip

gen ip=v2 if v1=="ip:"
encode ip, gen (ip_d)
bysort s_group: egen ip_=mean(ip_d)
drop ip
label values ip_ ip_d
decode ip_, gen (ip)
drop ip_
drop ip_d

*verification

gen verification=v2 if v1=="verification:"
encode verification, gen (verification_d)
bysort s_group: egen verification_=mean(verification_d)
drop verification
label values verification_ verification_d
decode verification_, gen (verification)
drop verification_
drop verification_d

bysort sub verification: gen f=1 if _n==1
gen sub_no_2=sum(f)

codebook sub_no_2
des sub*, full
replace sub_no_=sub_no_+1
tab sub_no_ if sub_no_2!=sub_no_

drop f
replace sub_no_=sub_no_2
drop sub_no_2

codebook sub_no_
sum sub_no_

*experimentDuration
gen experimentDuration=v2 if v1=="experimentDuration:"
encode experimentDuration, gen (experimentDuration_d)
bysort s_group: egen experimentDuration_=mean(experimentDuration_d)
drop experimentDuration
label values experimentDuration_ experimentDuration_d
decode experimentDuration_, gen (experimentDuration)
drop experimentDuration_
drop experimentDuration_d

drop order


*trial
keep if v3=="trialid"

tab v2
encode v2, gen(order_trial)
drop v2 v1

rename sub_no sub_no
drop s_group

rename v4 order_of_trial
drop order_trial
drop v3
drop v19

rename v20 togge_count
destring togge, replace
destring order_of, replace

// There are two gambles in the training that participants need to choose: Gamble 0 and Gamble 1.

// "coalesced_gamble" and "bottom gamble" variables refer to the first gamble (Gamble 0). The first variable is in coalesced form, while the second variable is in split form. 

// "coalesced_gamble1" and "bottom gamble1" refer to the second gamble (Gamble 1). The first variable is in coalesced form, while the second variable is in split form. 



rename v6 coalesced_gamble
rename v8 split_gamble

rename v10 coalesced_gamble1
rename v12 split_gamble1


drop v5 v7
drop v9 v11

drop v17
rename v18 RT

rename v16 response
drop v15

rename v14 training
drop v13
destring training, replace

replace split_gamble="" if split_gamble=="null.jpeg"
replace split_gamble1="" if split_gamble1=="null.jpeg"

list coalesced_gamble	split_gamble	coalesced_gamble1	split_gamble1 if ///
training==1

tab coalesced_gamble	split_gamble	 if ///
training==1

*training gamble
gen training_gamble=coalesced_gamble if training==1
replace training_gamble="F_minus vs F_plus" if coalesced_gamble=="F_minus.jpeg"
replace training_gamble="F_plus vs F_minus" if coalesced_gamble=="F_plus.jpeg"
replace training_gamble="G_minus vs G_plus" if coalesced_gamble=="G_minus.jpeg"
replace training_gamble="G_plus vs G_minus" if coalesced_gamble=="G_plus.jpeg"

tab training*
tab split_gamble1 training, missing

replace training_gamble="" if missing(split_gamble)

drop split_g*
drop training
encode training, gen(training_g)

drop training_gamble

sort sub order
ssc install carryforward
bysort sub (order):  carryforward training_g, gen(training_followed)
decode training_fo, gen(training_received)
drop training_foll

bysort sub (order): gen toggle_count=sum(togge_count)
//drop if !missing(training_g)
drop training_g

gen training_version="G" if training_received=="G_plus vs G_minus" | ///
training_received=="G_minus vs G_plus"

replace training_version="F" if training_received=="F_plus vs F_minus" | ///
training_received=="F_minus vs F_plus"

tab training_version, gen (training_)
rename training_1 training_F
rename training_2 training_G
replace training_F=0 if missing(training_F)
replace training_G=0 if missing(training_G)

gen any_training=0
replace any_training=1 if !missing(training_v)

tab toggle_c any



gen gamble_played="G" if ///
coalesced_gamble=="G_minus.jpeg" | coalesced_gamble=="G_plus.jpeg"

replace gamble_played="F" if ///
coalesced_gamble=="F_minus.jpeg" | coalesced_gamble=="F_plus.jpeg"

replace gamble_played="GS" if ///
coalesced_gamble=="GS_minus.jpeg" | coalesced_gamble=="GS_plus.jpeg"

replace gamble_played="FS" if ///
coalesced_gamble=="FS_minus.jpeg" | coalesced_gamble=="FS_plus.jpeg"

tab gamble_played, missing

encode gamble_pla, gen(gamble_played_d)

tab gamble_played_d, gen(gamble_played_)


des gamble_played_*


tab any gamble_played //only some gambles G could have received training before
gen G_and_training=0
replace G_and_training=1 if gamble_played_2==1 & any==1
tab any G_and_trai, missing
label var gamble_played_1 "Gamble played F (no training)"
label var gamble_played_2 "Gamble played G (training/no training)"
label var gamble_played_3 "Gamble played GS (no training)"

label var G_and_tra "Gamble played G (training)"



gen G_and_no_training=1 if gamble_played_2==1 & any==0
replace G_and_no_training=0 if missing(G_and_no_training)

label var G_and_no_tra "Gamble played G (no training)"
tab gamble_played_2 G_and_no_training
tab gamble_played_2 G_and_tra

*RT
destring RT, replace
destring experimentDuration, replace
sum RT, detail //5%  2569   95% 34022
sum experimentD, detail //5% 39463  95% 275589
tab toggle_count
gen odd = mod(c.toggle_c,2)

hist toggle_c
tab toggle_c condition

gen toggle_count2 =toggle_count^2
tab toggle_count2 , missing

tab toggle_count , missing

gen toggle_count_upto10=toggle_count
replace toggle_count_up=10 if toggle_count_u>10
tab toggle_count_upto10

gen training_FG=any_training
replace training_FG=2 if training_F==1

label define FGtraining ///
0 "No training" ///
1 "Training G" ///
2 "Training F"
label values training_FG FGtraining

bysort sub_no (order): gen ip2=ip if _n==1 //first ip
codebook ip2

bysort ip sub_no: gen an_ip_sub=1 if _n==1
bysort ip: egen many_sub=sum(an_ip_sub)


tab many_sub, missing
list sub if many>1
gen duplicate_id=0 //same ip
replace duplicate=1 if many>1
drop ip2 an_ip many_sub

bysort ip sub_no: gen order_sub_ip=1 if _n==1
replace order_sub_ip=0 if missing(order_sub_ip)


bysort ip (sub_no): gen order_sub_ip2=sum(order_sub_ip)
gen omit_ip=0
replace omit_ip=1 if duplicate==1 
drop order_sub_ip*

*labelling key variables
des gamble_played*
label var toggle_count "Practice (Number of clicks)" 
label var toggle_count2 "Practice (Number of clicks) ^ 2" 

label define gamble_played_d ///
           1 "F+ vs F-" ///
           2 "G+ vs G-" ///
           3 "GS+ vs GS-", replace

gen gamble_played_condition=gamble_played
replace gamble_played_condition="(A) G+ vs G-" if condition=="A" & gamble_played=="G"
replace gamble_played_condition="(C) GS+ vs GS-" if condition=="C" & gamble_played=="GS"
replace gamble_played_condition="(B) G+ vs G-" if condition=="B" & gamble_played=="G"
replace gamble_played_condition="(B) F+ vs F-" if condition=="B" & gamble_played=="F"

tab gamble_played_condition condition, missing	
gen gamble_cond_n=4 if gamble_played_condition=="(A) G+ vs G-" 
replace gamble_cond_n=1 if  gamble_played_condition=="(C) GS+ vs GS-" 
replace gamble_cond_n=3 if gamble_played_condition=="(B) G+ vs G-" 
replace gamble_cond_n=2 if  gamble_played_condition=="(B) F+ vs F-" 
	
	
label define gamble_condition ///
4 "(A) G+ vs G-" ///
1 "(C) GS+ vs GS-" ///
3 "(B) G+ vs G-" ///
2 "(B) F+ vs F-", replace 

label values gamble_cond_n gamble_condition
	

tab any
label define any_t ///
1 "Choice w/ training" ///
0 "Choice w/o training", replace
label values any any_t
gen logRT=log(RT)
hist logRT

label var logRT "Log Reaction Time(ms)"    
xtset sub_no
drop togge
cd "$location_cleaned_data"


save "Data_experiment_cleaned_IP_masked.dta", replace

