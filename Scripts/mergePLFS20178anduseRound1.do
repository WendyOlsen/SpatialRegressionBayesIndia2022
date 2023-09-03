* Code written by Jihye Kim and Wendy Olsen 2023
* this code brings in all rounds over the year 2017/8 of India's PLFS. Then
* we create the household weights, merge the data, drop all except Round 1, and save the individual data with
* the household variables attached to it. Be very careful to read all documentation before using the data.

* University of Manchester
clear all
infix using "c:\data\PLFS20172018\13 PLFS(2017-18)\person1.dct", using ("C:\data\PLFS20172018\13 PLFS(2017-18)\FPER_FV.txt")
browse
gen id= quarter+visit+fsu+ hg+ sss+ hhno + psno
gen hhid= fsu+ hg+ sss+ hhno
gen psid = hhid+psno
gen comb_wt = mlt /100 if nss == nsc
replace comb_wt= mlt/200 if nss != nsc
br
tabstat comb_wt, statistics( sum ) format(%14.0f)
sort id hhid psid
save "C:\data\PLFS20172018\13 PLFS(2017-18)\person1.dta", replace
keep psid
save "C:\data\PLFS20172018\13 PLFS(2017-18)\psidonly.dta", replace

clear
infix using "C:\data\PLFS20172018\13 PLFS(2017-18)\person2.dct", using ("C:\data\PLFS20172018\13 PLFS(2017-18)\FPER_RV.txt")
browse
gen id= quarter+visit+fsu+ hg+ sss+ hhno +psno
gen hhid= fsu+ hg+ sss+ hhno
gen psid = hhid+psno
gen comb_wt = mlt /100 if nss == nsc
replace comb_wt= mlt/200 if nss != nsc
*browse
tabstat comb_wt, statistics( sum ) format(%14.0f)
sort id hhid psid
save "C:\data\PLFS20172018\13 PLFS(2017-18)\person2.dta", replace
clear all
use "C:\data\PLFS20172018\13 PLFS(2017-18)\person2.dta", clear


clear
infix using "C:\data\PLFS20172018\13 PLFS(2017-18)\household.dct", using ("C:\data\PLFS20172018\13 PLFS(2017-18)\FHH_FV.txt")
*browse
gen id= quarter+visit+fsu+ hg+ sss+ hhno 
gen hhid= fsu+ hg+ sss+ hhno
sort id hhid
save "C:\data\PLFS20172018\13 PLFS(2017-18)\household1.dta", replace

clear
infix using "C:\data\PLFS20172018\13 PLFS(2017-18)\household.dct", using ("C:\data\PLFS20172018\13 PLFS(2017-18)\FHH_RV.txt")
*browse
gen id= quarter+visit+fsu+ hg+ sss+ hhno 
gen hhid= fsu+ hg+ sss+ hhno
sort id hhid
save "C:\data\PLFS20172018\13 PLFS(2017-18)\household2.dta", replace

*first visit only data. Get persons from Round 1, add their household round 1 variables.
clear all
use "C:\data\PLFS20172018\13 PLFS(2017-18)\person1.dta", clear
merge m:1 hhid using "C:\data\PLFS20172018\13 PLFS(2017-18)\household1.dta"
codebook psid hhid
save "C:\data\PLFS20172018\13 PLFS(2017-18)\plfs_2018.dta", replace

clear all 
use "C:\data\PLFS20172018\13 PLFS(2017-18)\plfs_2018.dta", clear
destring *, replace
gen totalhours1_1= hours_1st_act1 if (status_1st_act1>=11 & status_1st_act1<=51)
gen totalhours2_1= hours_2nd_act1 if (status_2nd_act1>=11 & status_2nd_act1<=51)
gen totalhours3_1= hours_3th_act1 if (status_3th_act1>=11 & status_3th_act1<=51)
gen totalhours4_1= hours_4th_act1 if (status_4th_act1>=11 & status_4th_act1<=51)
gen totalhours5_1= hours_5th_act1 if (status_5th_act1>=11 & status_5th_act1<=51)
gen totalhours6_1= hours_6th_act1 if (status_6th_act1>=11 & status_6th_act1<=51)
gen totalhours7_1= hours_7th_act2 if (status_7th_act1>=11 & status_7th_act1<=51)
gen totalhours1_2= hours_1st_act2 if (status_1st_act1>=11 & status_1st_act1<=51)
gen totalhours2_2= hours_2nd_act2 if (status_2nd_act1>=11 & status_2nd_act1<=51)
gen totalhours3_2= hours_3th_act2 if (status_3th_act1>=11 & status_3th_act1<=51)
gen totalhours4_2= hours_4th_act2 if (status_4th_act1>=11 & status_4th_act1<=51)
gen totalhours5_2= hours_5th_act2 if (status_5th_act1>=11 & status_5th_act1<=51)
gen totalhours6_2= hours_6th_act2 if (status_6th_act1>=11 & status_6th_act1<=51)
gen totalhours7_2= hours_7th_act2 if (status_7th_act1>=11 & status_7th_act1<=51)

replace totalhours1_1=0 if totalhours1_1==.
replace totalhours2_1=0 if totalhours2_1==.
replace totalhours3_1=0 if totalhours3_1==.
replace totalhours4_1=0 if totalhours4_1==.
replace totalhours5_1=0 if totalhours5_1==.
replace totalhours6_1=0 if totalhours6_1==.
replace totalhours7_1=0 if totalhours7_1==.
replace totalhours1_2=0 if totalhours1_2==.
replace totalhours2_2=0 if totalhours2_2==.
replace totalhours3_2=0 if totalhours3_2==.
replace totalhours4_2=0 if totalhours4_2==.
replace totalhours5_2=0 if totalhours5_2==.
replace totalhours6_2=0 if totalhours6_2==.
replace totalhours7_2=0 if totalhours7_2==.

gen totalhours=totalhours1_1+totalhours2_1+totalhours3_1+totalhours4_1+totalhours5_1+totalhours6_1+totalhours7_1 ///
+totalhours1_2+totalhours2_2+totalhours3_2+totalhours4_2+totalhours5_2+totalhours6_2+totalhours7_2 

gen rural=0
replace rural=1 if sector==1
tab rural
gen female=0
replace female =1 if sex==2
tab female

hist hhsize
gen mpce = expenditure / hhsize
hist mpce, bins(40)
hist mpce if mpce <2000, bins(40)
hist mpce if mpce >5000, bins(40)
gen logRsincpc=ln(mpce)
hist logRsincpc if mpce <2000, bins(40)
hist logRsincpc if mpce >5000, bins(40)

hist logRsincpc , bins(40)
tab  status_principal female

gen narrowwork=0
replace narrowwork=1 if  status_principal ==31 |  status_principal  == 51  |status_principal==  41 | status_principal==12
gen medwork=0
replace medwork=1 if  status_principal ==31 |  status_principal  == 51  |status_principal==  41  |  status_principal  == 12  ///
| status_principal ==11 |  status_principal== 21

* NOTE:  Unemployed is status_principal == 81  

gen widework=0
replace widework=1 if   status_principal ==31 |  status_principal  == 51  |status_principal==  41  |  status_principal  == 12 ///
| status_principal ==11  |status_principal== 21 ///
  |  status_principal  == 93  
*NOTE: status_principal == 81 omitted above.
* ADD TO THIS SUBSID IFF THE PERSON IS NOT OTHERWISE EMPLOYED.
replace widework=1 if widework==0 & status_sub==93


save "C:\data\PLFS20172018\13 PLFS(2017-18)\plfs_2018.dta", replace
*put it in nice place and put a nicer version in for use in R.
save "C:\data\PLFS20172018\plfs_2018.dta", replace
saveold "C:\data\PLFS20172018\plfs_2018version13.dta", replace version(13)

*hist men and women age 15
sum totalhours if sex==1 & age>14  & totalhours!=0
sum totalhours if sex==2 & age>14  & totalhours!=0
hist totalhours if sex==1 & age>14 & totalhours!=0
hist totalhours if sex==2 & age>14 & totalhours!=0

*hist children age 5-15
sum totalhours if age>4 & age <16 & totalhours!=0
hist totalhours if age>4 & age <16 & totalhours!=0

*hist children age 5-17
sum totalhours if age>4 & age <18 & totalhours!=0
hist totalhours if age>4 & age <18 & totalhours!=0

* R code - for use elsewhere - ggplot(data, aes(totalhours, weight=comb_wt)) + geom_histogram()



                * * the below would be used if wanting all Rounds of the panel.
				
*clear all      
*use "D:\PLFS(2017-18)\household1.dta", clear
*merge 1:1 id using "D:\PLFS(2017-18)\household2.dta"
*gen negativeexpenditure=-(expenditure)
*sort hhid quarter visit
*duplicates drop hhid, force
*drop _merge
*save "D:\PLFS(2017-18)\household.dta", replace
*merge m:1 psid using "D:\PLFS(2017-18)\psidonly.dta"
*drop if (_merge==1|_merge==2)
*drop _merge
*save "D:\PLFS(2017-18)\person2.1.dta", replace
*codebook psid hhid

