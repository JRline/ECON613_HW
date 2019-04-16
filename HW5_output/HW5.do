log using "C:\Users\jiere\Dropbox\Spring 2019\ECON 613\ECON613_HW\HW5_output\HW5", replace

clear all
set more off, perm
set scrollbufsize 2000000
cd "C:\Users\jiere\Dropbox\Spring 2019\ECON 613\ECON613_HW\HW5_output"
pwd
set seed 1


********
* HW 2 *
********
* Exercise 1: Data generation
set seed 613
set obs 1000 
gen X1 = runiform(1,3)
gen X2 = rgamma(3,2)
gen X3 = rbinomial(1,0.3)
gen eps = rnormal(2,1)
gen Y = 0.5 + 1.2*X1 - 0.9*X2 + 0.1*100*eps
egen Y_mean = mean(Y)
gen Y_dum = (Y>Y_mean)

* Exercise 2
cor Y X1

reg Y X1 X2 X3
est sto X_Y

* bootstraping
bootstrap, size(1000) reps(49): reg Y X1 X2 X3
est sto boot49

bootstrap, size(1000) reps(499): reg Y X1 X2 X3
est sto boot499

* Exercise 3
* probit model
probit Y_dum X1 X2 X3
est sto probit_

margins, dydx(*)
est sto probit_m

margins, dydx(*) vce(delta)
est sto probit_mdelta

*Bootstrap se option not included for margin

* Exercise 4&5
* logit model
reg Y_dum X1 X2 X3
est sto linear_

logit Y_dum X1 X2 X3
est sto logit_

margins, dydx(*) vce(delta)
est sto logit_mdelta

*Bootstrap se option not included for margin


********
* HW 3 *
********
* merge two data set (these two data set are exported from R)
insheet using "demos.csv",clear
sort hhid
preserve

tempfile tmp
insheet using "choicePrice.csv",clear
sort hhid
save `tmp'

restore
merge hhid using `tmp'
drop hhid v1 _merge
gen id = _n

* Exercise 1
* summary statistics
su ppk_stk  pbb_stk pfl_stk phse_stk pgen_stk pimp_stk pss_tub /*
*/ppk_tub pfl_tub phse_tub
tab choice

* market share by type
gen choice_type = cond(choice == 1|choice == 2|choice == 3|choice == 4|choice ///
== 5|choice == 6,"stk","tub")
tab choice_type

* market share by brand
gen choice_brand = cond(choice == 1|choice == 8,"ppk", /*
*/cond(choice == 2,"pbb",cond(choice == 3|choice == 9,"pfl",cond(choice == 4|choice == 10,"phse",/*
*/cond(choice == 5,"pgen",cond(choice == 6,"pimp",cond(choice == 7,"pss","")))))))
tab choice_brand

* Exercise 2&4
* conditional logit
* reshape from wide to long
rename (ppk_stk  pbb_stk pfl_stk phse_stk pgen_stk pimp_stk pss_tub /*
*/ppk_tub pfl_tub phse_tub)(Price1 Price2 Price3 Price4 Price5 Price6 Price7 /*
*/Price8 Price9 Price10) // first need to rename the price

reshape long Price, i(id)
rename (_j)(alt)
gen d = cond(choice == alt,1,0) //one means choosen, zero means not

asclogit d Price, case(id) alternatives(alt)
est sto c_logit
estat mfx // marginal effect estimation

* Exercise 3&4
* Multinomial logit 
asclogit d, case(id) alternatives(alt) casevar(income)
est sto m_logit
estat mfx 

* Exercise 5
* Mixed logit
asclogit d Price, case(id) alternatives(alt) casevar(income)
est sto mix_logit
estat mfx
gen ll = e(ll)

* Mixed logit of droping one choice
asclogit d Price if alt != 10, case(id) alternatives(alt) casevar(income)
est sto mix_logit_alt
gen ll2 = e(ll)

* use the MTT test provide by Prof.Sidibe
gen MTT = 2*(ll - ll2)
di MTT
hausman mix_logit mix_logit_alt
di r(p)
* we reject the null hypothese and state that IIA is hold.

********
* HW 4 *
********
insheet using "Koop-Tobias.csv", clear

* Exercise 1
* panel data setup
xtset personid timetrnd
xtdes
global yvar logwage
global xvar "educ potexper"

* Exercise 2
xtreg $yvar $xvar, re // random effect
est sto re

* Exercise 3
xtreg $yvar $xvar, be // between estimator
est sto be

xtreg $yvar $xvar, fe // with-in estimator
est sto fe

* first difference
gen educ_D = educ - L.educ
gen educ_L = L.educ
gen potexper_D = potexper - L.potexper
gen logwage_D = logwage - L.logwage

reg logwage_D educ_D potexper_D, noconstant
est sto fd
*NOTE: this only consider the difference of data in consequtive time periods

* Exercise 4
* draw 100 person sample (take unique, select 100 person, then merge their data back)
preserve

tempfile tmp
bysort personid: keep if _n == 1
sample 100,count
sort personid
save `tmp'

restore
merge m:1 personid using `tmp'
keep if _merge == 3
drop _merge

global yvar_ logwage
global xvar_ "educ potexper"

* first get the indiviudal fixed effect
reg $yvar_ $xvar_ i.personid, nocon vce(robust)
est sto idfix

* store the coefficient and only keep individual effects
matrix beta = e(b) 
matrix beta = beta'
matrix beta = beta[3..102,1]

* For part 2, regressing on time invarients, keeping one observation for each individual
bysort personid: keep if _n == 1
svmat double beta, names(individual) // add individual effects in
drop if individual == 0 // drop the reference group, as its individual effect is 0

global xvar_timinv "ability mothered fathered brknhome siblings"
reg individual $xvar_timinv, vce(robust)
est sto idfix_timinv

* HW2 summary
est tab X_Y boot49 boot499, se title(OLS and bootstrape)
est tab linear_ probit_ probit_m /*
*/ probit_mdelta logit_ logit_mdelta /*
*/ , se title(Linear, Probit, Logit)

* HW3 summary
est tab c_logit m_logit mix_logit, se title(Conditional Logit)

* HW4 Summary
est tab re be fe fd, se title(Panel Methods)
est tab idfix idfix_timinv, se title(Individual Fixed effect w/ robust se)

log close
