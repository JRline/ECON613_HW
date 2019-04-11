/******************************************************************************
Project:	ECON 613 Stata Short Tutorial
Created by: S. Jin
Created:	2019/04/11
Updated: 	
Notes:		This .do file is intended only to get you started with using 
            Stata to complete the last homework.
			There are great resources available for free if you want to
			learn other vital Stata skills not directly related to this
			class. For example, https://stats.idre.ucla.edu/stata/ .
******************************************************************************/



******************************************
* Options and miscellaneity *
******************************************
clear all
set more off, perm
set scrollbufsize 2000000

* Install packages, if you need them.
*ssc install maptile

* Set working directory.
cd "/Users/shijiejin/Documents/Duke - 04/2 - Modibo/0411"
pwd

* Keep logs.
capture log close
* log using "Tutorial_0411.txt", replace





******************************************
* Panacea *
******************************************

* Please take advantage of the 'help' command / the Stata manual,
* and, of course, Google.





******************************************
* Data Creation and Simple Regressions*
******************************************
set seed 613

* Generate some RVs
set obs 1000
generate X1 = runiform(1,3)
gen X2 = rgamma(3,2)
gen eps = rnormal(0,2)
gen Y = X1 + 0.5*X2 + eps



* Basic Summary Stats
su Y
browse Y


* Regression?
reg Y X1 X2
* Store the results and make a table.
eststo silly
esttab silly, se(3) title(Title is Silly)



* Get the dummies: Note a new function for generating vars.
egen Y_mean = mean(Y)
gen Y_dum = 0
replace Y_dum = 1 if Y>Y_mean
* Or...
gen Y_dum2 = (Y>Y_mean)

tab Y_dum



* Now you can do logit, probit, margins, etc.
probit Y_dum X1 X2
margins, dydx(*)
*margins, dydx(*) atmeans



* What about multinomial choices?



* Three types of loops, if you need them.
* 1/3: forvalues
* Define some locals, and note the punctuation rule when calling them.
local counter = 0
local N = _N
forval i = 1 / `N' {
	local counter = `counter'+1
}
display `counter'



* 2/3: foreach
foreach var in X1 X2 {
	rename `var' `var'_cool // from var to var_cool
}
bro



* 3/3: while
local counter = 1
local N = _N
while (`counter' < `N') {
	local counter = `counter'+1
}
dis `counter'





******************************************
* Data in Other Formats *
******************************************

* Import Data
//import excel using "Data/Data_Assignments/Census-2010-DP1.xlsx", firstrow
//insheet using "Koop-Tobias.csv", clear

* Basic summary stats
su educ
su educ, de





******************************************
* Panels! *
******************************************
* Call in a simple panel from the system directory.
* How is this panel different from the ones we usually see?
sysuse xtline1, clear

* Declare the panel identifiers - otherwise it won't work.
xtset person day
bysort person: gen t = _n
xtdes



* Let's make it unbalanced.
drop if calories>3650 & calories<3750 | calories>3800 & calories<3850 | calories > 4050



* Observe!
xtdes



* Other potentially useful commands include xtsum, xttab, xttrans, etc.



* Count number of obs. for each individual.
bysort person: gen count = _N
bysort person: gen obs = _n



* Lagging and Forwarding
gen calories_L = L.calories // one period lag
gen calories_L2 = L2.calories // two period lag
gen calories_F = F.calories
gen calories_F2 = F2.calories



* Order the vars to more conveniently examine the new vars.
aorder
order person t



* Generate a outcome variable.
generate X1 = runiform(1,3)
generate X2 = runiform(0,1)
gen eps = rgamma(3,2)
gen obesity = calories + 500*X1 + 300*X2 + 100*eps



* Declare a global var to save some efforts.
* Note the punctuation rule using the dollar sign in the next block.
global xvars "calories X1 X2"



* Regression - 1st sets
reg obesity $xvars
est sto OLS
xtreg obesity $xvars
est sto Panel_OLS



* Regression - 2nd sets
xtreg obesity $xvars, fe // fixed effect
estimates store Panel_FE 
reg obesity $xvars i.person // assigning each person dummy
estimates store OLS_dum



* Now think about the HW problem (last question).
* What's wrong and how to correct it?



* Output
est tab OLS Panel_OLS Panel_FE OLS_dum, se(3) title(Cool Table)

// Neet to compile table

* Equivalence within sets of results?



* Hausman Test (or you can choose to do an auxiliary reg.)
hausman Panel_FE Panel_OLS, sigmamore
