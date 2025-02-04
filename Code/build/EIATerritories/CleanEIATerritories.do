

******************** SYNOPSIS ***********************
// Read in the raw annaul data from the EIA on US territories' petroleum 
// product consumption, and clean dataset to a usable format

/*********** BASIC SETUP *********************/
clear all
set more off
capture log close

* Set local git directory and local dropbox directory
* Calling the path file works only if the working directory is nested in the repo
pathutil split "`c(pwd)'"
while "`s(filename)'" != "JonesAct" && "`s(filename)'" != "jonesact" {
	cd ..
	pathutil split "`c(pwd)'"
}
do "globals.do"

* Load dropbox location
global rawdir = "$dropbox/rawdata/orig/EIATerritories"
global outdir = "$dropbox/intdata/EIATerritories"
global texdir = "$repodir/output/tex_numbers"



/*********** BEGIN CODE *********************/

* Import the raw data file
import delimited "$rawdir/INT-Export-07-17-2023_14-02-51.csv", varnames(2) clear

* v2 is the series name. v48 is 2018 and v51 is 2021
keep v2 v48 v49 v51
rename v2 series
rename v48 Q2018
rename v49 Q2019
rename v51 Q2021

* Create row ID and keep only needed rows
gen rowID = _n
gen Terr = ""
order rowID Terr
keep if rowID>=32 & rowID<=47	// keeps Puerto Rico and US Virgin Islands
replace Terr = "PR" if rowID<=40
replace Terr = "VI" if rowID>=41
keep if inlist(rowID,34,35,37,44,45,47)	// keep gasoline, jet fuel, distillate
drop rowID

* Convert data from string to double
destring Q2018 Q2019, replace

* Reshape long
sort Terr series
reshape long Q, i(Terr series) j(Year)

* Clean up and save
rename Q Consumption_mbblperday
rename series prod_category
replace prod_category = "Conventional" if prod_category=="            Motor gasoline (Mb/d)"
replace prod_category = "Jet Fuel" if prod_category=="            Jet fuel (Mb/d)"
replace prod_category = "ULSD" if prod_category=="            Distillate fuel oil (Mb/d)"

sort Terr prod_category Year
export delimited using "$outdir/EIATerritories.csv", replace

* Write avg annual consumption of each fuel to a tex file
drop if Year==2021
bysort Terr prod_category: egen CAnn = mean(Consumption)
drop Year Consumption
duplicates drop
sort prod_category
collapse(sum) CAnn, by(prod_category)
replace CAnn = CAnn * 365 / 1000		// convert to annual
file open PRVI_gas using "$texdir/PRVI_gas.tex", write replace
file write PRVI_gas %8.0f (CAnn[1])
file close PRVI_gas
file open PRVI_jet using "$texdir/PRVI_jet.tex", write replace
file write PRVI_jet %8.0f (CAnn[2])
file close PRVI_jet
file open PRVI_ulsd using "$texdir/PRVI_ulsd.tex", write replace
file write PRVI_ulsd %8.0f (CAnn[3])
file close PRVI_ulsd


exit
