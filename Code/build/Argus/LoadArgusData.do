/*
This file loads the Argus data and converts it to .dta
*/


clear all
set more off
capture log close

* Set local git directory and local dropbox directory
*
* Calling the path file works only if the working directory is nested in the repo
* This will be the case when the file is called via any scripts in the repo.
* Otherwise you must cd to at least the home of the repository in Stata before running.
pathutil split "`c(pwd)'"
while "`s(filename)'" != "JonesAct" && "`s(filename)'" != "jonesact" {
  cd ..
  pathutil split "`c(pwd)'"
}

do "globals.do"

// Set input and output directories
global rawdir = "$dropbox/rawdata/orig/Argus"
global outdir = "$dropbox/intdata/Argus"
global codedir = "$repodir/code/build/Argus"



// Load dirty freight data
clear all
import excel "$rawdir/argus dirty freight historical_2018-2019.xls", /*
	*/ sheet("Price history") cellrange(A4)
rename A Date
* Keep USD/tonne rates for 70kt shipments to East Coast Canada and UKC (Rotterdam)
rename B Canada
drop C-F
rename G UKC
* Add dirty vs clean variable
gen DC = "D"
* Save intermediate data file
order DC Date
sort DC Date
tempfile temp_dirty
save "`temp_dirty'"



// Load clean freight data. Comes in two separate files
clear all
import excel "$rawdir/argus clean freight historical_2018-2019_1.xls", /*
	*/ sheet("Price history") cellrange(A4)
rename A Date
* Keep USD/tonne rates for 38kt shipments to East Coast Canada and DR
drop B
rename C Canada
drop D-F
rename G DR
drop H-I
* Add dirty vs clean variable
gen DC = "C"
* Save intermediate data file
order DC Date
sort DC Date
tempfile temp_clean
save "`temp_clean'"

* Now bring in second file
clear all
import excel "$rawdir/argus clean freight historical_2018-2019_2.xls", /*
	*/ sheet("Price history") cellrange(A4)
rename A Date
* Keep USD/tonne rates for 38kt shipments to Argentina/Uruguay, east coast Mexico, and Las Minas
rename B Argentina
drop C
rename D Mexico
drop E
rename F LasMinas
* Add dirty vs clean variable
gen DC = "C"
* Merge with first file
order DC Date
sort DC Date
merge 1:1 DC Date using "`temp_clean'"
drop _merge



// Append with dirty rates and save intermediate data file
append using "`temp_dirty'"
sort DC Date
saveold "$outdir/ArgusData.dta", version(17) replace

