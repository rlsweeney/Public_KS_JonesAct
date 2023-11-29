/*
This file loads the Army Corps data and converts it to .dta
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
global rawdir = "$dropbox/rawdata/orig/ArmyCorps"
global outdir = "$dropbox/intdata/ArmyCorps"
global codedir = "$repodir/code/build/ArmyCorps"



*******************************************
// Load raw army corps data
// 3 files: Tampa, Pt Everglades, Pt Canaveral
// Tampa
clear all
import delimited "$rawdir/5YearCargoReport-Domestic-TAM.csv", /*
	*/ varnames(2)
keep if inlist(code,2100,2211,2221,2330)	// keep crude, gasoline, kerosene, diesel
keep commodity cy2019 cy2018
gen port = "TAM"	
* Save intermediate data file
tempfile temp_tam
save "`temp_tam'"

// Pt. Everglades
clear all
import delimited "$rawdir/5YearCargoReport-Domestic-PortEverglades.csv", /*
	*/ varnames(2)
keep if inlist(code,2100,2211,2221,2330)	// keep crude, gasoline, kerosene, diesel
keep commodity cy2019 cy2018
gen port = "EV"	
* Save intermediate data file
tempfile temp_ev
save "`temp_ev'"

// Pt. Canaveral
clear all
import delimited "$rawdir/5YearCargoReport-Domestic-Canaveral.csv", /*
	*/ varnames(2)
keep if inlist(code,2100,2211,2221,2330)	// keep crude, gasoline, kerosene, diesel
keep commodity cy2019 cy2018
gen port = "CNV"	
* Save intermediate data file
tempfile temp_cnv
save "`temp_cnv'"

// Jacksonville
clear all
import delimited "$rawdir/5YearCargoReport-Domestic-JAX.csv", /*
	*/ varnames(2)
keep if inlist(code,2100,2211,2221,2330)	// keep crude, gasoline, kerosene, diesel
keep commodity cy2019 cy2018
gen port = "JAX"	
* Save intermediate data file
tempfile temp_jax
save "`temp_jax'"

// Savannah, GA
clear all
import delimited "$rawdir/5YearCargoReport-Domestic-SavannahGA.csv", /*
	*/ varnames(2)
keep if inlist(code,2100,2211,2221,2330)	// keep crude, gasoline, kerosene, diesel
keep commodity cy2019 cy2018
gen port = "SAV"	
* Save intermediate data file
tempfile temp_sav
save "`temp_sav'"

// Charleston, SC
clear all
import delimited "$rawdir/5YearCargoReport-Domestic-CharlestonSC.csv", /*
	*/ varnames(2)
keep if inlist(code,2100,2211,2221,2330)	// keep crude, gasoline, kerosene, diesel
keep commodity cy2019 cy2018
gen port = "CHA"



*******************************************
// Append ports into a single file and save
append using "`temp_tam'"
append using "`temp_ev'"
append using "`temp_cnv'"
append using "`temp_jax'"
append using "`temp_sav'"
order port

saveold "$outdir/ArmyCorpsData.dta", version(17) replace

