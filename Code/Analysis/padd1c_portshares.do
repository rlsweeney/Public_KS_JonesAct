/*
This file creates a table showing the share of domestic movements and imports
going to the six main padd 1c ports
Shares from domestic movements come from the Army Corps data
Shares from imports come from the company-level imports
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
global rawdir = "$dropbox/intdata"
global outdir = "$dropbox/intdata"
global codedir = "$repodir/code/Analysis"
global figdir = "$repodir/output/figures"
global texdir = "$repodir/output/tex_numbers"
global tabdir = "$repodir/output/tables"



***************************************************
* Start with domestic movements
***************************************************
// Load the cleaned Army Corps data
clear all
use "$rawdir/ArmyCorps/ArmyCorpsData.dta"

// Sum volumes across years, ignore crude
rename commodity prod_category
drop if prod_category=="Crude Petroleum"
replace prod_category = "Conventional" if prod_category=="Gasoline"
replace prod_category = "JetFuel" if prod_category=="Kerosene"
replace prod_category = "ULSD" if prod_category=="Distillate Fuel Oil"
gen Q = cy2019 + cy2018
drop cy2019 cy2018

// Get percentages to each port
bysort prod_category: egen Qtot = sum(Q)
gen PortPctDom = Q / Qtot * 100
drop Q Qtot
tempfile temp_portmoves
save "`temp_portmoves'"



***************************************************
* Process imports
***************************************************
use "$rawdir/EIACompanyLevelImports/CompanyImports_renamed.dta", clear

// Keep 2018-2019, desired ports and products
keep if inlist(YEAR,2018,2019)
keep if inlist(PORT_CITY,"TAMPA, FL","PT EVERGLADE, FL","PT CANAVERAL, FL", /*
	*/ "JACKSONVILLE, FL","SAVANNAH,GA","CHARLESTON, SC")
gen prod_category = ""
replace prod_category = "Conventional" if inlist(PROD_NAME, /*
	*/ "ALL OTHER MOTOR GAS BLENDING COMPONENTS", /*
	*/ "MOTOR GAS, CONVENTIONAL, OTHER", /*
	*/ "MGBC, GASOLINE TREATED AS BLENDSTOCK (GTAB)", /*
	*/ "MGBC, CONVENTIONAL BLENDSTOCK FOR OXYGENATE BLENDING (CBOB)")
replace prod_category = "JetFuel" if PROD_NAME=="JET FUEL, KEROSENE-TYPE"
replace prod_category = "ULSD" if PROD_NAME=="DISTILLATE, 15 PPM SULFUR AND UNDER"
drop if prod_category==""
rename PORT_CITY port
keep port prod_category QUANTITY
replace port = "TAM" if port=="TAMPA, FL"
replace port = "EV" if port=="PT EVERGLADE, FL"
replace port = "CNV" if port=="PT CANAVERAL, FL"
replace port = "JAX" if port=="JACKSONVILLE, FL"
replace port = "SAV" if port=="SAVANNAH,GA"
replace port = "CHA" if port=="CHARLESTON, SC"

// Get percentages to each port
bysort port prod_category: egen Q = sum(QUANTITY)
drop QUANTITY
duplicates drop
bysort prod_category: egen Qtot = sum(Q)
gen PortPctImp = Q / Qtot * 100
drop Q Qtot
tempfile temp_portimp
save "`temp_portimp'"


***************************************************
* Merge and create tex table
***************************************************
use "`temp_portmoves'", clear
merge 1:1 port prod_category using "`temp_portimp'"
drop _merge
replace PortPctImp = 0 if PortPctImp==.		// one missing value with no volume
gen PortOrder = 1
replace PortOrder = 2 if port=="EV"
replace PortOrder = 3 if port=="CNV"
replace PortOrder = 4 if port=="JAX"
replace PortOrder = 5 if port=="SAV"
replace PortOrder = 6 if port=="CHA"
sort port prod_category
reshape long PortPct, i(PortOrder port prod_category) j(type) string
rename PortPct P
reshape wide P, i(PortOrder port type) j(prod_category) string
sort type PortOrder

* Write table
file open tabports using "$tabdir/Padd1cportshares.tex", write replace
file write tabports "\begin{tabular}{lccc}" _n
file write tabports	"\hline" _n
file write tabports	"& Conventional & & \\" _n
file write tabports	"& Gasoline & Jet fuel & ULSD \\" _n
file write tabports	"\hline" _n
file write tabports	"\textbf{Domestic movements} &  &  &  \\" _n
file write tabports	"Tampa, FL & " %2.0f (PConventional[1]) "\% & " ///
	%2.0f (PJetFuel[1]) "\% & " %2.0f (PULSD[1]) "\% \\" _n
file write tabports	"Pt. Everglades, FL & " %2.0f (PConventional[2]) "\% & " ///
	%2.0f (PJetFuel[2]) "\% & " %2.0f (PULSD[2]) "\% \\" _n
file write tabports	"Pt. Canaveral, FL & " %2.0f (PConventional[3]) "\% & " ///
	%2.0f (PJetFuel[3]) "\% & " %2.0f (PULSD[3]) "\% \\" _n
file write tabports	"Jacksonville, FL & " %2.0f (PConventional[4]) "\% & " ///
	%2.0f (PJetFuel[4]) "\% & " %2.0f (PULSD[4]) "\% \\" _n
file write tabports	"Savannah, GA & " %2.0f (PConventional[5]) "\% & " ///
	%2.0f (PJetFuel[5]) "\% & " %2.0f (PULSD[5]) "\% \\" _n
file write tabports	"Charleston, SC & " %2.0f (PConventional[6]) "\% & " ///
	%2.0f (PJetFuel[6]) "\% & " %2.0f (PULSD[6]) "\% \\" _n
file write tabports	"\hline" _n
file write tabports	"\textbf{Imports} &  &  &  \\" _n
file write tabports	"Tampa, FL & " %2.0f (PConventional[7]) "\% & " ///
	%2.0f (PJetFuel[7]) "\% & " %2.0f (PULSD[7]) "\% \\" _n
file write tabports	"Pt. Everglades, FL & " %2.0f (PConventional[8]) "\% & " ///
	%2.0f (PJetFuel[8]) "\% & " %2.0f (PULSD[8]) "\% \\" _n
file write tabports	"Pt. Canaveral, FL & " %2.0f (PConventional[9]) "\% & " ///
	%2.0f (PJetFuel[9]) "\% & " %2.0f (PULSD[9]) "\% \\" _n
file write tabports	"Jacksonville, FL & " %2.0f (PConventional[10]) "\% & " ///
	%2.0f (PJetFuel[10]) "\% & " %2.0f (PULSD[10]) "\% \\" _n
file write tabports	"Savannah, GA & " %2.0f (PConventional[11]) "\% & " ///
	%2.0f (PJetFuel[11]) "\% & " %2.0f (PULSD[11]) "\% \\" _n
file write tabports	"Charleston, SC & " %2.0f (PConventional[12]) "\% & " ///
	%2.0f (PJetFuel[12]) "\% & " %2.0f (PULSD[12]) "\% \\" _n
file write tabports	"\hline" _n
file write tabports "\end{tabular}"
file close tabports
