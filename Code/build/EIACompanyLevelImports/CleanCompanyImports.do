* CLEAN COMPANY LEVEL IMPORTS
local fname CleanCompanyImports

******************** SYNOPSIS ***********************
// Read in and append the raw Company Level Imports data files downloaded from the EIA //
// government website, add PADD Mappings to the PORT_STATE, and lastly sort the data //
// with regards to DATE and PADD accordingly //

/*********** BASIC SETUP *********************/
clear all
set more off, permanently
capture log close

* Set local git directory and local dropbox directory
* Calling the path file works only if the working directory is nested in the repo
pathutil split "`c(pwd)'"
while "`s(filename)'" != "JonesAct" && "`s(filename)'" != "jonesact" {
	cd ..
	pathutil split "`c(pwd)'"
}
do "globals.do"

global mapdir = "$repodir/code/inputs/PADDMappings"

* Load dropbox location
global rawdir = "$dropbox/rawdata/orig/EIACompanyLevelImports"
global outdir = "$dropbox/intdata/EIACompanyLevelImports"


/*********** BEGIN CODE *********************/
* import the Product mappings.csv file and save it in a tempfile for future merge
import delimited "$mapdir/PADD_Mappings", varnames(1) case(upper) clear
tempfile padd_mappings
save "`padd_mappings'"


* Import the raw data file for year 2000 and store it in the tempfile
local it 0
import excel using "$rawdir/impa0`it'd.xls", firstrow clear
tempfile apdat
save "`apdat'"

* Append the raw data files from year 1986 to year 1999 to the tempfile
forval it = 86/99 {
	import excel using "$rawdir/impa`it'd.xls", firstrow clear
	append using "`apdat'", force
	save "`apdat'", replace
}

* There is a random text line in 2001
local it 1
import excel using "$rawdir/impa0`it'd.xls", firstrow clear
drop if LINE_NUM == "LINE_NUM"
* rename RPT_P since the dates are in a inconsistent format (strings) as for now
rename RPT_P RPT_PERIOD_post00
destring LINE_NUM PROD_CODE PORT_CODE PORT_PADD GCTRY_CODE QUANTITY SULFUR APIGRAVITY PCOMP_PADD, replace
append using "`apdat'"
save "`apdat'", replace

local it 2
import excel using "$rawdir/impa0`it'd.xls", firstrow clear
rename RPT_P RPT_PERIOD_post00
destring LINE_NUM PROD_CODE PORT_CODE PORT_PADD GCTRY_CODE QUANTITY SULFUR APIGRAVITY PCOMP_PADD, replace
append using "`apdat'"
save "`apdat'", replace

* Append the raw data files from year 2003 to year 2008 to the tempfile
forval it = 3/8 {
	import excel using "$rawdir/impa0`it'd.xls", firstrow clear
	destring LINE_NUM PROD_CODE PORT_CODE PORT_PADD GCTRY_CODE QUANTITY SULFUR APIGRAVITY PCOMP_PADD, replace
	append using "`apdat'"
	save "`apdat'", replace
}

* Append the raw data files from year 2009 to year 2012 to the tempfile
forval it = 9/12 {
	if (`it' == 9) {
		import excel using "$rawdir/impa0`it'd.xls", firstrow clear
	}
	else {
		import excel using "$rawdir/impa`it'd.xls", firstrow clear
	}
	* change the data type of RPT_PERIOD from time to dates
	replace RPT_PERIOD = dofc(RPT_PERIOD)
	destring LINE_NUM PROD_CODE PORT_CODE PORT_PADD GCTRY_CODE QUANTITY SULFUR APIGRAVITY PCOMP_PADD, replace
	append using "`apdat'"
	save "`apdat'", replace
}

* Append the raw data files from year 2013 to year 2015 to the tempfile
forval it = 13/15 {
	import excel using "$rawdir/impa`it'd.xls", firstrow clear
	destring LINE_NUM PROD_CODE PORT_CODE PORT_PADD GCTRY_CODE QUANTITY SULFUR APIGRAVITY PCOMP_PADD, replace
	append using "`apdat'"
	save "`apdat'", replace
}

* Append the raw data files from year 2016 
local it 16
import excel using "$rawdir/impa`it'd.xlsx", firstrow clear
destring LINE_NUM PROD_CODE PORT_CODE PORT_PADD	GCTRY_CODE QUANTITY	SULFUR APIGRAVITY PCOMP_PADD PCOMP_SITEID, replace
append using "`apdat'", force
save "`apdat'", replace

* Append the raw data files from year 2017
local it 17
import excel using "$rawdir/impa`it'd.xlsx", firstrow clear
destring LINE_NUM PROD_CODE PORT_CODE PORT_PADD	GCTRY_CODE QUANTITY	SULFUR APIGRAVITY PCOMP_PADD PCOMP_SITEID, replace
append using "`apdat'", force
save "`apdat'", replace

* Append the raw data files from year 2018
local it 18
import excel using "$rawdir/impa`it'd.xlsx", firstrow clear
destring LINE_NUM PROD_CODE PORT_CODE PORT_PADD	GCTRY_CODE QUANTITY	SULFUR APIGRAVITY PCOMP_PADD PCOMP_SITEID, replace
append using "`apdat'", force
save "`apdat'", replace

* Append the raw data files from year 2019
local it 19
import excel using "$rawdir/impa`it'd.xlsx", firstrow clear
destring LINE_NUM PROD_CODE PORT_CODE PORT_PADD	GCTRY_CODE QUANTITY	SULFUR APIGRAVITY PCOMP_PADD PCOMP_SITEID, replace
append using "`apdat'", force
save "`apdat'", replace

* Append the raw data files from year 2020 
local it 20
import excel using "$rawdir/impa`it'd.xlsx", firstrow clear
destring LINE_NUM PROD_CODE PORT_CODE PORT_PADD	GCTRY_CODE QUANTITY	SULFUR APIGRAVITY PCOMP_PADD PCOMP_SITEID, replace
append using "`apdat'", force
save "`apdat'", replace

* Append the raw data files from year 2021 
local it 21
import excel using "$rawdir/impa`it'd.xlsx", firstrow clear
destring LINE_NUM PROD_CODE PORT_CODE PORT_PADD	GCTRY_CODE QUANTITY	SULFUR APIGRAVITY PCOMP_PADD PCOMP_SITEID, replace
append using "`apdat'", force
save "`apdat'", replace

* Append the raw monthly data files from year 2022 to the tempfile
forval it = 1/12 {
	if (`it' > 9) {
		import excel using "$rawdir/2022_`it'.xlsx", firstrow clear
	} 
	else {
		import excel using "$rawdir/2022_0`it'.xlsx", firstrow clear
	}
	destring LINE_NUM PROD_CODE PORT_CODE PORT_PADD	GCTRY_CODE QUANTITY	SULFUR APIGRAVITY PCOMP_PADD PCOMP_SITEID, replace
	append using "`apdat'", force
	save "`apdat'", replace
} 

* Append the raw monthly data files from the first third of year 2023 to the tempfile
forval it = 1/5 {
	import excel using "$rawdir/2023_0`it'.xlsx", firstrow clear
	destring LINE_NUM PROD_CODE PORT_CODE PORT_PADD	GCTRY_CODE QUANTITY	SULFUR APIGRAVITY PCOMP_PADD PCOMP_SITEID, replace
	append using "`apdat'", force
	save "`apdat'", replace
} 

* Change the data format for years 2001-2002 from string to dates
destring RPT_PERIOD_post00, replace
replace RPT_PERIOD = mdy(mod(RPT_PERIOD_post00, 100), 31, 2000 + floor(RPT_PERIOD_post00/100)) if RPT_PERIOD>=.
replace RPT_PERIOD = mdy(mod(RPT_PERIOD_post00, 100), 30, 2000 + floor(RPT_PERIOD_post00/100)) if RPT_PERIOD>=.
replace RPT_PERIOD = mdy(mod(RPT_PERIOD_post00, 100), 29, 2000 + floor(RPT_PERIOD_post00/100)) if RPT_PERIOD>=.
replace RPT_PERIOD = mdy(mod(RPT_PERIOD_post00, 100), 28, 2000 + floor(RPT_PERIOD_post00/100)) if RPT_PERIOD>=.

* Label all the variable names
label var RPT_PERIOD	"Report Period in YYMM Format"
label var R_S_NAME	"Importing Company Name"
label var LINE_NUM	"Line Number Reported by Importer of Record"
label var PROD_CODE	"Internal EIA Product Code"
label var PROD_NAME	"Product Name"
label var PORT_CODE	"Port Code"
label var PORT_CITY	"Port of Entry"
label var PORT_STATE	"State Abbreviation"
label var PORT_PADD	"Petroleum Administration for Defense District (PADD) for the Port of Entry"
label var GCTRY_CODE	"Country Code"
label var CNTRY_NAME	"Country Name"
label var QUANTITY	"Import Quantity (thousand barrels)"
label var SULFUR	"Sulfur Percent"
label var APIGRAVITY	"API Gravity"
label var PCOMP_RNAM	"Processing Company Name"
label var PCOMP_SNAM	"Processing Facility Name"
label var PCOMP_STAT	"Processing Company State Code"
label var STATE_NAME	"Processing Company State"
label var PCOMP_PADD	"Processing Company PADD"

* Change all PORT_STATE names into standardized two-letter abbreviations
replace PORT_STATE = "AL" if PORT_STATE == "ALABAMA"
replace PORT_STATE = "AK" if PORT_STATE == "ALASKA"
replace PORT_STATE = "AZ" if PORT_STATE == "ARIZONA"
replace PORT_STATE = "AR" if PORT_STATE == "ARKANSAS"
replace PORT_STATE = "CA" if PORT_STATE == "CALIFORNIA"
replace PORT_STATE = "CO" if PORT_STATE == "COLORADO"
replace PORT_STATE = "CT" if PORT_STATE == "CONNECTICUT"
replace PORT_STATE = "DE" if PORT_STATE == "DELAWARE"
replace PORT_STATE = "FL" if PORT_STATE == "FLORIDA"
replace PORT_STATE = "GA" if PORT_STATE == "GEORGIA"
replace PORT_STATE = "HA" if PORT_STATE == "HAWAII" | PORT_STATE == "HI"
replace PORT_STATE = "ID" if PORT_STATE == "IDAHO"
replace PORT_STATE = "IL" if PORT_STATE == "ILLINOIS"
replace PORT_STATE = "IN" if PORT_STATE == "INDIANA"
replace PORT_STATE = "IA" if PORT_STATE == "IOWA"
replace PORT_STATE = "KS" if PORT_STATE == "KANSAS"
replace PORT_STATE = "KY" if PORT_STATE == "KENTUCKY"
replace PORT_STATE = "LA" if PORT_STATE == "LOUISIANA"
replace PORT_STATE = "ME" if PORT_STATE == "MAINE"
replace PORT_STATE = "MD" if PORT_STATE == "MARYLAND"
replace PORT_STATE = "MA" if PORT_STATE == "MASSACHUSETTS"
replace PORT_STATE = "MI" if PORT_STATE == "MICHIGAN"
replace PORT_STATE = "MN" if PORT_STATE == "MINNESOTA"
replace PORT_STATE = "MS" if PORT_STATE == "MISSISSIPPI"
replace PORT_STATE = "MO" if PORT_STATE == "MISSOURI"
replace PORT_STATE = "MT" if PORT_STATE == "MONTANA"
replace PORT_STATE = "NE" if PORT_STATE == "NEBRASKA"
replace PORT_STATE = "NV" if PORT_STATE == "NEVADA"
replace PORT_STATE = "NH" if PORT_STATE == "NEW HAMPSHIRE"
replace PORT_STATE = "NJ" if PORT_STATE == "NEW JERSEY"
replace PORT_STATE = "NM" if PORT_STATE == "NEW MEXICO"
replace PORT_STATE = "NY" if PORT_STATE == "NEW YORK"
replace PORT_STATE = "NC" if PORT_STATE == "NORTH CAROLINA"
replace PORT_STATE = "ND" if PORT_STATE == "NORTH DAKOTA"
replace PORT_STATE = "OH" if PORT_STATE == "OHIO"
replace PORT_STATE = "OK" if PORT_STATE == "OKLAHOMA"
replace PORT_STATE = "AK" if PORT_STATE == "ALASKA"
replace PORT_STATE = "OR" if PORT_STATE == "OREGON"
replace PORT_STATE = "PA" if PORT_STATE == "PENNSYLVANIA"
replace PORT_STATE = "RI" if PORT_STATE == "RHODE ISLAND"
replace PORT_STATE = "SC" if PORT_STATE == "SOUTH CAROLINA"
replace PORT_STATE = "SD" if PORT_STATE == "SOUTH DAKOTA"
replace PORT_STATE = "TN" if PORT_STATE == "TENNESSEE"
replace PORT_STATE = "TX" if PORT_STATE == "TEXAS"
replace PORT_STATE = "UT" if PORT_STATE == "UTAH"
replace PORT_STATE = "VT" if PORT_STATE == "VERMONT"
replace PORT_STATE = "VA" if PORT_STATE == "VIRGINIA"
replace PORT_STATE = "WA" if PORT_STATE == "WASHINGTON"
replace PORT_STATE = "WV" if PORT_STATE == "WEST VIRGINIA"
replace PORT_STATE = "WI" if PORT_STATE == "WISCONSIN"
replace PORT_STATE = "WY" if PORT_STATE == "WYOMING"
replace PORT_STATE = "GU" if PORT_STATE == "GUAM"
replace PORT_STATE = "PR" if PORT_STATE == "PUERTO RICO"
replace PORT_STATE = "VI" if PORT_STATE == "VIRGIN ISLANDS"
* Drop unused information
drop PCOMP_STAT
drop RPT_PERIOD_post00

* Rename RPT_PERIOD
rename RPT_PERIOD DATE

* Obtain PADD ID by merging PADD_Mappings into the data set
*merge m:1 state_abbreviation using "$rawdir/PADD_Mappings"
merge m:1 PORT_STATE using "`padd_mappings'", keepusing(PADD) 
drop if _merge == 2
replace PORT_PADD = PADD
drop PADD _merge
* Sort data by PORT_PADD first and then dates
sort PORT_PADD DATE

* Change the date to monthly date 
gen monthly=mofd(DATE)
format monthly %tm
rename DATE DATE_day
rename monthly DATE

* Add YEAR, MONTH variables and reorder
gen YEAR = year(DATE_day)
gen MONTH = month(DATE_day)

order PORT_PADD YEAR MONTH DATE
sort PORT_PADD YEAR MONTH 

* Drop observations with missing values
drop if PROD_NAME == ""
********** END *************
save "$outdir/CompanyImports", replace

capture log close
exit
