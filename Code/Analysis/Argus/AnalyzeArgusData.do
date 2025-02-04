/*
This file studies the freight rates in the Argus data
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
global rawdir = "$dropbox/intdata/Argus"
global outdir = "$dropbox/intdata/Argus"
global codedir = "$repodir/code/Analysis/Argus"
global figdir = "$repodir/output/figures"
global texdir = "$repodir/output/tex_numbers"

***************************************************
// Define constants
local bblpert = 7.47221


***************************************************
// Load the cleaned freight rate data
clear all
use "$rawdir/ArgusData.dta"

* Drop Argentina since we only have 1.5 months of data
drop Argentina

* Convert all rates from USD/tonne to USD/bbl
foreach name in "Mexico" "LasMinas" "Canada" "DR" "UKC" {
	replace `name' = `name' / `bblpert'
}

format Date %dm_CY

* Time series plot of dirty rates (not for publication)
twoway line UKC Date if DC=="D", lpattern("solid") lcolor("red") || /*
	*/ line Canada Date if DC=="D", lpattern("--.") lcolor("black") /*
	*/ xtitle("") ytitle("Freight rate $/bbl") /*
	*/ ylabel(#5, grid gstyle(off)) xlabel(#4, labsize(4) nogrid) /*
	*/ legend(label(1 "Rotterdam") label(2 "East coast Canada") cols(1) pos(11) /*
	*/ ring(0) bplacement("nwest"))
	
* Time series plot of clean rates (not for publication)
twoway line Canada Date if DC=="C", lpattern("--.") lcolor("black") || /*
	*/ line LasMinas Date if DC=="C", lpattern("solid") lcolor("red") || /*
	*/ line DR Date if DC=="C", lpattern("--") lcolor("blue") || /*
	*/ line Mexico Date if DC=="C", lpattern("-...") lcolor("green") /*
	*/ xtitle("") ytitle("Freight rate $/bbl") /*
	*/ ylabel(#5, grid gstyle(off)) xlabel(#4, labsize(4) nogrid) /*
	*/ legend(label(1 "East coast Canada") label(2 "Las Minas, Panama") /*
	*/ label(3 "Dominican Republic") label(4 "East coast Mexico") /*
	*/ cols(1) pos(11) ring(0) bplacement("nwest"))
	
* Plot of normalized averaged (across destinations) rates
* Normalization is clean rate on Jan 2, 2018 = 1
egen meanrate = rmean(Mexico-UKC)
replace meanrate = . if DC=="D" & UKC==.	// drop times when there is only one dirty dest
gen normrate = meanrate / meanrate[1]
twoway line normrate Date if DC=="D", lpattern("-.") lcolor("black")  lwidth("thick") || /*
	*/ line normrate Date if DC=="C", lpattern("solid") lcolor("green")  lwidth("thick") /*
	*/ xtitle("") ytitle("Normalized freight rate (Jan 2018 = 1)", size(5)) /*
	*/ ylabel(#5, grid gstyle(off) labsize(5)) xlabel(#4, labsize(5) nogrid) /*
	*/ xscale(range(21185 21960)) /*
	*/ legend(label(1 "Dirty freight") label(2 "Clean freight") cols(1) pos(11) /*
	*/ ring(0) bplacement("nwest") size(*1.3))
* TNR font for paper
graph set window fontface "Times New Roman"
graph export "$figdir/NormalizedFreightRates.pdf", replace
graph set window fontface default
* Drop unneeded vars
drop meanrate normrate



***************************************************
// Output mean rate difference across routes
foreach dest in "Mexico" "LasMinas" "Canada" "DR" "UKC" {
	bysort DC: egen M`dest' = mean(`dest') if DC=="C" | (DC=="D" & UKC~=.)
}
* Clean Las Minas vs Mexico
gen diff = MLasMinas - MMexico if DC=="C"
sum diff
local diffout = round(`r(mean)',0.01)
file open DiffLasMinas_Mexico using "$texdir/DiffLasMinas_Mexico.tex", write replace
file write DiffLasMinas_Mexico %9s "\\$0" (`diffout')
file close DiffLasMinas_Mexico
* Clean Canada vs Mexico
replace diff = .
replace diff = MCanada - MMexico if DC=="C"
sum diff
local diffout = round(`r(mean)',0.01)
file open DiffCanada_Mexico using "$texdir/DiffCanada_Mexico.tex", write replace
file write DiffCanada_Mexico %9s "\\$" (`diffout')
file close DiffCanada_Mexico
* Dirty UKC vs Canada
replace diff = .
replace diff = MUKC - MCanada if DC=="D"
sum diff
local diffout = round(`r(mean)',0.01)
file open DiffUKC_Canada using "$texdir/DiffUKC_Canada.tex", write replace
file write DiffUKC_Canada %9s "\\$0" (`diffout')
file close DiffUKC_Canada

drop MMexico-diff

	

***************************************************
// Input route distances. Output differences in distances
* Input distances from Houston to each destination, in nautical miles (nm)
* Distances from https://searoutes.com/routing-api/?speed=13&panama=true&suez=true&kiel=true&rivers=allow&roads=block
* Distances for ports in Argus dataset
local DistMexico = 515
local DistLasMinas = 1543
local DistCanada = 2575.5
local DistRotterdam = 5051
* Destinations for which we predict rates
local DistNYC = 1915		// NYC
local DistPHL = 1874		// Philadelphia
local DistTAM = 697			// Tampa, FL
local DistEV = 989			// Pt. Everglades, FL
local DistCNV = 1129		// Pt. Canaveral, FL
local DistJAX = 1264		// Jacksonville, FL
local DistSAV = 1368		// Savannah, GA
local DistCHA = 1398 		// Charleston, SC
local DistBOS = 2086		// Boston
local DistPR = 1839			// San Juan, Puerto Rico
local DistVI = 1937			// Christiansted, St Croix, US Virgin Islands
local DistLA = 4532			// Los Angeles, CA
* A few other distances for reference
local DistSuezToCNV = 5819	// Suez Canal to Pt. Canaveral, FL
local DistSuezToNYC = 5249 	// Suez Canal to NYC
local DistSuezToBOS = 5066 	// Suez Canal to BOS

* Export distances
file open DistMexico using "$texdir/DistMexico.tex", write replace
file write DistMexico %9s (`DistMexico')
file close DistMexico
file open DistLasMinas using "$texdir/DistLasMinas.tex", write replace
file write DistLasMinas %9s (`DistLasMinas')
file close DistLasMinas
file open DistCanada using "$texdir/DistCanada.tex", write replace
file write DistCanada %9s (`DistCanada')
file close DistCanada
file open DistRotterdam using "$texdir/DistRotterdam.tex", write replace
file write DistRotterdam %9s (`DistRotterdam')
file close DistRotterdam
file open DistNYC using "$texdir/DistNYC.tex", write replace
file write DistNYC %9s (`DistNYC')
file close DistNYC
file open DistPHL using "$texdir/DistPHL.tex", write replace
file write DistPHL %9s (`DistPHL')
file close DistPHL
file open DistTAM using "$texdir/DistTAM.tex", write replace
file write DistTAM %9s (`DistTAM')
file close DistTAM
file open DistEV using "$texdir/DistEV.tex", write replace
file write DistEV %9s (`DistEV')
file close DistEV
file open DistCNV using "$texdir/DistCNV.tex", write replace
file write DistCNV %9s (`DistCNV')
file close DistCNV
file open DistJAX using "$texdir/DistJAX.tex", write replace
file write DistJAX %9s (`DistJAX')
file close DistJAX
file open DistSAV using "$texdir/DistSAV.tex", write replace
file write DistSAV %9s (`DistSAV')
file close DistSAV
file open DistCHA using "$texdir/DistCHA.tex", write replace
file write DistCHA %9s (`DistCHA')
file close DistCHA
file open DistBOS using "$texdir/DistBOS.tex", write replace
file write DistBOS %9s (`DistBOS')
file close DistBOS
file open DistPR using "$texdir/DistPR.tex", write replace
file write DistPR %9s (`DistPR')
file close DistPR
file open DistVI using "$texdir/DistVI.tex", write replace
file write DistVI %9s (`DistVI')
file close DistVI
file open DistLA using "$texdir/DistLA.tex", write replace
file write DistLA %9s (`DistLA')
file close DistLA
file open DistSuezToCNV using "$texdir/DistSuezToCNV.tex", write replace
file write DistSuezToCNV %9s (`DistSuezToCNV')
file close DistSuezToCNV
file open DistSuezToNYC using "$texdir/DistSuezToNYC.tex", write replace
file write DistSuezToNYC %9s (`DistSuezToNYC')
file close DistSuezToNYC


***************************************************
// Reshape long, with distances, to set up regressions
* Drop DR since it's the same as Las Minas
drop DR
* Renames to prep reshape
rename Mexico Rate1
rename LasMinas Rate2
rename Canada Rate3
rename UKC Rate4
* Input distances from Houston to each destination, in nautical miles (nm)
* Distances from https://searoutes.com/routing-api/?speed=13&panama=true&suez=true&kiel=true&rivers=allow&roads=block
* Include NYC and PHL as destinations for which we predict rates
gen Dist1 = `DistMexico'			// East coast Mexico
gen Dist2 = `DistLasMinas'			// Las Minas, Panama
gen Dist3 = `DistCanada'			// East coast Canada
gen Dist4 = `DistRotterdam'			// Rotterdam
gen Dist5 = `DistNYC'				// NYC
gen Dist6 = `DistPHL'				// PHL
gen Dist7 = `DistTAM'				// Tampa, FL
gen Dist8 = `DistEV'				// Pt. Everglades, FL
gen Dist9 = `DistCNV'				// Pt. Canaveral, FL
gen Dist10 = `DistJAX'				// Jacksonville, FL
gen Dist11 = `DistSAV'				// Savannah, GA
gen Dist12 = `DistCHA'				// Charleston, SC
gen Dist13 = `DistBOS'				// Boston, MA
gen Dist14 = `DistPR'				// San Juan, Puerto Rico
gen Dist15 = `DistVI'				// Christiansted, St Croix, US Virgin Islands
gen Dist16 = `DistLA'				// Los Angeles, CA
reshape long Dist Rate, i(DC Date) j(Dest)

* Get log rates and rescaled distance
gen LogRate = log(Rate)
gen Dist1000 = Dist/1000



***************************************************
// Estimate model for dirty freight
* Initial regression to separate time vs destination dummies
areg LogRate i.Dest if DC=="D", a(Date)
predict TimeEffect, d					// Time effects. Missing for missing obs
predict DestEffect if DC=="D", xb		// Location effects (incl constant). Never missing
* Extend time effect to all destinations
bysort Date: egen MeanTimeEff = mean(TimeEffect) if DC=="D"
sort DC Date Dest
* Distance regression
gen ExpDest = exp(DestEffect)
reg ExpDest Dist1000 if inlist(Dest,3,4)
local betadist = round(_b[Dist1000],0.01)
predict DistEffect if DC=="D"
* Export coef on distance to tex
file open BetaDist_dirty using "$texdir/BetaDist_dirty.tex", write replace
file write BetaDist_dirty %9s "0" (`betadist')
file close BetaDist_dirty
* Spec with distance in logs
gen LogDist1000 = log(Dist1000)
reg ExpDest LogDist1000 if inlist(Dest,3,4)
predict LogDistEffect if DC=="D"
* Predicted rates
gen PredRate = exp(MeanTimeEff)*DistEffect
gen PredRate_LD = exp(MeanTimeEff)*LogDistEffect
drop TimeEffect-LogDistEffect



// Estimate model for clean freight
* Initial regression to separate time vs destination dummies
areg LogRate i.Dest if DC=="C", a(Date)
predict TimeEffect, d					// Time effects. Missing for missing obs
predict DestEffect if DC=="C", xb		// Location effects (incl constant). Never missing
* Extend time effect to all destinations
bysort Date: egen MeanTimeEff = mean(TimeEffect) if DC=="C"
sort DC Date Dest
* Distance regression
gen ExpDest = exp(DestEffect)
reg ExpDest Dist1000 if inlist(Dest,1,2,3)
local betadist = round(_b[Dist1000],0.01)
local distr2 = round(e(r2),0.001)
predict DistEffect if DC=="C"
* Export coef on distance and r2 to tex
file open BetaDist_clean using "$texdir/BetaDist_clean.tex", write replace
file write BetaDist_clean %9s "0" (`betadist')
file close BetaDist_clean
file open R2Dist_clean using "$texdir/R2Dist_clean.tex", write replace
file write R2Dist_clean %9s "0" (`distr2')
file close R2Dist_clean
* Spec with distance in logs
gen LogDist1000 = log(Dist1000)
reg ExpDest LogDist1000 if inlist(Dest,1,2,3)
local ldistr2 = round(e(r2),0.001)
predict LogDistEffect if DC=="C"
file open R2LDist_clean using "$texdir/R2LDist_clean.tex", write replace
file write R2LDist_clean %9s "0" (`ldistr2')
file close R2LDist_clean
* Predicted rates
replace PredRate = exp(MeanTimeEff)*DistEffect if DC=="C"
replace PredRate_LD = exp(MeanTimeEff)*LogDistEffect if DC=="C"
drop TimeEffect-LogDistEffect



***************************************************
// Plot predictions for clean shipments to CNV and NYC
twoway line PredRate Date if DC=="C" & Dest==5, lpattern("solid") lcolor("black") lwidth("medthick") || /*
	*/ line PredRate Date if DC=="C" & Dest==9, lpattern("solid") lcolor("green") lwidth("thick") /*
	*/ xtitle("") ytitle("Predicted freight rate, $/bbl", size(7)) /*
	*/ ylabel(0(1)6, grid gstyle(off) labsize(7)) xlabel(#4, labsize(7) nogrid) /*
	*/ xscale(range(21185 21960)) /*
	*/ legend(label(1 "USGC to NYC") label(2 "USGC to Port Canaveral")  cols(1) pos(11) /*
	*/ ring(0) bplacement("nwest") size(*1.8))
* TNR font for paper
graph set window fontface "Times New Roman"
graph export "$figdir/PredictedFreightRates_NYCCNV_clean.pdf", replace
graph set window fontface default



***************************************************
// Plot predictions for dirty shipments to CNV and NYC
twoway line PredRate Date if DC=="D" & Dest==5, lpattern("solid") lcolor("black") lwidth("medthick") || /*
	*/ line PredRate Date if DC=="D" & Dest==9, lpattern("solid") lcolor("green") lwidth("thick") /*
	*/ xtitle("") ytitle("Predicted freight rate, $/bbl", size(7)) /*
	*/ ylabel(0(1)6, grid gstyle(off) labsize(7)) xlabel(#4, labsize(7) nogrid) /*
	*/ xscale(range(21185 21960)) /*
	*/ legend(label(1 "USGC to NYC") label(2 "USGC to Port Canaveral")  cols(1) pos(11) /*
	*/ ring(0) bplacement("nwest") size(*1.8))
* TNR font for paper
graph set window fontface "Times New Roman"
graph export "$figdir/PredictedFreightRates_NYCCNV_dirty.pdf", replace
graph set window fontface default




***************************************************
// Export predictions to csv
keep DC Date Dest PredRate PredRate_LD
keep if inlist(Dest,5,6,7,8,9,10,11,12,13,14,15,16)
gen Destination = "NYC"
replace Destination = "PHL" if Dest==6
replace Destination = "TAM" if Dest==7
replace Destination = "EV" if Dest==8
replace Destination = "CNV" if Dest==9
replace Destination = "JAX" if Dest==10
replace Destination = "SAV" if Dest==11
replace Destination = "CHA" if Dest==12
replace Destination = "BOS" if Dest==13
replace Destination = "PR" if Dest==14
replace Destination = "VI" if Dest==15
replace Destination = "LA" if Dest==16

drop Dest
order DC Destination Date PredRate PredRate_LD
sort DC Destination Date 
export delimited using "$outdir/PredictedFreightRates.csv", replace


