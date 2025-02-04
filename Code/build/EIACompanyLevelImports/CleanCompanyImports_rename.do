* CLEAN COMPANY LEVEL IMPORTS
local fname CleanCompanyImports_rename

******************** SYNOPSIS ***********************
// Read in the intermediate Company Imports data file from the CleanCompanyImports.do //
// file and standardize all the string variables in the data //

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

global mapdir = "$repodir/code/inputs/PRODMappings"

* Load dropbox location
global rawdir = "$dropbox/intdata/EIACompanyLevelImports"
global outdir = "$dropbox/intdata/EIACompanyLevelImports"


/*********** BEGIN CLEANING DATA *********************/

use "$rawdir/CompanyImports"

* Change all CNTRY_NAME to be standardized country names
replace CNTRY_NAME = "ABU DHABI" if CNTRY_NAME == "ABU ZABY"
replace CNTRY_NAME = "BAHAMAS" if CNTRY_NAME == "BAHAMAS, The" ///
| CNTRY_NAME == "BAHAMAS, THE"
replace CNTRY_NAME = "CHINA" if CNTRY_NAME == "CHINA, PEOPLE'S REP" ///
| CNTRY_NAME == "CHINA, PEOPLES REP"
replace CNTRY_NAME = "COTE D'IVOIRE" if CNTRY_NAME == "COTE D'IVOIRE (IVORY COAST)" ///
| CNTRY_NAME == "COTE D'IVOIRE(IVORY COAST)" | CNTRY_NAME == "IVORY COAST"
replace CNTRY_NAME = "KOREA, SOUTH" if CNTRY_NAME == "SOUTH KOREA" ///
| CNTRY_NAME == "KOREA, REPUBLIC OF"
replace CNTRY_NAME = "SOUTH AFRICA" if CNTRY_NAME == "SOUTH AFRICA, REP OF"

* Change all R_S_NAME/PCOMP_RNAM to be standardized company names
* Kill off trailing white spaces in all reported names
replace R_S_NAME = strrtrim(R_S_NAME)
replace PCOMP_RNAM = strrtrim(PCOMP_RNAM)

* standardize company names
replace R_S_NAME = "AGGREGATE INDUSTRIES-NORTHEAST" if R_S_NAME == "AGGREGATE INDUSTRIES NORTHEAST REGIO"
replace R_S_NAME = "AGWAY PETRO CORP" if R_S_NAME == "AGWAY ENERGY PROD LLC"
replace R_S_NAME = "ALL STATES ASPH INC" if R_S_NAME == "ALL STATES ASPHALT INC"
replace R_S_NAME = "ALON REFINING KROTZ SPGS INC" if R_S_NAME == "ALON USA LP"
replace R_S_NAME = "AMERADA HESS CORP" if R_S_NAME == "AMERADA HESS PORT READING CORP" ///
| R_S_NAME == "AMERADA HESS TRADG CO"
replace R_S_NAME = "AMERICAN REFINING GROUP INC" if R_S_NAME == "AMERICAN REFINING GROUP"
replace R_S_NAME = "APEX OIL CO" if R_S_NAME == "APEX OIL CO LLC"
replace R_S_NAME = "ARCO" if R_S_NAME == "ARCO CRUDE OIL & NGL MKTG" ///
| R_S_NAME == "ARCO CHEM CO" | R_S_NAME == "ARCO PROD CO"
replace R_S_NAME = "ATLANTIC TRDG & MKTG INC" if R_S_NAME == "ATLANTIC TRADING & MARKETING" ///
| R_S_NAME == "ATLANTIC TRADING MARKETING"
replace R_S_NAME = "AUTORE OIL CO" if R_S_NAME == "AUTORE OIL PROPANE"
replace R_S_NAME = "BASF CORP POLYMERS DIV" if R_S_NAME == "BASF CORP POLYMERS DIVISION"
replace R_S_NAME = "BLUE WATER OIL CO INC" if R_S_NAME == "BLUE WATER OIL TRANSPORT" ///
| R_S_NAME == "BLUE WATER TRANSPORT LLC" 
replace R_S_NAME = "BOGNAR EJ INC" if R_S_NAME == "BOGNAR E J INC"
replace R_S_NAME = "BP CANADA ENERGY MKTG CORP" if R_S_NAME == "BP CANADA ENERGY MARKETING"
replace R_S_NAME = "BP PRODTS N AMER INC" if R_S_NAME == "BP PRODUCTS N AMERICA INC" ///
| R_S_NAME == "BP PRODUCTS NORTH AMERICA INC" | R_S_NAME == "BP N AMER PETRO INC"
replace R_S_NAME = "BP WEST COAST PRODTS LLC" if R_S_NAME == "BP WEST COAST PRODUCTS LLC"
replace R_S_NAME = "BULK OIL (USA) INC" if R_S_NAME == "BULK TRADG & TRANSP CO" ///
| R_S_NAME == "BULK TRADING & TRANSP CO"
replace R_S_NAME = "CALIFORNIA CEDAR PROD CO" if R_S_NAME == "CALIFORNIA CEDAR PRODUCT CO"
replace R_S_NAME = "CANADIAN ENTERPRISE GAS PRODTS LTD" if R_S_NAME == "CANADIAN ENTERPRISE GAS PRODUCTS LTD"
replace R_S_NAME = "CARIBBEAN PETROLEUM" if R_S_NAME == "CARIBBEAN GULF REFG CORP"
replace R_S_NAME = "CAVALIER GAS CO" if R_S_NAME == "CAVALIER GAS LLC"
replace R_S_NAME = "CENEX INC" if R_S_NAME == "CENEX HARVEST STATES COOP"
replace R_S_NAME = "CENOVUS ENERGY" if R_S_NAME == "CENOVUS ENERGY MKTG SVCS LTD" ///
| R_S_NAME == "CENOVUS MARKETING USA INC"
replace R_S_NAME = "CENTENNIAL ENERGY LLC" if R_S_NAME == "CENTENNIAL GAS LIQ"
replace R_S_NAME = "CHAMPLAIN OIL CO" if R_S_NAME == "CHAMPLAIN OIL CO INC" ///
| R_S_NAME == "CHAMPLIN PETRO CO" | R_S_NAME == "CHAMPLIN REFG & CHEM INC" ///
| R_S_NAME == "CHAMPLIN REFG CO" 
replace R_S_NAME = "CHEVRON CORP" if R_S_NAME == "CHEVRON MARINE PRODUCTS LLC" ///
| R_S_NAME == "CHEVRON PUERTO RICO LLC" | R_S_NAME == "CHEVRON USA INC"
replace R_S_NAME = "CHEVRON PHILLIPS" if strpos(R_S_NAME, "CHEVRON PHILLIPS")
replace R_S_NAME = "CITGO PETRO CORP" if R_S_NAME == "CITGO ASPH REFG CO" ///
| R_S_NAME == "CITGO ASPHALT REFINING CO" | R_S_NAME == "CITGO PETROLEUM CORP"
replace R_S_NAME = "CLARENDON MKTG INC" if R_S_NAME == "CLARENDON LTD"
replace R_S_NAME = "Clark Brands, LLC" if R_S_NAME == "CLARK OIL & REFG CORP" ///
| R_S_NAME == "CLARK OIL TRADG CO" | R_S_NAME == "CLARK OIL TRADING CO" ///
| R_S_NAME == "CLARK REFG & MKTG INC"
replace R_S_NAME = "COASTAL CORP" if R_S_NAME == " COASTAL CORP THE" ///
| R_S_NAME == "COASTAL OIL CO OF NEW YORK INC"
replace R_S_NAME = "COASTAL FUELS MKTG" if R_S_NAME == "COASTAL FUELS & MKTG INC"
replace R_S_NAME = "COCHIN P L LTD" if R_S_NAME == "COCHIN PL LTD"
replace R_S_NAME = "COLLINS S B INC" if R_S_NAME == "COLLINS SB INC"
replace R_S_NAME = "COLONIAL OIL INDUSTRIES INC" if R_S_NAME == "COLONIAL OIL INDUS INC"
replace R_S_NAME = "CONOCOPHILLIPS CO" if R_S_NAME == "CONOCO INC" ///
| R_S_NAME == "CONOCO PHILLIPS" | R_S_NAME == "ConocoPhillips Company"
replace R_S_NAME = "D & C TRANSPORTATION INC" if R_S_NAME == "D & C TRANSP INC"
replace R_S_NAME = "DOW CHEMICAL CO" if R_S_NAME == "DOW CHEMICAL CO THE" ///
| R_S_NAME == "DOW CHEMICALS CO THE"
replace R_S_NAME = "DOW HYDROCARBONS & RESRCS INC" if R_S_NAME == "DOW HYDROCARBONS & RESRCS LLC"
replace R_S_NAME = "DUKE ENERGY MERCHANTS LLC" if R_S_NAME == "DUKE ENERGY MERCHANTS LC"
replace R_S_NAME = "DYNEGY MIDSTREAM SERVICES LP" if R_S_NAME == "DYNEGY MIDSTREAM SVCS LP"
replace R_S_NAME = "EDGINGTON OIL CO INC" if R_S_NAME == "EDGINGTON OIL CO LLC"
replace R_S_NAME = "ENRON LIQ FUELS INC" if R_S_NAME == "ENRON LIQ FUELS INC" ///
| R_S_NAME == "ENRON PRODTN & MKTG CO"
replace R_S_NAME = "EPSILON TRADING INC" if R_S_NAME == "EPSILON TRADING LLC"
replace R_S_NAME = "ERGON REFINING INC" if R_S_NAME == "ERGON REFG INC"
replace R_S_NAME = "FEDERATED COOP INC" if R_S_NAME == "FEDERATED COOP"
replace R_S_NAME = "FERRELLGAS INC" if R_S_NAME == "FERRELL NORTH AMERICA" ///
| R_S_NAME == "FERRELL PETRO INC"
replace R_S_NAME = "GAS CO" if R_S_NAME == "GAS CO THE"
replace R_S_NAME = "GAS SUPPLY RESOURCES" if R_S_NAME == "GAS SUPPLY INC" ///
| R_S_NAME == "GAS SUPPLY RESOURCES INC" ///
| R_S_NAME == "GAS SUPPLY RESOURCES LLC" | R_S_NAME == "GAS SUPPLY RESRCS INC"
replace R_S_NAME = "GEORGIA PACIFIC CORP" if R_S_NAME == "GEORGIA-PACIFIC CONSUMER PRODTS LP"
replace R_S_NAME = "GLOBALWAX LLC" if R_S_NAME == "GLOBAL WAX LLC"
replace R_S_NAME = "GOETZ ENERGY CORP" if R_S_NAME == "GOETZ ENERGY"
replace R_S_NAME = "GREAT LAKES CARBON CORP" if R_S_NAME == "GREAT LAKES CARBON LLC"
replace R_S_NAME = "HELM US CHEMICAL CORP" if R_S_NAME == "HELM US CHEM CORP"
replace R_S_NAME = "HESS CORP" if R_S_NAME == "HESS ENERGY TRADING CO LLC"
replace R_S_NAME = "HIGH SIERRA CRUDE OIL & MKTG" if R_S_NAME == "HIGH SIERRA ENERGY"
replace R_S_NAME = "HUDSON LIQ ASPH INC" if R_S_NAME == "HUDSON LIQ ASPHALT INC"
replace R_S_NAME = "HUNT CRUDE OIL SUPPLY CO" if R_S_NAME == "HUNT OIL CO" ///
| R_S_NAME == "HUNT REFG CO"
replace R_S_NAME = "INDUSTRIAL RAW MATERIALS LLC" if R_S_NAME == "INDUSTRIAL RAW MATERIALS INC" ///
| R_S_NAME == "INDUSTRIAL RAW MATERIALS CORP"
replace R_S_NAME = "KILDAIR SERV LTD" if R_S_NAME == "KILDAIR SERVICE LTEE"
replace R_S_NAME = "KINETIC RESC" if R_S_NAME == "KINETIC RESOURCES USA" ///
| R_S_NAME == "KINETIC RSRCS"
replace R_S_NAME = "KOLMAR AMERICAS INC" if R_S_NAME == "KOLMAR PETROCHEM AMERICAS INC"
replace R_S_NAME = "LOUIS DREYFUS COMPANY" if R_S_NAME == "LOUIS DREYFUS AGRI INDUSTRY" ///
| R_S_NAME == "LOUIS DREYFUS CLAYPOOL HOLDINGS" | R_S_NAME == "LOUIS DREYFUS COMMODITIES ETHANOL MD" ///
| R_S_NAME == "LOUIS DREYFUS ENERGY SVCES LP" | R_S_NAME == "LOUIS DREYFUS ENERGY SVCS LP"
replace R_S_NAME = "LYONDELL CITGO REFG LP" if R_S_NAME == "LYONDELL CHEM WORLDWIDE INC" ///
| R_S_NAME == "LYONDELL PETROCHEM CO" | R_S_NAME == "LYONDELL-CITGO REFINING LP"
replace R_S_NAME = "MACQUARIE ENERGY NORTH AMERICA TRAD" if R_S_NAME == "MACQUARIE COMMODITIES TRDG US"
replace R_S_NAME = "MARATHON PETRO CO" if R_S_NAME == "MARATHON ASHLAND PETRO LLC" ///
| R_S_NAME == "MARATHON PETRO CO LLC" | R_S_NAME == "MARATHON PETROLEUM CO LLC"
replace R_S_NAME = "MERCURIA ENERGY CANADA INC" if R_S_NAME == " MERCURIA CANADA COMMODITIES CORP"
replace R_S_NAME = "MICHIGAN PETROLEUM TECH" if R_S_NAME == "MICHIGAN PETRO TECHNLGYS"
replace R_S_NAME = "NGL SUPPLY WHOLESALE LLC" if R_S_NAME == "NGL CRUDE LOGISTICS LLC" ///
| R_S_NAME == "NGL SUPPLY CO LTD" | R_S_NAME == "NGL SUPPLY INC" 
replace R_S_NAME = "NORTHEAST PETRO CORP" if R_S_NAME == "NORTHEAST PRODUCTS CO"
replace R_S_NAME = "NORTHERN ENERGY INC" if R_S_NAME == "NORTHERN PETROCHEM CO"
replace R_S_NAME = "NORTHWEST PETRO CO" if R_S_NAME == "NORTHWEST LP GAS CO" ///
| R_S_NAME == "NORTHWEST PETROLEUM CO" | R_S_NAME == "NORTHWEST PROPANE INC"
replace R_S_NAME = "NOVA CHEM CORP" if R_S_NAME == "NOVA CHEM INC" ///
| R_S_NAME == "NOVA CHEMICAL CORP" | R_S_NAME == "NOVA CHEMICALS CANADA LTD" ///
| R_S_NAME == "NOVA CHEMICALS INC" | R_S_NAME == "NOVA CHEMICALS OLEFINS LLC" ///
| R_S_NAME == "NOVA PETRO SPECIALTIES CO LLC" | R_S_NAME == "NOVA PETROLEUM & CHEMS CORP" ///
| R_S_NAME == "NOVA PETROLEUM SPECIALTIES CO LLC" 
replace R_S_NAME = "NOVACOR CHEM INC" if R_S_NAME == "NOVACOR CHEM INC" 
replace R_S_NAME = "NRG ENERGY MKTG" if R_S_NAME == "NRG ENERGY INC" ///
| R_S_NAME == "NRG ENERGY INC" | R_S_NAME == "NRG ENERGY MARKETING"
replace R_S_NAME = "NUSTAR ASPHALT LLC" if R_S_NAME == "NUSTAR ASPHALT REFINING LLC"
replace R_S_NAME = "OXBOW RESC INC" if R_S_NAME == "OXBOW CALCINING INTL LLC" ///
| R_S_NAME == "OXBOW CARBON & MINERALS"
replace R_S_NAME = "PARAMOUNT PETROLEUM CORP" if R_S_NAME == "PARAMOUNT PETOLEUM CORP" ///
| R_S_NAME == "PARAMOUNT PETRO CORP" | R_S_NAME == "PARAMOUNT PETRO CORP-WILLBRIDGE"
replace R_S_NAME = "PDV MIDWEST REFINING LLC" if R_S_NAME == "PDV MIDWEST REFG LLC"
replace R_S_NAME = "PEMBINA RESOURCE SERVICES USA" if R_S_NAME == "PEMBINA MIDSTREAM USA INC"
replace R_S_NAME = "PETRO-CANADA AMERICA" if R_S_NAME == "PETRO-CANADA CHEM INC" ///
| R_S_NAME == "PETRO-CANADA CHEMICAL INC" | R_S_NAME == "PETRO-CANADA PROD"
replace R_S_NAME = "PETRO-DIAMOND INC" if R_S_NAME == "PETROLEUM DIAMOND INC"
replace R_S_NAME = "PLAINS MIDSTREAM CANADA ULC" if R_S_NAME == "PLAINS LPG SERVICES LP" ///
| R_S_NAME == "PLAINS MARKETING LP" | R_S_NAME == "PLAINS MIDSTREAM EMPRESS MGMT LP" ///
| R_S_NAME == "PLAINS MKTG CANADA LP" | R_S_NAME == "PLAINS MKTG CANADA LP" ///
| R_S_NAME == "PLAINS MKTG LP (HOUSTON TX)"
replace R_S_NAME = "POWER ENERGY PARTNERS LP" if R_S_NAME == "POWER ENERGY MARKETING LLC"
replace R_S_NAME = "PRSI TRADING CO LP" if R_S_NAME == "PRSI TRADING LLC" ///
| R_S_NAME == "PRSI TRADING LP"
replace R_S_NAME = "RYMES HEATING OIL INC" if R_S_NAME == "RYMES HEATING OILS INC"
replace R_S_NAME = "SALMON RESC LTD" if R_S_NAME == "SALMON RESOURCES LTD"
replace R_S_NAME = "SASOL WAX AMERICAS INC" if R_S_NAME == "SASOL WAS AMERICAS INC" ///
| R_S_NAME == "SASOL WAX NORTH AMERICAS CORP"
replace R_S_NAME = "SEA-3 INC" if R_S_NAME == "SEA 3 INC" ///
| R_S_NAME == "SEA 3 LLC" | R_S_NAME == "SEA 3 OF FLORIDA INC" ///
| R_S_NAME == "SEA-3 OF FLORIDA INC"
replace R_S_NAME = "SEMMATERIALS" if R_S_NAME == "SEMMATERIALS LP"
replace R_S_NAME = "SIMONS PETRO INC" if R_S_NAME == "SIMONS PETROLEUM INC"
replace R_S_NAME = "SONNEBORN INC" if R_S_NAME == "SONNBORN INC"
replace R_S_NAME = "SOUND REFG INC" if R_S_NAME == "SOUND REFINING INC"
replace R_S_NAME = "SPRAGUE ENERGY CORP" if R_S_NAME == "SPRAGUE OPERATING RESRCS LLC"
replace R_S_NAME = "STAR GAS PROPANE LP" if R_S_NAME == "STAR OIL CO INC THE"
replace R_S_NAME = "STATOIL MKTG & TRDG US INC" if R_S_NAME == "STATOIL MKTG & TRDG (US) INC" ///
| R_S_NAME == "STATOIL NORTH AMERICA INC"
replace R_S_NAME = "STEPHENS ENERGY LTD" if R_S_NAME == "STEPHENS ENERGY CORP"
replace R_S_NAME = "SUNOCO INC" if R_S_NAME == "SUN CO INC" ///
| R_S_NAME == "SUNCHEM DIV OF SUN CO" | R_S_NAME == "Sunoco Inc."
replace R_S_NAME = "SUNCOR ENERGY" if R_S_NAME == "SUNCOR ENERGY (USA) INC" ///
| R_S_NAME == "SUNCOR ENERGY MKTG INC" | R_S_NAME == "SUNCOR ENERGY USAINC" ///
| R_S_NAME == "SUNCOR ENERGY USA INC" 
replace R_S_NAME = "SUPERIOR GAS LIQUIDS" if R_S_NAME == "SUPERIOR GAS LIQUIDS USA" 
replace R_S_NAME = "SWANSTON EQUIP CO" if R_S_NAME == "SWANSTON EQUIPMENT CO"
replace R_S_NAME = "TARGA MIDSTREAM SERVICES LLC" if R_S_NAME == "TARGA MIDSTREAM SERVICES LP" ///
| R_S_NAME == "TARGA MIDSTREAM SVCS LP" | R_S_NAME == "TARGA SOUND TERMINAL" 
replace R_S_NAME = "TARGRAY MARKETS INC" if R_S_NAME == "TARGRAY INDUSTRIES INC"
replace R_S_NAME = "TAUBER PETROCHEMICAL CO" if R_S_NAME == "TAUBER OIL CO"
replace R_S_NAME = "TESORO CORP" if R_S_NAME == "TESORO HAWAII CORP" ///
| R_S_NAME == "TESORO PETRO CORP" | R_S_NAME == "TESORO PETROLEUM CORP"
replace R_S_NAME = "TEXACO REFG & MKTG INC" if R_S_NAME == "TEXACO CHEM CO" ///
| R_S_NAME == "TEXACO INC" | R_S_NAME == "TEXACO INTL TRADER" ///
| R_S_NAME == "TEXACO PRODG INC" | R_S_NAME == "TEXACO TRADE & TRANSP INC" ///
| R_S_NAME == "TEXACO TRADG & TRANSP INC"
replace R_S_NAME = "TEXAS PETROCHEMS CORP" if R_S_NAME == "TEXAS PETROCHEMICALS LP"
replace R_S_NAME = "TEXPAR ENERGY LLC" if R_S_NAME == "TEXPAR ENERGY INC"
replace R_S_NAME = "TIDAL ENERGY MARKETING INC" if R_S_NAME == "TIDAL ENERGY MARKETING US LLC" ///
| R_S_NAME == "TIDAL ENERGY MKTG INC"
replace R_S_NAME = "TOSCO REFG CO" if R_S_NAME == "TOSCO CORP" | R_S_NAME == "TOSCOPETRO"
replace R_S_NAME = "TOTAL PETROCHEMICALS & REFINING USA" if R_S_NAME == "TOTAL PETRO INC" ///
| R_S_NAME == "TOTAL PETROCHEM USA INC" | R_S_NAME == "TOTAL PETROCHEMICALS USA INC"
replace R_S_NAME = "TRAMMO PETRO INC" if R_S_NAME == "TRAMMO INC" | R_S_NAME == "TRAMMOCHEM" ///
| R_S_NAME == "TRAMMOCHEM DIV OF TRANSAMMONIA INC" | R_S_NAME == "TRANSAMMONIA INC"
replace R_S_NAME = "TRANSMONTAIGNE PRODT SVCS LLC" if R_S_NAME == "TRANSMONTAIGNE PRODT SVCS INC"
replace R_S_NAME = "TRIFINERY PETRO SERV" if R_S_NAME == "TRIFINERY"
replace R_S_NAME = "ULTRAMAR ENERGY INC" if R_S_NAME == "ULTRAMAR PETRO INC"
replace R_S_NAME = "UNION CARBIDE CORP" if R_S_NAME == "UNION CARBIDE CHEM & PLASTICS"
replace R_S_NAME = "UNITED AVIATION FUEL CORP" if R_S_NAME == "UNITED AVIATION FUELS CORP"
replace R_S_NAME = "UNITED REFG CO" if R_S_NAME == "UNITED REFINING CO"
replace R_S_NAME = "UPS FREIGHT SERVICES" if R_S_NAME == "UPS SUPPLY SERVICES"
replace R_S_NAME = "US OIL & REFINING" if R_S_NAME == "US OIL & REFG CO" ///
| R_S_NAME == "US OIL & REFINING CO"
replace R_S_NAME = "VALERO MARKETING & SUPPLY CO" if R_S_NAME == "VALERO MKTG & SPLY CO - NE" ///
| R_S_NAME == "VALERO MKTG & SUPPLY CO" | R_S_NAME == "VALERO REFG & MKTG CO" ///
| R_S_NAME == "VALERO REFG CO" | R_S_NAME == "VALERO ST CHARLES REFY"
replace R_S_NAME = "VITOL S A INC" if R_S_NAME == "VITOL SA INC"
replace R_S_NAME = "VITUS LLC" if R_S_NAME == "VITUS MARINE LLC" | R_S_NAME == "VITUS ENERGY LLC"
replace R_S_NAME = "WARNER PETROLEUM CORP" if R_S_NAME == "WARNER PETRO CORP"
replace R_S_NAME = "WARREN G E" if R_S_NAME == "WARREN GE"
replace R_S_NAME = "WESTERN PETRO CO" if R_S_NAME == "WESTERN PETROLEUM CO"
replace R_S_NAME = "WESTPORT PETRO INC" if R_S_NAME == "WESTPORT PETROLEUM INC"
replace R_S_NAME = "WHITE MOUNTAIN OIL CO INC" if R_S_NAME == "WHITE MTN OIL CO INC"

replace R_S_NAME = "EXXONMOBIL OIL CORP" if R_S_NAME == "EXXONMOBIL CO USA" ///
| R_S_NAME == "EXXONMOBIL CHEMICAL CO" | R_S_NAME == "EXXONMOBIL CHEMICAL" ///
| R_S_NAME == "EXXONMOBIL CHEM CRBBN" | R_S_NAME == "EXXONMOBIL CHEM" ///
| R_S_NAME == "EXXON RESC & ENG" | R_S_NAME == "EXXON CORP" ///
| R_S_NAME == "EXXON CO USA" | R_S_NAME == "EXXON CHEM TRADG INC" ///
| R_S_NAME == "EXXON CHEM PUERTO RICO INC" | R_S_NAME == "EXXON CHEM AMER" ///
| R_S_NAME == "EXXON CHEM"

replace R_S_NAME = "KOCH INDUS INC" if R_S_NAME == "KOCH HYDROCARBON LP" ///
| R_S_NAME == "KOCH HYDROCARBONS CO" | R_S_NAME == "KOCH MATERIALS CO" ///
| R_S_NAME == "KOCH SUPPLY & TRADING CO" | R_S_NAME == "KOCH SUPPLY & TRADING LP" ///
| R_S_NAME == "KOCH SUPPLY & TRDG CO" | R_S_NAME == "KOCH SUPPLY TRADING LP" ///

replace R_S_NAME = "SHELL OIL CO" if R_S_NAME == "SHELL ANACORTES REFG CO" ///
| R_S_NAME == "SHELL CHEM LP" | R_S_NAME == "SHELL CHEM YABUCOA INC" ///
| R_S_NAME == "SHELL CHEMICAL - C4" | R_S_NAME == "SHELL CHEMICAL LP" ///
| R_S_NAME == "SHELL CHEMICAL YABUCOA INC" | R_S_NAME == "SHELL CO PUERTO RICO LTD" ///
| R_S_NAME == "SHELL GUAM INC" | R_S_NAME == "SHELL OIL CO DEER PARK" ///
| R_S_NAME == "SHELL OIL PRODTS US" | R_S_NAME == "SHELL OIL PRODUCTS US" ///
| R_S_NAME == "SHELL OIL PRODUCTS US PUGET SOUND"
replace R_S_NAME = "SHELL US TRADING CO" if R_S_NAME == "SHELL TRADING US CO" ///
| R_S_NAME == "SHELL US TRADG CO"


* Standardize PORT_CITY to be the same across different raw data files
replace PORT_CITY = "BEAUFT-MORHD, NC" if PORT_CITY == "BEAUFT-MORHD,NC"
replace PORT_CITY = "BUFF-NIAG FL, NY" if PORT_CITY == "BUFF-NIAG FL,NY"
replace PORT_CITY = "CARQUINEZ ST, CA" if PORT_CITY == "CARQUINEZ ST,CA"
replace PORT_CITY = "CHAMPL-RS PT, NY" if PORT_CITY == "CHAMPL-RS PT,NY"
replace PORT_CITY = "CHRISTIANSTD, VI" if PORT_CITY == "CHRISTIANSTD,VI"
replace PORT_CITY = "CORPUS CHRIS, TX" if PORT_CITY == "CORPUS CHRIS,TX"
replace PORT_CITY = "HONOLU/PEARL, HA" if PORT_CITY == "HONOLULU,HI"
replace PORT_CITY = "JACKSONVILLE, FL" if PORT_CITY == "JACKSONVILLE,FL"

replace PORT_CITY = "NEWPORT NEWS, VA" if PORT_CITY == "NEWPORT NEWS,VA"
replace PORT_CITY = "PORT MANATEE, FL" if PORT_CITY == "PORT MANATEE,FL"
replace PORT_CITY = "PT CANAVERAL, FL" if PORT_CITY == "PT CANAVERAL,FL"
replace PORT_CITY = "RCHMD-PETERS, VA" if PORT_CITY == "RCHMD-PETERS,VA"
replace PORT_CITY = "SAGINW-BY CT, MI" if PORT_CITY == "SAGINW-BY CT,MI"
replace PORT_CITY = "SANPABLO BAY, CA" if PORT_CITY == "SANPABLO BAY,CA"
replace PORT_CITY = "SAULT ST-MAR, MI" if PORT_CITY == "SAULT ST-MAR,MI"
replace PORT_CITY = "ST PETERSBRG, FL" if PORT_CITY == "ST PETERSBRG,FL"

replace PORT_CITY = "W PALM BEACH, FL" if PORT_CITY == "W PALM BEACH,FL"
replace PORT_CITY = "PORT MANATEE, FL" if PORT_CITY == "PORT MANATEE,FL"
replace PORT_CITY = "PT CANAVERAL, FL" if PORT_CITY == "PT CANAVERAL,FL"
replace PORT_CITY = "RCHMD-PETERS, VA" if PORT_CITY == "RCHMD-PETERS,VA"
replace PORT_CITY = "SAGINW-BY CT, MI" if PORT_CITY == "SAGINW-BY CT,MI"
replace PORT_CITY = "SANPABLO BAY, CA" if PORT_CITY == "SANPABLO BAY,CA"
replace PORT_CITY = "SAULT ST-MAR, MI" if PORT_CITY == "SAULT ST-MAR,MI"
replace PORT_CITY = "ST PETERSBRG, FL" if PORT_CITY == "ST PETERSBRG,FL"

* Standardize PCOMP_RNAM to be the same across different raw data files

replace PCOMP_RNAM = "APEX OIL CO INC" if PCOMP_RNAM == "APEX OIL"
replace PCOMP_RNAM = "ATLANTIC REFG & MKTG CORP" if PCOMP_RNAM == "ATLANTIC PETRO & MKTG CO"
replace PCOMP_RNAM = "BIG WEST OIL CO" if PCOMP_RNAM == "BIG WEST OIL LLC" ///
| PCOMP_RNAM == "BIG WEST OF CALIFORNIA LLC"
replace PCOMP_RNAM = "CENTER POINT TERMINAL LLC" if PCOMP_RNAM == "CENTER POINT TERMINAL CO" ///
| PCOMP_RNAM == "CENTER PT TERML BALTIMORE LLC"
replace PCOMP_RNAM = "CHALMETTE REFINING LLC" if PCOMP_RNAM == "CHALMETTE REFG LLC"
replace PCOMP_RNAM = "CHAMPLIN REFG CO" if PCOMP_RNAM == "CHAMPLIN PETRO CO" ///
| PCOMP_RNAM == "CHAMPLIN REFG & CHEM INC"
replace PCOMP_RNAM = "CHEMOIL TERMINALS CORP" if PCOMP_RNAM == "CHEMOIL REFG CORP" ///
| PCOMP_RNAM == "CHEMOIL TERMINAL CORP" | PCOMP_RNAM == "CHEMOIL TRMNLS CORP"
replace PCOMP_RNAM = "CLARK REFG & MKTG INC" if PCOMP_RNAM == "CLARK OIL & REFG CORP"
replace PCOMP_RNAM = "COFFEYVILLE RESRCS REFG & MKTG" if PCOMP_RNAM == "COFFEYVILLE RESOURCES LLC" ///
| PCOMP_RNAM == "COFFEYVILLE RSRCES REFG & MKTG LLC"
replace PCOMP_RNAM = "COLONIAL OIL INDUSTRIES INC" if PCOMP_RNAM == "COLONIAL TERMINALS INC"
replace PCOMP_RNAM = "DEER PARK REFINING LTD PTNRSHP" if PCOMP_RNAM == "DEER PARK REFG LTD PTNRSHP"
replace PCOMP_RNAM = "DIAMOND SHAMROCK REFG & MKTG" if PCOMP_RNAM == "DIAMOND SHAMROCK REF & MKT"
replace PCOMP_RNAM = "DOW CHEM CO" if PCOMP_RNAM == "DOW CHEM USA" ///
| PCOMP_RNAM == "DOW CHEMICAL CO"
replace PCOMP_RNAM = "EDGINGTON OIL CO INC" if PCOMP_RNAM == "EDGINGTON OIL CO"
replace PCOMP_RNAM = "ENTRPRS PRODTS OPERATING LLC" if PCOMP_RNAM == "ENTERPRISE PRODTS OPERATING LP"
replace PCOMP_RNAM = "EQUILON ENTERPRISES LLC" if PCOMP_RNAM == "EQUISTAR CHEMICALS LP"
replace PCOMP_RNAM = "FLINT HILLS RESOURCES LP" if PCOMP_RNAM == "FLINT HILL RESOURCES LP"
replace PCOMP_RNAM = "FRONTIER REFINING INC" if PCOMP_RNAM == "Frontier Refining Inc."
replace PCOMP_RNAM = "GETTY PETROLEUM MARKETING INC" if PCOMP_RNAM == "GETTY PETRO MKTG INC"
replace PCOMP_RNAM = "GIANT INDUSTRIES" if PCOMP_RNAM == "GIANT INDUSTRIES INC" ///
| PCOMP_RNAM == "GIANT YORKTOWN REFG"
replace PCOMP_RNAM = "GLOBAL PETRO CORP" if PCOMP_RNAM == "GLOBAL CO LLC" ///
| PCOMP_RNAM == "GLOBAL PETROLEUM CORP"
replace PCOMP_RNAM = "GULF OIL LP" if PCOMP_RNAM == "GULF OIL"
replace PCOMP_RNAM = "HAWAIIAN INDEP REFG INC" if PCOMP_RNAM == "HAWAII INDEPENDENT ENERGY"
replace PCOMP_RNAM = "IMTT" if PCOMP_RNAM == "IMTT BAYONNE"
replace PCOMP_RNAM = "KERN OIL & REFINING" if PCOMP_RNAM == "KERN OIL & REFG CO"
replace PCOMP_RNAM = "KOCH PETRO GROUP LP" if PCOMP_RNAM == "KOCH REFG CO" ///
| PCOMP_RNAM == "KOCH REFINING CO"
replace PCOMP_RNAM = "LA GLORIA OIL & GAS CO" if PCOMP_RNAM == "LA GLORIA  OIL & GAS CO"
replace PCOMP_RNAM = "LION OIL CO" if PCOMP_RNAM == "LION OIL CO ERGON"
replace PCOMP_RNAM = "LYONDELL CITGO REFG LP" if PCOMP_RNAM == "LYONDELL PETROCHEM CO" ///
| PCOMP_RNAM == "LYONDELL-CITGO REFINING LP"
replace PCOMP_RNAM = "MONTANA REFG CO" if PCOMP_RNAM == "MONTANA REFINING CO"
replace PCOMP_RNAM = "NATIONAL COOP REFINERY ASSOC" if PCOMP_RNAM == "NATIONAL COOP REFY ASSN"
replace PCOMP_RNAM = "NORTHVILLE INDUS CORP" if PCOMP_RNAM == "NORTHVILLE INDUSTRIES CORP"
replace PCOMP_RNAM = "NUSTAR ENERGY LP" if PCOMP_RNAM == "NUSTAR ASPHALT REFINING LLC" ///
| PCOMP_RNAM == "NUSTAR REFINING LLC"
replace PCOMP_RNAM = "OIL TANKING PL INC" if PCOMP_RNAM == "OIL TANKING HOUSTON INC" ///
| PCOMP_RNAM == "OILTANKING BEAUMONT" | PCOMP_RNAM == "OILTANKING HOUSTON LP"
replace PCOMP_RNAM = "PARAMOUNT PETROLEUM CORP" if PCOMP_RNAM == "PARAMOUNT PETRO CORP"
replace PCOMP_RNAM = "PASADENA REFINING SYSTEMS INC" if PCOMP_RNAM == "PASADENA REFG SYSTEMS INC"
replace PCOMP_RNAM = "PDV MIDWEST REFINING LLC" if PCOMP_RNAM == "PDV MIDWEST REFG LLC"
replace PCOMP_RNAM = "PETRO-DIAMOND TERMINAL CO" if PCOMP_RNAM == "PETRO-DIAMOND TERM CO"
replace PCOMP_RNAM = "PHIBRO ENERGY USA INC" if PCOMP_RNAM == "PHIBRO REFG INC"
replace PCOMP_RNAM = "PLAINS LPG SERVICES" if PCOMP_RNAM == "PLAINS LPG SERVICES LP"
replace PCOMP_RNAM = "SILVER EAGLE REFINING INC" if PCOMP_RNAM == "SILVER EAGLE REFG"
replace PCOMP_RNAM = "ST SERVICES" if PCOMP_RNAM == "ST SERVICE SHORE TERMINALS" ///
| PCOMP_RNAM == "ST SERVICES SHORE TERMINALS LLC"
replace PCOMP_RNAM = "SUPPORT TERMLS OPER PRTNSHP LP" if PCOMP_RNAM == "SUPPORT TRMNLS OPER PTNRSHP LP"
replace PCOMP_RNAM = "TARGA SOUND TERMINAL" if PCOMP_RNAM == "TARGA RESOURCES" ///
| PCOMP_RNAM == "TARGA MIDSTREAM SERVICES LLC"
replace PCOMP_RNAM = "TEPPCO TERMINAL & MKTG CO LLC" if PCOMP_RNAM == "TEPPCO CRUDE PL"
replace PCOMP_RNAM = "TOTAL PETROCHEMICALS & REFINING USA" if PCOMP_RNAM == "TOTAL PETRO INC" ///
| PCOMP_RNAM == "TOTAL PETRO OF PUERTO RICO CORP" | PCOMP_RNAM == "TOTAL PETROCHEMICALS USA INC"
replace PCOMP_RNAM = "TRANSAMERICAN REFG CO" if PCOMP_RNAM == "TRANSAMERICAN REFG CORP"
replace PCOMP_RNAM = "TRANSMONTAIGNE INC" if PCOMP_RNAM == "TRANSMONTAIGNE OIL CO" ///
| PCOMP_RNAM == "TRANSMONTAIGNE PRODT SVCS INC"
replace PCOMP_RNAM = "TRIFINERY PETRO SERV" if PCOMP_RNAM == "TRIFINERY"
replace PCOMP_RNAM = "TRIGEANT LTD" if PCOMP_RNAM == "TRIGEANT EP LTD"
replace PCOMP_RNAM = "ULTRAMAR INC" if PCOMP_RNAM == "ULTRAMAR REFG"
replace PCOMP_RNAM = "US OIL & REFINING CO" if PCOMP_RNAM == "US OIL & REFG CO"
replace PCOMP_RNAM = "WESTERN REFINING" if PCOMP_RNAM == "WESTERN REFINING YORKTOWN INC" ///
| PCOMP_RNAM == "WESTERN REFINING SOUTHWEST INC"
replace PCOMP_RNAM = "WILLIAMS RFNG & MKTG LLC" if PCOMP_RNAM == "WILLIAMS REFG LLC" ///
| PCOMP_RNAM == "WILLIAMS REFG & MKTG LLC"

replace PCOMP_RNAM = "BP" if substr(PCOMP_RNAM, 1, 2) == "BP"
replace PCOMP_RNAM = "BUCKEYE PARTNERS" if substr(PCOMP_RNAM, 1, 7) == "BUCKEYE"
replace PCOMP_RNAM = "CITGO" if substr(PCOMP_RNAM, 1, 5) == "CITGO"
replace PCOMP_RNAM = "COSTAL CORP" if substr(PCOMP_RNAM, 1, 7) == "COASTAL"
replace PCOMP_RNAM = "CONOCO INC" if substr(PCOMP_RNAM, 1, 6) == "CONOCO"
replace PCOMP_RNAM = "FRONTIER OIL" if substr(PCOMP_RNAM, 1, 8) == "FRONTIER"
replace PCOMP_RNAM = "HOLLY CORP" if substr(PCOMP_RNAM, 1, 5) == "HOLLY"
replace PCOMP_RNAM = "HUNT OIL" if substr(PCOMP_RNAM, 1, 4) == "HUNT"
replace PCOMP_RNAM = "INTERCONTINENTAL OIL" if substr(PCOMP_RNAM, 1, 16) == "INTERCONTINENTAL"
replace PCOMP_RNAM = "KINDER MORGAN" if substr(PCOMP_RNAM, 1, 2) == "KM" ///
| substr(PCOMP_RNAM, 1, 13) == "KINDER MORGAN"
replace PCOMP_RNAM = "LBC TANK TERMINALS" if substr(PCOMP_RNAM, 1, 3) == "LBC"
replace PCOMP_RNAM = "MAGELLAN MIDSTREAM PARTNERS" if substr(PCOMP_RNAM, 1, 8) == "MAGELLAN"
replace PCOMP_RNAM = "MARATHON OIL" if substr(PCOMP_RNAM, 1, 8) == "MARATHON"
replace PCOMP_RNAM = "PHILLIPS 66" if substr(PCOMP_RNAM, 1, 11) == "PHILLIPS 66"
replace PCOMP_RNAM = "SHELL" if substr(PCOMP_RNAM, 1, 5) == "SHELL"
replace PCOMP_RNAM = "SINCLAIR" if substr(PCOMP_RNAM, 1, 8) == "SINCLAIR"
replace PCOMP_RNAM = "SUNCOR ENERGY" if substr(PCOMP_RNAM, 1, 6) == "SUNCOR"
replace PCOMP_RNAM = "TESORO" if substr(PCOMP_RNAM, 1, 6) == "TESORO"
replace PCOMP_RNAM = "TOSCO CORP" if substr(PCOMP_RNAM, 1, 5) == "TOSCO"
replace PCOMP_RNAM = "UNKNOWN PROCESSOR" if substr(PCOMP_RNAM, 1, 17) == "UNKNOWN PROCESSOR"
replace PCOMP_RNAM = "VALERO ENERGY" if substr(PCOMP_RNAM, 1, 6) == "VALERO"

* Standardize PROD_NAME
replace PROD_NAME = strupper(PROD_NAME)

* Order the sort the variables
order PORT_PADD YEAR MONTH DATE PROD_NAME
sort PORT_PADD YEAR MONTH DATE PROD_NAME


*************** END **********************
save "$outdir/CompanyImports_renamed", replace

capture log close
exit
