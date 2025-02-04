#!/bin/sh

#-------------------------------------------------------------------------------
# Name:        jones_build_script.sh
# Purpose:     Runs scripts that import raw data and perform cleaning / merging
#	       operations to prepare data for analysis.
#              Does not re-run the EIA API pull in $CODEDIR/EIA_API_Output/run_eia_api_v2.R
#              That script is commented out below.
#
# Created:     8 November, 2023
#-------------------------------------------------------------------------------

# THIS FILE BEGINS WITH DETAILED DESCRIPTIONS OF EACH SCRIPT
# EXECUTION COMMANDS FOLLOW

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# LIST OF SCRIPTS IN ORDER, LISTING ALL INPUTS AND OUTPUTS
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# stataSE-64 -e do $CODEDIR/Argus/LoadArgusData.do
#
# INPUTS
# DBDIR/rawdata/orig/Argus/argus dirty freight historical_2018-2019.xls
# DBDIR/rawdata/orig/Argus/argus clean freight historical_2018-2019_1.xls.xls
# DBDIR/rawdata/orig/Argus/argus clean freight historical_2018-2019_2.xls.xls
#
# OUTPUTS
# DBDIR/intdata/Argus/ArgusData.dta
#
# ------------------------------------------------------
# stataSE-64 -e do $CODEDIR/ArmyCorps/LoadArmyCorpsData.do
#
# INPUTS
# DBDIR/rawdata/orig/ArmyCorps/5YearCargoReport-Domestic-TAM.csv
# DBDIR/rawdata/orig/ArmyCorps/5YearCargoReport-Domestic-PortEverglades.csv
# DBDIR/rawdata/orig/ArmyCorps/5YearCargoReport-Domestic-Canaveral.csv
# DBDIR/rawdata/orig/ArmyCorps/5YearCargoReport-Domestic-SavannahGA.csv
# DBDIR/rawdata/orig/ArmyCorps/5YearCargoReport-Domestic-CharlestonSC.csv
#
# OUTPUTS
# DBDIR/intdata/ArmyCorps/ArmyCorpsData.dta
#
# ------------------------------------------------------
# stataSE-64 -e do $CODEDIR/ArmyCorps/CleanCompanyImports.do
#
# INPUTS
# REPODIR/code/inputs/PADDMappings/PADD_Mappings.csv
# DBDIR/rawdata/orig/EIACompanyLevelImports/impa**d.xls (many files for different years)
# DBDIR/rawdata/orig/EIACompanyLevelImports/20**_**.xlsx (many files for different years)
#
# OUTPUTS
# DBDIR/intdata/EIACompanyLevelImports/CompanyImports.dta
#
# ------------------------------------------------------
# stataSE-64 -e do $CODEDIR/ArmyCorps/CleanCompanyImports_rename.do
#
# INPUTS
# DBDIR/intdata/EIACompanyLevelImports/CompanyImports.dta
#
# OUTPUTS
# DBDIR/intdata/EIACompanyLevelImports/CompanyImports_renamed.dta
#
# ------------------------------------------------------
# stataSE-64 -e do $CODEDIR/EIATerritories/CleanEITerritories.do
#
# INPUTS
# DBDIR/rawdata/orig/EIATerritories/INT-Export-07-17-2023_14-02-51.csv
#
# OUTPUTS
# DBDIR/intdata/EIATerritories/EIATerritories.csv
# REPODIR/output/tex_numbers/PRVI_gas.tex
# REPODIR/output/tex_numbers/PRVI_jet.tex
# REPODIR/output/tex_numbers/PRVI_ulsd.tex
#
# ------------------------------------------------------
# Rscript $CODEDIR/Bloomberg/import_bbg.R
#
# INPUTS
# DBDIR/raw_data/orig/Bloomberg/Crude/bbg_indices2023.xlsx
#
# OUTPUTS
# DBDIR/intdata/Bloomberg/Crude/bbg_indices_crude_monthly.RDS
#
# ------------------------------------------------------
# Rscript $CODEDIR/EIARefineryInputs/CleanRefineryInputs.R
#
# INPUTS
# DBDIR/rawdata/orig/EIARefineryInputs/PET_PNP_UNC_A_EPXXX2_YIY_MBBLPD_M_20230613.xls
#
# OUTPUTS
# DBDIR/intdata/EIARefineryInputs/EIA_RefineryInputs.RDS
#
# ------------------------------------------------------
# Rscript $CODEDIR/EIA_API_Output/run_eia_api_v2.R
#
# This file is NOT included in the execution scripts below
# If executed, the four save commands are currently commented out
# Uncommenting the save commands and executing the script will execute
# a new pull from the EIA API and over-write the four output files,
# unless `outdir` is changed on line 25
#
# INPUTS
# No file inputs; this script pulls data from the EIA API
#
# OUTPUTS
# DBDIR/rawdata/data/EIA_API_Output/July2023/EIASpotPrices.RDS
# DBDIR/rawdata/data/EIA_API_Output/July2023/EIAPrimeSupplierSalesVol.RDS
# DBDIR/rawdata/data/EIA_API_Output/July2023/EIAPaddExportsDestination.RDS
# DBDIR/rawdata/data/EIA_API_Output/July2023/EIAMovementsTankerBarge.RDS
#
# ------------------------------------------------------

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# EXECUTION COMMANDS
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

# Define path to code subfolder based on $REPODIR variable
# exported from jones_bash_file.sh
CODEDIR=$REPODIR/code/build

if [ "$OS" = "Unix" ]; then
    if [ "$STATA" = "SE" ]; then
      stata-se -e do $CODEDIR/Argus/LoadArgusData.do &&
      stata-se -e do $CODEDIR/ArmyCorps/LoadArmyCorpsData.do &&
      stata-se -e do $CODEDIR/EIACompanyLevelImports/CleanCompanyImports.do &&
      stata-se -e do $CODEDIR/EIACompanyLevelImports/CleanCompanyImports_rename.do &&
      stata-se -e do $CODEDIR/EIATerritories/CleanEIATerritories.do
    elif [ "$STATA" = "MP" ]; then
      stata-mp -e do $CODEDIR/Argus/LoadArgusData.do &&
      stata-mp -e do $CODEDIR/ArmyCorps/LoadArmyCorpsData.do &&
      stata-mp -e do $CODEDIR/EIACompanyLevelImports/CleanCompanyImports.do &&
      stata-mp -e do $CODEDIR/EIACompanyLevelImports/CleanCompanyImports_rename.do &&
      stata-mp -e do $CODEDIR/EIATerritories/CleanEIATerritories.do
    fi
elif [ "$OS" = "Windows" ]; then
    if [ "$STATA" = "SE" ]; then
      stataSE-64 -e do $CODEDIR/Argus/LoadArgusData.do &&
      stataSE-64 -e do $CODEDIR/ArmyCorps/LoadArmyCorpsData.do &&
      stataSE-64 -e do $CODEDIR/EIACompanyLevelImports/CleanCompanyImports.do &&
      stataSE-64 -e do $CODEDIR/EIACompanyLevelImports/CleanCompanyImports_rename.do &&
      stataSE-64 -e do $CODEDIR/EIATerritories/CleanEIATerritories.do
    elif [ "$STATA" = "MP" ]; then
      stataMP-64 -e do $CODEDIR/Argus/LoadArgusData.do &&
      stataMP-64 -e do $CODEDIR/ArmyCorps/LoadArmyCorpsData.do &&
      stataMP-64 -e do $CODEDIR/EIACompanyLevelImports/CleanCompanyImports.do &&
      stataMP-64 -e do $CODEDIR/EIACompanyLevelImports/CleanCompanyImports_rename.do &&
      stataMP-64 -e do $CODEDIR/EIATerritories/CleanEIATerritories.do
    fi
fi

Rscript $CODEDIR/Bloomberg/import_bbg.R
Rscript $CODEDIR/EIARefineryInputs/CleanRefineryInputs.R
# Rscript $CODEDIR/EIA_API_Output/run_eia_api_v2.R

# Clean up log files
rm *.log

exit
