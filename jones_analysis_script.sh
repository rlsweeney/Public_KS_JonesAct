#!/bin/sh

#-------------------------------------------------------------------------------
# Name:        jones_analysis_script.sh
# Purpose:     Runs all scripts that carry out our quantitative analysis,
#	       including estimation and simulation of counterfactual
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
# stataSE-64 -e do $CODEDIR/Argus/AnalyzeArgusData.do
#
# INPUTS
# DBDIR/intdata/Argus/ArgusData.dta
#
# OUTPUTS
# REPODIR/output/figures/NormalizedFreightRates.pdf
# REPODIR/output/figures/PredictedFreightRates_NYCTAM_clean.pdf
# REPODIR/output/figures/PredictedFreightRates_NYCTAM_dirty.pdf
# DBDIR/intdata/Argus/PredictedFreightRates.csv
# REPODIR/output/tex_numbers/*.tex (many single number tex files)
#
#-------------------------------------------------------------------------------
# stataSE-64 -e do $CODEDIR/padd1c_portshares.do
#
# INPUTS
# DBDIR/intdata/ArmyCorps/ArmyCorpsData.dta
# DBDIR/intdata/EIACompanyLevelImports/CompanyImports_renamed.dta
#
# OUTPUTS
# REPODIR/output/tables/Padd1cportshares.tex
#
#-------------------------------------------------------------------------------
# Rscript $CODEDIR/clean_main_data.R
#
# INPUTS
# DBDIR/intdata/Bloomberg/Crude/bbg_indices_crude_monthly.RDS
# DBDIR/rawdata/data/EIA_API_Output/July2023/EIASpotPrices.RDS
# DBDIR/rawdata/data/EIA_API_Output/July2023/EIAPrimeSupplierSalesVol.RDS
# DBDIR/rawdata/data/EIA_API_Output/July2023/EIAPaddExportsDestination.RDS
# DBDIR/rawdata/data/EIA_API_Output/July2023/EIAMovementsTankerBarge.RDS
# DBDIR/intdata/EIARefineryInputs/EIA_RefineryInputs.RDS
# DBDIR/intdata/EIACompanyLevelImports/CompanyImports_renamed.dta
#
# OUTPUTS
# DBDIR/intdata/clean_eia_spot_prices.RDS
# DBDIR/intdata/clean_prices_bbg.RDS
# DBDIR/intdata/combined_subpadd_volumes.RDS
# DBDIR/intdata/main_padd3_df.RDS
#
#-------------------------------------------------------------------------------
# Rscript $CODEDIR/price_figures.R
#
# INPUTS
# DBDIR/intdata/clean_prices_bbg.RDS
# DBDIR/intdata/PredictedFreightRates.csv
# DBDIR/intdata/clean_eia_spot_prices.RDS
#
# OUTPUTS
# REPODIR/output/figures/pricediffs_arugsrates.png
# REPODIR/output/figures/pricespread_argus_crude.png
# REPODIR/output/figures/pricespread_argus_jet.png
# REPODIR/output/figures/pricespread_argus_conv.png
# REPODIR/output/figures/pricespread_argus_ULSD.png
# REPODIR/output/figures/pricespread_long_crude.png
# REPODIR/output/figures/pricespread_long_jet.png
# REPODIR/output/figures/pricespread_long_conv.png
# REPODIR/output/figures/pricespread_long_ULSD.png
# REPODIR/output/figures/pricediffs_eia_long.png
# REPODIR/output/figures/pricediffs_spread_eia.png
# REPODIR/output/tex_numbers/*.tex (many single number tex files)
#
#-------------------------------------------------------------------------------
# Rscript $CODEDIR/flowtable.R
#
# INPUTS
# DBDIR/intdata/combined_subpadd_volumes.RDS
# DBDIR/intdata/main_padd3_df.RDS
#
# OUTPUTS
# REPODIR/output/tables/flows_summary.tex
# REPODIR/output/tex_numbers/totalECimports.tex
# REPODIR/output/tex_numbers/totalGCexports.tex
#
#-------------------------------------------------------------------------------
# Rscript $CODEDIR/counterfactuals.R
#
# INPUTS
# DBDIR/intdata/clean_prices_bbg.RDS
# DBDIR/intdata/PredictedFreightRates.csv
# DBDIR/intdata/combined_subpadd_volumes.RDS
# DBDIR/intdata/main_padd3_df.RDS
#
# OUTPUTS
# REPODIR/output/figures/subpadd_counterfactual_gas.png
# REPODIR/output/figures/subpadd_counterfactual_jet.png
# REPODIR/output/figures/subpadd_counterfactual_ulsd.png
# REPODIR/output/figures/padd1_counterfactual_12panel.png
# REPODIR/output/figures/padd1_counterfactual_12panel.png
# REPODIR/output/tables/counterfactual_flows_summary.tex
# REPODIR/output/tables/counterfactual_efficiency.tex
# REPODIR/output/tables/counterfactual_price.tex
# REPODIR/output/tables/counterfactual_cs.tex
# REPODIR/output/tables/counterfactual_summary.tex
# REPODIR/output/tables/counterfactual_efficiency_port_ev.tex
# REPODIR/output/tables/counterfactual_price_port_ev.tex
# REPODIR/output/tables/counterfactual_cs_port_ev.tex
# REPODIR/output/tables/counterfactual_summary_port_ev.tex
# REPODIR/output/tex_numbers/cf/*.tex (many single number tex files)
#
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# EXECUTION COMMANDS
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

# Define path to code/analysis subfolder based on $REPODIR variable
# exported from jones_bash_file.sh
CODEDIR=$REPODIR/code/analysis

if [ "$OS" = "Unix" ]; then
    if [ "$STATA" = "SE" ]; then
      stata-se -e do $CODEDIR/Argus/AnalyzeArgusData.do &&
      stata-se -e do $CODEDIR/padd1c_portshares.do
    elif [ "$STATA" = "MP" ]; then
      stata-mp -e do $CODEDIR/Argus/AnalyzeArgusData.do &&
      stata-mp do $CODEDIR/padd1c_portshares.do
    fi
elif [ "$OS" = "Windows" ]; then
    if [ "$STATA" = "SE" ]; then
      stataSE-64 -e do $CODEDIR/Argus/AnalyzeArgusData.do &&
      stataSE-64 -e do $CODEDIR/padd1c_portshares.do
    elif [ "$STATA" = "MP" ]; then
      stataMP-64 -e do $CODEDIR/Argus/AnalyzeArgusData.do &&
      stataMP-64 -e do $CODEDIR/padd1c_portshares.do
    fi
fi

Rscript $CODEDIR/clean_main_data.R
Rscript $CODEDIR/price_figures.R
Rscript $CODEDIR/flowtable.R
Rscript $CODEDIR/counterfactuals.R

# Clean up log files
rm *.log

exit
