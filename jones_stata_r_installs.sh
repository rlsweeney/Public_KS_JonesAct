#!/bin/sh

#-------------------------------------------------------------------------------
# Name:        jones_stata_r_installs.sh
# Purpose:     Installs Stata packages and the R "here" package
#	       All other R packages are checked for and installed via
#	       $REPODIR/code/basic_setup.R, which is called at the start
#	       of each R script
#
# Created:     8 Novebmer, 2023
#-------------------------------------------------------------------------------

# Define path to code subfolder based on $REPODIR variable
# exported from jones_bash_file.sh
CODEDIR=$REPODIR/code

# Stata installs
if [ "$OS" = "Unix" ]; then
    if [ "$STATA" = "SE" ]; then
      stata-se -e do $CODEDIR/stata_installs.do
    elif [ "$STATA" = "MP" ]; then
      stata-mp -e do $CODEDIR/stata_installs.do
    fi
elif [ "$OS" = "Windows" ]; then
    if [ "$STATA" = "SE" ]; then
      stataSE-64 -e do $CODEDIR/stata_installs.do
    elif [ "$STATA" = "MP" ]; then
      stataMP-64 -e do $CODEDIR/stata_installs.do
    fi
fi


# Install R "here" package
R -e "install.packages('here', repos = 'https://cran.rstudio.com/', type = 'binary')"


# Clean up log files
rm *.log

exit
