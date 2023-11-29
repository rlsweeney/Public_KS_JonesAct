#!/bin/sh

#-------------------------------------------------------------------------------
# Name:        jones_paper_script.sh
# Purpose:     Compiles the paper. LaTex code inputs figures, tables, and single
#	       number tex files from subfolders in JonesAct/output
#
# Created:     8 November, 2023
#-------------------------------------------------------------------------------

# Define path to paper subfolder based on $REPODIR variable
# exported from jones_bash_file.sh
CODEDIR=$REPODIR/paper


# COMPILE PAPER
cd $CODEDIR
pdflatex -output-directory=$CODEDIR KS_JonesAct.tex
bibtex KS_JonesAct
pdflatex -output-directory=$CODEDIR KS_JonesAct.tex
pdflatex -output-directory=$CODEDIR KS_JonesAct.tex

# COMPILE APPENDIX
pdflatex -output-directory=$CODEDIR KS_JonesAct_appx.tex
bibtex KS_JonesAct_appx
pdflatex -output-directory=$CODEDIR KS_JonesAct_appx.tex
pdflatex -output-directory=$CODEDIR KS_JonesAct_appx.tex

# GET CROSS REFERENCES BETWEEN PAPER AND APPX
pdflatex -output-directory=$CODEDIR KS_JonesAct.tex
pdflatex -output-directory=$CODEDIR KS_JonesAct_appx.tex

#clean up log files
rm *.log

exit
