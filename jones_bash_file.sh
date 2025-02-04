#!/bin/sh

#-------------------------------------------------------------------------------
# Name:        jones_bash_file.sh
# Purpose:     Calls every file from raw data import through final simulations,
#              and compilation of the paper
#
# Created:     8 November, 2023
#-------------------------------------------------------------------------------


# ---------------------------------------------------------------------------
# Execution
# Users can run this script by opening a bash/unix shell, changing the directory
# to their local repository ($REPODIR below), and then using this command:
#
# bash -x jones_bash_file.sh |& tee jones_bash_file_out.txt
#
# ---------------------------------------------------------------------------


# USER INPUT NEEDED HERE FOR ENVIRONMENT VARIABLES:
# USER-SPECIFIC PATHS TO THIS SCRIPT'S DIRECTORY AND THE DATA DIRECTORY
# USER-SPECIFIC OPERATING SYSTEMS AND STATA VERSION
# "OS" SHOULD BE "Windows" OR "Unix"
# "STATA" SHOULD BE "SE" OR "MP"
if [ "$HOME" = "/c/Users/kelloggr" ]; then
        REPODIR=C:/Work/JonesAct
        DBDIR="C:/Users/kelloggr/Dropbox/JonesAct"
        OS="Windows"
    	STATA="SE"
elif [ "$HOME" = "PATHTORICH" ]; then
        REPODIR=C:/Work/JonesAct
        DBDIR="C:/Users/kelloggr/Dropbox/JonesAct"
        OS="Unix"
    	STATA="SE"
fi


# EXPORT VARIABLES TO SUBSCRIPTS
export REPODIR
export DBDIR
export OS
export STATA


# STATA INSTALLS AND R "here" PACKAGE INSTALL
# UNCOMMENT THE LINE BELOW IF PACKAGES NEED TO BE INSTALLED
# bash -x $REPODIR/jones_stata_r_installs.sh |& tee jones_stata_r_installs_out.txt

# CLEAR INTERMEDIATE DATA AND OUTPUT FOLDERS
bash -x $REPODIR/jones_clear_folders.sh |& tee jones_clear_folders_out.txt

# RUN THE BUILD
bash -x $REPODIR/jones_build_script.sh |& tee jones_build_script_out.txt

# RUN THE ANALYSIS SCRIPTS
bash -x $REPODIR/jones_analysis_script.sh |& tee jones_analysis_script_out.txt

# COMPILE THE PAPER
bash -x $REPODIR/jones_paper_script.sh |& tee jones_paper_script_out.txt

#clean up log files
rm *.log

exit

