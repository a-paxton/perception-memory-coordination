#### libraries_and_functions-pmc.r: Part of `perception-memory-coordination.Rmd` ####
#
# This script loads libraries and creates a number of 
# additional functions to facilitate data prep and analysis.
#
# The script `required_packages-pmc.r` should be run once first
# to ensure that the all required packages have been installed.
#
# Written by: A. Paxton (University of California, Berkeley)
# Date last modified: 19 October 2017
#####################################################################################

#### Load necessary packages ####

# list of required packages
required_packages = c(
  'plyr',
  'dplyr',
  'stringr',
  'data.table',
  'lme4',
  'ggplot2',
  'pander',
  'gridExtra',
  'plotrix',
  'gtable',
  'viridis',
  'jsonlite',
  'lubridate'
)

# load required packages
invisible(lapply(required_packages, require, character.only = TRUE))
