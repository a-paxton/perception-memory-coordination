#### required_packages-pmc.r: Part of `perception-memory-coordination.Rmd` ####
#
# This script downloads packages required by data preparation and analysis.
# Run this prior to any other scripts.
#
# Written by: A. Paxton (University of California, Berkeley)
# Date last modified: 26 January 2017
#####################################################################################

# list of required packages as strings
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
  'lubridate',
  'tidyr',
  'tibble',
  'devtools',
  'xtable',
  'RCurl',
  'broom'
)

# install missing packages (adapted from <http://stackoverflow.com/a/4090208>)
missing_packages = required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}

# install the correct template for the CogSci proceedings
devtools::install_github("kemacdonald/cogsci2016")
