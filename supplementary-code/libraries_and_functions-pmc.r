#### libraries_and_functions-pmc.r: Part of `perception-memory-coordination.Rmd` ####
#
# This script loads libraries and creates a number of 
# additional functions to facilitate data prep and analysis.
#
# Written by: A. Paxton (University of California, Berkeley)
# Date last modified: 26 January 2017
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
  'lubridate',
  'tidyr',
  'tibble',
  'xtable',
  'RCurl'
)

# load required packages (thanks to https://gist.github.com/smithdanielle/9913897)
check.packages <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, 
                     dependencies = TRUE,
                     repos="http://cloud.r-project.org/")
  sapply(pkg, require, character.only = TRUE)
}
check.packages(required_packages)

#### Prevent scientific notation ####
options(scipen=999)

#### Create useful global variables ####

# identify desired max lag
ccf_max_lag = 5

# get a list of named questionnaire variables
questionnaire_variables = c("cooperative_partner",
                            "cooperative_self",
                            "trust_partner",
                            "trust_self",
                            "engagement",
                            "difficulty")

#### Crib other folks' functions #### 

#' Adapted from rmd2rscript: script for converting .Rmd files to .R scripts
#' 
#' Thanks to Kevin Keenan:
#' http://rstudio-pubs-static.s3.amazonaws.com/12734_0a38887f19a34d92b7311a2c9cb15022.html
#' 
#' This function will read a standard R markdown source file and convert it to 
#' an R script to allow the code to be run using the "source" function.
#' 
#' The function is quite simplisting in that it reads a .Rmd file and adds 
#' comments to non-r code sections, while leaving R code without comments
#' so that the interpreter can run the commands.

rmd2rscript <- function(infile, outname){
  
  # read the file
  flIn <- readLines(infile)
  
  # identify the start of code blocks
  cdStrt <- which(grepl(flIn, pattern = "```{r*", perl = TRUE))
  
  # identify the end of code blocks
  cdEnd <- sapply(cdStrt, function(x){
    preidx <- which(grepl(flIn[-(1:x)], pattern = "```", perl = TRUE))[1]
    return(preidx + x)
  })
  
  # define an expansion function
  # strip code block indacators
  flIn[c(cdStrt, cdEnd)] <- ""
  expFun <- function(strt, End){
    strt <- strt+1
    End <- End-1
    return(strt:End)
  }
  idx <- unlist(mapply(FUN = expFun, strt = cdStrt, End = cdEnd, 
                       SIMPLIFY = FALSE))
  
  # add comments to all lines except code blocks
  comIdx <- 1:length(flIn)
  comIdx <- comIdx[-idx]
  for(i in comIdx){
    flIn[i] <- paste("#' ", flIn[i], sep = "")
  }
  
  # create an output file
  flOut <- file(paste(outname, "[rmd2r].R", sep = ""), "w")
  for(i in 1:length(flIn)){
    cat(flIn[i], "\n", file = flOut, sep = "\t")
  }
  close(flOut)
}

#### Create functions we'll need ####

# read in output printing at the correct commit
# thanks to https://github.com/opetchey/RREEBES/wiki/
xtable_lme_url = "https://raw.githubusercontent.com/a-paxton/stats-tools/bee546f2c959cb6a5b9cad1f28d3afbae6e46c41/xtable_lme.r"
xtable_lme_file = getURL(xtable_lme_url, ssl.verifypeer = FALSE)
eval(parse(text = xtable_lme_file))

