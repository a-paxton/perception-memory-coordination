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
  'xtable'
)

# load required packages
invisible(lapply(required_packages, require, character.only = TRUE))

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

# "pander_lme": simplify lme4 printouts (available on GitHub: https://github.com/a-paxton/stats-tools)
pander_lme = function(lme_model_name, stats.caption){
  
  # load in pander
  library(pander)
  library(dplyr)
  
  # disable scientific notation
  options(scipen = 999)
  
  # convert the model summary to a dataframe
  neat_output = data.frame(summary(lme_model_name)$coefficient)
  
  # round p-values (using Psychological Science's recommendations)
  neat_output$p = 2*(1-pnorm(abs(neat_output$t.value)))
  neat_output$p[neat_output$p < .0005] = round(neat_output$p[neat_output$p < .0005],4)
  neat_output$p[neat_output$p >= .0005] = round(neat_output$p[neat_output$p >= .0005],3)
  neat_output$p[neat_output$p >= .25] = round(neat_output$p[neat_output$p >= .25],2)
  
  # create significance and trending markers
  neat_output$sig = ' '
  neat_output$sig[neat_output$p < .1] = '.'
  neat_output$sig[neat_output$p < .05] = '*'
  neat_output$sig[neat_output$p < .01] = '**'
  neat_output$sig[neat_output$p < .001] = '***'
  
  neat_output = select(neat_output, -Std..Error)
  
  # fix names
  setDT(neat_output, keep.rownames = TRUE)[]
  names(neat_output)[1] = "Variable"
  names(neat_output)[2] = "Estimate"
  names(neat_output)[3] = "t-value"
  names(neat_output)[4] = "p-value"
  names(neat_output)[5] = "Sig."
  
  # set a caption that includes R-squared values
  if (stats.caption == TRUE){
    
    # use MuMIN to calculate R-squared
    library(MuMIn)
    model_marginal_r_squared = r.squaredGLMM(lme_model_name)[['R2m']]
    model_conditional_r_squared = r.squaredGLMM(lme_model_name)[['R2c']]
    neat_caption = paste('**Marginal *R*-squared: ',
                         round(model_marginal_r_squared,2), 
                         ". Conditional *R*-squared: ",
                         round(model_conditional_r_squared,2),".**",sep="")
    
    # return the table
    return(pander(neat_output, split.table = Inf, caption = neat_caption, style = 'rmarkdown'))
    
  } else { # or return a table without it
    return(pander(neat_output, split.table = Inf, style = 'rmarkdown'))
  }
}

# "xtable_lme": simplify lme4 printouts (available on GitHub: https://github.com/a-paxton/stats-tools)
xtable_lme = function(lme_model_name){
  
  # load in pander
  library(pander)
  library(dplyr)
  
  # disable scientific notation
  options(scipen = 999)
  
  # convert the model summary to a dataframe
  neat_output = data.frame(summary(lme_model_name)$coefficient)
  
  # round p-values (using Psychological Science's recommendations)
  neat_output$p = 2*(1-pnorm(abs(neat_output$t.value)))
  neat_output$p[neat_output$p < .0005] = round(neat_output$p[neat_output$p < .0005],4)
  neat_output$p[neat_output$p >= .0005] = round(neat_output$p[neat_output$p >= .0005],3)
  neat_output$p[neat_output$p >= .25] = round(neat_output$p[neat_output$p >= .25],2)
  
  # create significance and trending markers
  neat_output$sig = ' '
  neat_output$sig[neat_output$p < .1] = '.'
  neat_output$sig[neat_output$p < .05] = '*'
  neat_output$sig[neat_output$p < .01] = '**'
  neat_output$sig[neat_output$p < .001] = '***'
  
  neat_output = select(neat_output, -Std..Error, -t.value)
  
  # fix names
  setDT(neat_output, keep.rownames = TRUE)[]
  names(neat_output)[1] = ""
  names(neat_output)[2] = "Estimate"
  names(neat_output)[3] = "p-value"
  names(neat_output)[4] = "Sig."
  
  # return the table
  return(neat_output)
}

##

# "pander_lme_to_latex": Export an LMER summary table to a latex file

pander_lme_to_latex = function(lme_model_name, save_filename){
  
  # load in pander
  require(pander)
  require(Hmisc)
  require(plyr)
  require(dplyr)
  
  # disable scientific notation
  options(scipen = 999)
  
  # convert the model summary to a dataframe
  neat_output = data.frame(summary(lme_model_name)$coefficient)
  
  # round p-values (using Psychological Science's recommendations)
  neat_output$p = 2*(1-pnorm(abs(neat_output$t.value)))
  neat_output$p[neat_output$p < .0005] = round(neat_output$p[neat_output$p < .0005],4)
  neat_output$p[neat_output$p >= .0005] = round(neat_output$p[neat_output$p >= .0005],3)
  neat_output$p[neat_output$p >= .25] = round(neat_output$p[neat_output$p >= .25],2)
  
  # round the estimates, standard error, and t-values
  neat_output$"t-value" = round(neat_output$t.value,3)
  neat_output$"Std. Error" = round(neat_output$Std..Error,3)
  neat_output$Estimate = round(neat_output$Estimate,3)
  
  # create a new column for variable names
  neat_output$Predictor = row.names(neat_output)
  rownames(neat_output) = NULL
  neat_output = neat_output[,c(7,1,6,5,4)]
  
  # create significance and trending markers
  neat_output$"Sig." = ' '
  neat_output$"Sig."[neat_output$p < .1] = '.'
  neat_output$"Sig."[neat_output$p < .05] = '*'
  neat_output$"Sig."[neat_output$p < .01] = '**'
  neat_output$"Sig."[neat_output$p < .001] = '***'
  neat_output = plyr::rename(neat_output,c("p" = 'p-value'))
  
  # save to file
  latex(neat_output,file=save_filename,rownamesTexCmd=NULL)
}

##

# "pander_lm": simplify lm printouts and include adjusted R-squared and F-stats
pander_lm = function(lm_model_name, stats.caption){
  
  # load in pander
  library(pander)
  library(data.table)
  
  # disable scientific notation
  options(scipen = 999)
  
  # convert the model summary to a dataframe
  neat_output = data.frame(summary(lm_model_name)$coefficient)
  
  # round p-values (using Psychological Science's recommendations)
  neat_output$p = 2*(1-pnorm(abs(neat_output$t.value)))
  neat_output$p[neat_output$p < .0005] = round(neat_output$p[neat_output$p < .0005],4)
  neat_output$p[neat_output$p >= .0005] = round(neat_output$p[neat_output$p >= .0005],3)
  neat_output$p[neat_output$p >= .25] = round(neat_output$p[neat_output$p >= .25],2)
  
  # create significance and trending markers
  neat_output$sig = ' '
  neat_output$sig[neat_output$p < .15] = '.'
  neat_output$sig[neat_output$p < .05] = '*'
  neat_output$sig[neat_output$p < .01] = '**'
  neat_output$sig[neat_output$p < .001] = '***'
  
  # set a caption that includes R-squared values
  if (stats.caption==TRUE){
    
    # grab stats F-stats and adjusted R-squared
    model_adj_r_squared = summary(lm_model_name)$adj.r.squared
    model_fstatistics = summary(lm_model_name)$fstatistic
    neat_caption = paste('**Adjusted *R*-squared: ',
                         round(model_adj_r_squared,2), "; *F*(",
                         model_fstatistics[2],",",model_fstatistics[3],
                         ") = ",round(model_fstatistics[1],2),"**",sep="")
    
    # return the table
    return(pander(neat_output, split.table = Inf, caption = neat_caption, style = 'rmarkdown'))
  }else{ # or return a table without the caption
    return(pander(neat_output,split.table = Inf, style = 'rmarkdown'))
  }
}

##

# "pander_lme_to_latex": Export an LMER summary table to a latex file

pander_lme_to_latex = function(lme_model_name, save_filename){
  
  # load in pander
  require(pander)
  require(Hmisc)
  require(plyr)
  require(dplyr)
  
  # disable scientific notation
  options(scipen = 999)
  
  # convert the model summary to a dataframe
  neat_output = data.frame(summary(lme_model_name)$coefficient)
  
  # round p-values (using Psychological Science's recommendations)
  neat_output$p = 2*(1-pnorm(abs(neat_output$t.value)))
  neat_output$p[neat_output$p < .0005] = round(neat_output$p[neat_output$p < .0005],4)
  neat_output$p[neat_output$p >= .0005] = round(neat_output$p[neat_output$p >= .0005],3)
  neat_output$p[neat_output$p >= .25] = round(neat_output$p[neat_output$p >= .25],2)
  
  # round the estimates, standard error, and t-values
  neat_output$"t-value" = round(neat_output$t.value,3)
  neat_output$"Std. Error" = round(neat_output$Std..Error,3)
  neat_output$Estimate = round(neat_output$Estimate,3)
  
  # create a new column for variable names
  neat_output$Predictor = row.names(neat_output)
  rownames(neat_output) = NULL
  neat_output = neat_output[,c(7,1,6,5,4)]
  
  # create significance and trending markers
  neat_output$"Sig." = ' '
  neat_output$"Sig."[neat_output$p < .1] = '.'
  neat_output$"Sig."[neat_output$p < .05] = '*'
  neat_output$"Sig."[neat_output$p < .01] = '**'
  neat_output$"Sig."[neat_output$p < .001] = '***'
  neat_output = plyr::rename(neat_output,c("p" = 'p-value'))
  
  # save to file
  latex(neat_output,file=save_filename,rownamesTexCmd=NULL)
}

##

# "pander_anova": simplify anova printouts and include adjusted R-squared and F-stats
pander_anova = function(anova_model_name){
  
  # load in pander
  require(pander)
  require(plyr)
  
  # disable scientific notation
  options(scipen = 999)
  
  # convert the model summary to a dataframe and rename variables
  neat_output = data.frame(anova_model_name)
  
  # round p-values (using Psychological Science's recommendations)
  neat_output$p = neat_output$Pr..Chisq.
  neat_output$p[is.na(neat_output$p)] = 0
  neat_output$p[neat_output$p < .0005] = round(neat_output$p[neat_output$p < .0005],4)
  neat_output$p[neat_output$p >= .0005] = round(neat_output$p[neat_output$p >= .0005],3)
  neat_output$p[neat_output$p >= .25] = round(neat_output$p[neat_output$p >= .25],2)
  
  # create significance and trending markers
  neat_output$sig = ' '
  neat_output$sig[neat_output$p < .15] = '.'
  neat_output$sig[neat_output$p < .05] = '*'
  neat_output$sig[neat_output$p < .01] = '**'
  neat_output$sig[neat_output$p < .001] = '***'
  
  # re-create blank spaces from original anova output
  neat_output$p[is.na(neat_output$Pr..Chisq.)] = ' '
  neat_output$sig[is.na(neat_output$Pr..Chisq.)] = ' '
  neat_output = replace(neat_output,is.na(neat_output),' ')
  
  # rename variables
  neat_output = plyr::rename(neat_output, replace = c('Df' = 'DF',
                                                      'logLik' = 'Log Likelihood',
                                                      'Chisq' = "Chi Sq.",
                                                      'Chi.Df' = "Chi Sq. DF"))
  neat_output = subset(neat_output, select = -c(Pr..Chisq.))
  
  # return the neatened table
  return(pander(neat_output, style="rmarkdown",split.table = Inf))
}
