#### My Estimated Exhaustive Regression script


### If one wanted to run this entire script all at once,
### all he would need do is hit Ctrl+Shift+Enter
getwd()
setwd("~/DAEN_698/EER")
getwd()


# Load all libraries needed for this script.
# The library specifically needed to run a basic ASR is the 'leaps' library.
library(dplyr)
library(tidyverse)
library(stats)

library(ggplot2)
library(lattice)
library(caret)

library(leaps)
library(lmSubsets)
library(combinat)
library(purrr)



directory_path <- "~/DAEN_698/sample obs"
filepath_list <- list.files(path = directory_path, full.names = TRUE, recursive = TRUE)

## Reformat the names of the file paths for each of the csv formatted
## datasets so they match their actual names in the file folder
DS_names_list <- basename(filepath_list)
DS_names_list <- tools::file_path_sans_ext(DS_names_list)

# Read each of the 47,500 csv formatted datasets into R.
datasets <- lapply(filepath_list, read.csv)


## Start out by running a standard All Subsets Regression 
ER_fits <- lapply(datasets, function(i)
  regsubsets(x = as.matrix(select(i, starts_with("X"))), 
             y = i$Y, data = i, nvmax = 15, 
             intercept = TRUE, method = "exhaustive"))

ER_fits_summary <- summary(ER_fits)
ER_fits_summary

# The All Subsets Regression we are looking for should find a
# global optimum specification between 3 and 15 predictors (by design).
ER_Coeffs <- lapply(ER_fits, function(i) coef(i, id = 3:15))



