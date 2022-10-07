# This script required the user to hit Run either by the button
# in the top right corner of this panel, or by hitting Ctrl+Enter

getwd()
setwd("~/DAEN_698/MCS_BM1/txt & csv files")
getwd()

# load all necessary packages 1 by 1 instead
library(dplyr)
library(tidyverse)
library(readr)
library(tibble)
library(stringi)
library(purrr)

library(stats)
library(leaps)
library(lars)
library(elasticnet)



# these 2 lines together create a simple character list of 
# all the file names in the file folder of datasets you created
directory_paths <- "~/DAEN_698/other datasets/sample obs"
filepaths_list <- list.files(path = directory_paths, full.names = TRUE, recursive = TRUE)

# reformat the names of each of the csv file formatted dataset
DS_names_list <- basename(filepaths_list)
DS_names_list <- tools::file_path_sans_ext(DS_names_list)
head(DS_names_list)

DS_names_list[order(unlist(DS_names_list), decreasing = TRUE)]


# The code below reads the data into the RStudio Workspace from
# each of the 53k datasets in an iterative manner in such a way 
# that it assigns each of them to the corresponding name of that 
# dataset in the file folder they are stored in.
datasets <- lapply(filepaths_list, read.csv)


# This function fits all 53,500 LASSO regressions for/on
# each of the corresponding 53.5k datasets stored in the object
# of that name, then outputs standard regression results which 
# are typically called returned for any regression ran using R
set.seed(11)     # to ensure replicability
LASSO_fits <- lapply(datasets, function(i) 
               enet(x = as.matrix(select(i, starts_with("X"))), 
                    y = i$Y, lambda = 0, normalize = FALSE))


# This stores and prints out all of the regression 
# equation specifications selected by LASSO when called
set.seed(11)     # to ensure replicability
LASSO_Coeffs <- lapply(LASSO_fits, 
                       function(i) predict(i, x = as.matrix(select(i, starts_with("X"))), 
                                           s = 0.1, mode = "fraction", 
                                           type = "coefficients")[["coefficients"]])


### Write my own custom function which will separate out and return a 
### new list containing just the Independent Variables/Factors/Predictors
### which are 'selected' or chosen for each individual dataset (all 
### of which contain 500 randomly generated synthetic observations)
### by the unique LASSO Regression ran on it, i.e. the simple linear
### regression specification which LASSO can find that best fits the 
### the synthetic observations within that dataset.
Positive_Coeffs <- lapply(LASSO_Coeffs, function(i) i[i > 0])

IVs_Selected_by_LASSO <- lapply(LASSO_Coeffs, function(i) names(i[i > 0]))



## Write all the Factors/Predictors the 
## 53,500 LASSO Regressions to a text file.
getwd()
setwd("~/DAEN_698/MCS_BM1")
setwd("~/DAEN_698/MCS_BM1/txt & csv files")
getwd()

write.csv(data.frame(DS_name = DS_names_list, 
                     Coeff_Estimates = sapply(Positive_Coeffs, toString)), 
          file = "Practice_LASSO_Estimates.csv", row.names = FALSE)

write.csv(data.frame(DS_name = DS_names_list, 
                     IVs_selected = sapply(IVs_Selected_by_LASSO, toString)), 
          file = "Practice_IVs_Selected.csv", row.names = FALSE)
