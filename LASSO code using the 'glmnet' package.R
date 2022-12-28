# This script required the user to hit Run either by the button
# in the top right corner of this panel, or by hitting Ctrl+Enter

getwd()
setwd("~/DAEN_698/MCS_BM1")
getwd()



# load all necessary packages using only 1 command/line
library_list1 <- c(library(stats),library(dplyr),library(tidyverse),
                  library(readr),library(stringi))

library_list2 <- c(library(stats), library(leaps),
                   library(lars), library(elasticnet))

# load all necessary packages 1 by 1 instead
library(dplyr)
library(tidyverse)
library(readr)
library(stringi)
library(purrr)

library(stats)
library(Matrix)
library(glmnet)
library(ggplot2)




# these 2 lines together create a simple character list of 
# all the file names in the file folder of datasets you created
directory_path <- "~/DAEN_698/sample obs"
filepath_list <- list.files(path = directory_path, full.names = TRUE, 
                        recursive = TRUE)
length(filepath_list)
str(filepath_list)




# The code below reads the data into the RStudio Workspace from
# each of the 47k datasets in an iterative manner in such a way 
# that it assigns each of them to the corresponding name of that 
# dataset in the file folder they are stored in.
datasets <- lapply(list.files(path = "~/DAEN_698/sample obs", 
                              full.names = TRUE, recursive = TRUE), read.csv)


# reformat the names of each of the csv file formatted dataset
DS_names_list <- basename(filepath_list)
DS_names_list <- tools::file_path_sans_ext(DS_names_list)
head(DS_names_list, n = 4)
str(DS_names_list)



# This function fits all 47,500 LASSO regressions for/on
# each of the corresponding 47.5k datasets stored in the object
# of that name, then outputs standard regression results which 
# are typically called returned for any regression ran using R
set.seed(11)
LASSO_fits = lapply(datasets, function(i)
  lars(x, y, type = "lasso", normalize = FALSE, intercept = TRUE))



set.seed(11)     # to ensure replicability
LASSO_coefs <- predict(LASSO_fits, data.frame(a = 0, b = 0), s = 2)$fit

set.seed(11)     # to ensure replicability
LASSO_coeffs <- lapply(LASSO_fits, 
                      function(i) predict(i, data.frame(a = 0, b = 0), s = 2)$fit)




### Write my own custom function which will separate out and return a 
### new list containing just the Independent Variables/Factors/Predictors
### which are 'selected' or chosen for each individual dataset (all 
### of which contain 15,000 randomly generated synthetic observations)
### by the unique LASSO Regression ran on it, i.e. the simple linear
### regression specification which LASSO can find that best fits the 
### the synthetic observations within that dataset.
positive_coeffs <- lapply(LASSO_coeffs, function(i) i[i > 0])



IVs_selected_by_LASSO <- lapply(LASSO_coeffs, function(i) names(i[i > 0]))



## Write all the Factors/Predictors the 
## 47,500 LASSO Regressions to a text file.
getwd()
setwd("~/DAEN_698/MCS_BM1")
setwd("~/DAEN_698")
getwd()

write.csv(data.frame(DS_name = DS_names_list, 
                     IVs_selected = sapply(IVs_selected_by_LASSO, 
                                           toString)), 
          file = "Benchmark1_results.csv", row.names = FALSE)


