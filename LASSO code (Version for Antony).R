# This script required the user to hit Run either by the button
# in the top right corner of this panel, or by hitting Ctrl+Enter

getwd()
setwd("~/DAEN_698/MCS_BM1")
getwd()

# load all necessary packages using only 1 command/line
library_list <- c(library(dplyr),library(tidyverse),
                  library(readr),
                  library(stats),library(leaps),library(lars),
                  library(elasticnet),library(Matrix),library(glmnet),
                  library(stringi))
# load all necessary packages 1 by 1 instead
library(dplyr)
library(tidyverse)
library(readr)

library(stats)
library(leaps)
library(lars)

library(elasticnet)
library(Matrix)
library(glmnet)
library(stringi)


# these 2 lines together create a simple character list of 
# all the file names in the file folder of datasets you created
directory_paths <- "~/DAEN_698/spencer"
filefolder_path <- "~/GMU folders (local)/DAEN_698/spencer"
filepaths_list <- list.files(path = filefolder_path, full.names = TRUE, 
                             recursive = TRUE)

# reformat the names of each of the csv file formatted datasets
DS_names_list <- basename(filepaths_list)
DS_names_list <- tools::file_path_sans_ext(DS_names_list)
head(DS_names_list, n = 4)
str(DS_names_list)


# sort both of the list of file names so that they are in the proper order
my_order = DS_names_list |> 
  # split apart the numbers
  strsplit(split = "-", fixed = TRUE) |>  unlist() |> 
  # convert them to numeric and get them in a data frame
  as.numeric() |> matrix(nrow = length(DS_names_list), byrow = TRUE) |>
  as.data.frame() |>
  # get the appropriate ordering to sort the data frame
  do.call(order, args = _)

DS_names_list = DS_names_list[my_order]

filepaths_list = filepaths_list[my_order]
filepaths_list


# this line reads all of the data in each of the csv files 
# using the name of each store in the list we just created
datasets <- lapply(filepaths_list, read.csv)


# This function fits all 58,500 LASSO regressions for/on
# each of the corresponding 58.5k datasets stored in the object
# of that name, then outputs standard regression results which 
# are typically called returned for any regression ran using R
set.seed(11)     # to ensure replicability
LASSO_fits <- lapply(datasets, function(i) 
               enet(x = as.matrix(select(i, starts_with("X"))), 
               y = i$Y, lambda = 0, normalize = FALSE))

length(LASSO_fits)
head(str(LASSO_fits), n = 1)
head(LASSO_fits, n = 1)
head(LASSO_fits, n = 3)


# This stores and prints out all of the regression 
# equation specifications selected by LASSO when called
set.seed(11)     # to ensure replicability
LASSO_Coeffs <- lapply(LASSO_fits, 
                       function(i) predict(i, x = as.matrix(select(i, starts_with("X"))), 
                                           s = 0.1, mode = "fraction", 
                                           type = "coefficients")[["coefficients"]])

LASSO_Coeffs[[1]]
all_Coeffs_estimated_by_1st_LASSO <- LASSO_Coeffs[[1]]
all_Coeffs_estimated_by_1st_LASSO
LASSO_Coeffs[[1]][["X1"]]



### Write my own custom function which will separate out and return a 
### new list containing just the Independent Variables/Factors/Predictors
### which are 'selected' or chosen for each individual dataset (all 
### of which contain 15,000 randomly generated synthetic observations)
### by the unique LASSO Regression ran on it, i.e. the simple linear
### regression specification which LASSO can find that best fits the 
### the synthetic observations within that dataset.
positive_coeffs <- lapply(LASSO_Coeffs, function(i) i[i > 0])
positive_coeffs[1]


IVs_selected_by_LASSO <- lapply(LASSO_Coeffs, function(i) names(i[i > 0]))
IVs_selected_by_LASSO[1]
head(IVs_selected_by_LASSO, n = 4)
class(IVs_selected_by_LASSO)
str = str(IVs_selected_by_LASSO)
str(IVs_selected_by_LASSO[1])
str(IVs_selected_by_LASSO[2])
rownames(IVs_selected_by_LASSO)    # output: NULL
colnames(IVs_selected_by_LASSO)    # output: NULL


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

BM1_results <- data.frame(DS_name = DS_names_list, 
                          IVs_Selected = sapply(IVs_selected_by_LASSO, 
                                                toString))
