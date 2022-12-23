# Spencer Marlen-Starr's RStudio script of functions, methods, code, and 
# code comments required for running Benchmark #2, namely the 
# Backward Elimination version of Stepwise Regression
# on some of our random synthetic observations to see how well it 
# does so we can compare its results with Dr. Davies EER procedure.


# find out which working directory R has defaulted to
getwd()
setwd("~/GMU folders (local)/DAEN_698/MCS_BMs 2 & 3")
getwd()

### Benchmark #2: 
### One of the two main versions of the classical method
### of automated optimal variable selection back before
### the modern machine learning revolution; namely,
### the 'Backward Elimination' version of Stepwise Regression.

# load all necessary packages
library(stats)
library(plyr)
library(dplyr)
library(tidyverse)
library(tibble)
library(readr)

library(leaps)
library(lars)
library(stringi)
library(purrr)



# Extract all of the individual spreadsheet containing workbooks
# in the file folder called 'Run_3_seven) which is filled
# random synthetic observations to run Lasso, Stepwise, and eventually EER
# on to compare the results. There are 531 random spreadsheets in this folder
directory_paths <- "~/GMU folders (local)/DAEN_698/other datasets/sample obs(20 csvs)"
directory_paths <- "~/other datasets/sample obs(20 csvs)"
directory_paths <- "~/sample obs(20 csvs)"  
filepaths_list <- list.files(path = directory_paths, full.names = TRUE, 
                             recursive = TRUE)
length(filepaths_list)
str(filepaths_list)

# reformat the names of each of the csv file formatted datasets
DS_names_list <- basename(filepaths_list)
DS_names_list <- tools::file_path_sans_ext(DS_names_list)
head(DS_names_list, n = 4)


my_order = DS_names_list |> 
  # split apart the numbers
  strsplit(split = "-", fixed = TRUE) |>
  unlist() |> 
  # convert them to numeric and get them in a data frame
  as.numeric() |> 
  matrix(nrow = length(DS_names_list), byrow = TRUE) |>
  as.data.frame() |>
  # get the appropriate ordering to sort the data frame
  do.call(order, args = _)
my_order
DS_names_list[my_order]
DS_names_list = DS_names_list[my_order]
DS_names_list

filepaths_list[my_order]
filepaths_list = filepaths_list[my_order]
filepaths_list


# Import all 20 of the individual csv files, each of which
# containing 500 rows by 31 columns worth of randomly generated 
# (synthetic) observations below.
# In order to accomplish this, I will be using the readr library in R.
## This line reads all of the data in each of the csv files 
## using the name of each store in the list we just created.
datasets <- lapply(filepaths_list, read.csv)


### Step 3: Run a Backward Elimination Stepwise Regression
### function on each of the  datasets.
### Assign the full models to their corresponding datasets and
### store these in the object "all_regressors_models"
set.seed(11)      # for reproducibility
full_models <- vector("list", length = length(datasets))
BE_fits <- vector("list", length = length(datasets))
head(BE_fits, n = 3)   # returns a list with 18 elements, all of which are NULL


#LASSO_fits <- lapply(datasets, function(i)  full_models[[i]] <- lm(formula = Y ~ ., data = datasets[[i]])


set.seed(11)      # for reproducibility
for(i in seq_along(datasets)) {
  full_models[[i]] <- lm(formula = Y ~ ., data = datasets[[i]])
  BE_fits[[i]] <- step(object = full_models[[i]], 
                        scope = formula(full_models[[i]]),
                        direction = 'backward',
                        trace = 0)
}

head(full_models, n = 2)
head(BE_fits, n = 2)

BE_Coeffs <- lapply(seq_along(BE_fits), function(i) coef(BE_fits[[i]]))

Models_Selected_by_BE <- lapply(seq_along(BE_fits), 
                              \(i) names(coef(BE_fits[[i]])))
head(Models_Selected_by_BE, n = 3)

IVs_Selected_by_BE <- lapply(seq_along(BE_fits), 
                             \(i) names(coef(BE_fits[[i]])[-1]))



### Again, the final format of the results should look
### like the following:  "#-#-#-#, X#, X#, X#, X#, etc. 
# Important note! BE can wing up with a different number 
# of X#s to select.
## print out the output formatted the way Dr. Davies asked for!
BM2_models <- data.frame(DS_name = DS_names_list, 
                          Variables_selected = sapply(IVs_Selected_by_BE, 
                                                toString))





### Benchmark #3: Run a Forward Selection Stepwise Regression
### function on each of the 20 datasets.
### Assign the null models to their corresponding datasets and
### store these in the object "null_models"
set.seed(11)      # for reproducibility
null_models <- vector("list", length = length(datasets))
null_models[[1]]
null_models[[2]]
head(null_model)
head(null_models, n = 3)
FS_fits <- vector("list", length = length(datasets))
head(FS_fits, n = 3)   # returns a list with 15 elements, all of which are NULL


set.seed(11)      # for reproducibility
for(j in seq_along(datasets)) { null_models[[j]] = lm(formula = Y ~ 1, 
                                                     data = datasets[[j]]) 
                                FS_fits[[j]] = step(object = null_models[[j]],
                                                    direction = 'forward',
                       scope = formula(full_models[[j]]), trace = 0) }


FS_Coeffs <- lapply(seq_along(FS_fits), function(i) coef(FS_fits[[i]]))

Models_Selected_by_FS <- lapply(seq_along(FS_fits), 
                             \(i) names(coef(FS_fits[[i]])))

# without the Intercepts included
IVs_Selected_by_FS <- lapply(seq_along(Models_Selected_by_FS), 
                             \(i) Models_Selected_by_FS[[i]][-1])


### Again, the final format of the results should look
### like the following:  "#-#-#-#, X#, X#, X#, X#, etc. 
# Important note! BE can wing up with a different number 
# of X#s to select.
## print out the output formatted the way Dr. Davies asked for!
BM3_models <- data.frame(DS_name = DS_names_list, 
                          IVs_Selected = sapply(IVs_Selected_by_FS, 
                                                      toString))



