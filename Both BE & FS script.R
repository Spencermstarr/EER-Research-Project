# Spencer Marlen-Starr's RStudio script of functions, methods, code, and 
# code comments required for running Benchmark #2, namely the 
# Backward Elimination version of Stepwise Regression
# on some of our random synthetic observations to see how well it 
# does so we can compare its results with Dr. Davies EER procedure.


# find out which working directory R has defaulted to
getwd()
setwd("~/DAEN_698/MCS_BMs 2 & 3")
getwd()


### Benchmark #2: 
### One of the two main versions of the classical method
### of automated optimal variable selection back before
### the modern machine learning revolution; namely,
### the 'Backward Elimination' version of Stepwise Regression.


# load all necessary packages
library(stats)
library(dplyr)
library(tidyverse)
library(tibble)
library(readr)

library(leaps)
library(lars)
library(stringi)
library(purrr)


# Importing an entire file folder's worth of 1 worksheet csv files, 
# these worksheets were created using the random observations 
# generating Macro that Dr. Davies wrote for me with 3 different
# random possible worksheets created on each individual pass of the 
# various nested levels  of iteration laid out in the original 
# draft of the Working Paper's in depth description of the
# Monte Carlo Simulations used to compare his EER algorithm to
# the current standard alternatives and those of the past
# as benchmarks.




### Step 1 for comparing the results of BE on our
### randomly generated synthetic sample data,
### in terms of runtime, accuracy, and probability
### of accidentally choosing spuriously correlated
### regressors - Importing & Loading the aforementioned
### sample observations Dr. Davies
### generated for this purpose.
# Import all 531 of the individual csv files, each of which
# containing 500 rows by 30 columns worth of randomly generated 
# (synthetic) observations below.
# In order to accomplish this, I will be using the readr library in R.



# Extract all of the individual spreadsheet containing workbooks
# in the file folder called 'Run_3_seven) which is filled
# random synthetic observations to run Lasso, Stepwise, and eventually EER
# on to compare the results. There are 531 random spreadsheets in this folder
directory_path <- "~/DAEN_698/spencer"
filepath_list <- list.files(path = directory_path, full.names = TRUE, recursive = TRUE)
length(filepath_list)
str(filepath_list)

# reformat the names of each of the csv file formatted datasets
DS_names_list <- basename(filepath_list)
DS_names_list <- tools::file_path_sans_ext(DS_names_list)
head(DS_names_list, n = 4)

## This line reads all of the data in each of the csv files 
## using the name of each store in the list we just created.
datasets <- lapply(filepath_list, read.csv)
# Running lines 77 & 78 to create the 'datasets' list took somewhere
# between 22 & 23 minutes.
### Step 1 is complete



### Step 3: Run a Backward Elimination Stepwise Regression
### function on each of the 47,500 datasets.
### Assign the full models to their corresponding datasets and
### store these in the object "all_regressors_models"
set.seed(11)      # for reproducibility
full_models <- vector("list", length = length(datasets))
BE_fits <- vector("list", length = length(datasets))
head(BE_fits, n = 3)   # returns a list with 18 elements, all of which are NULL


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


IVs_Selected_by_BE <- lapply(seq_along(BE_fits), 
                              \(i) names(coef(BE_fits[[i]])))

head(IVs_Selected_by_BE, n = 3)



### Again, the final format of the results should look
### like the following:  "#-#-#-#, X#, X#, X#, X#, etc. 
# Important note! BE can wing up with a different number 
# of X#s to select.
## print out the output formatted the way Dr. Davies asked for!
write.csv(data.frame(DS_name = DS_names_list, 
                     Variables_selected = sapply(IVs_Selected_by_BE, 
                                                 toString)), 
          file = "IVs_Selected_by_BE.csv", row.names = FALSE)






### Step 4/5 (optional): Run a Forward Selection Stepwise Regression
### function on each of the 47,500 datasets.
### Assign the null models to their corresponding datasets and
### store these in the object "null_models"
set.seed(11)      # for reproducibility
#datasets[[1]]$
null_model = lm(datasets[[1]]$Y ~ 1, data = datasets[[1]])
null_model
null_model[1]
null_model[[1]]


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

IVs_selected_by_FS <- lapply(seq_along(FS_fits), 
                             \(i) names(coef(FS_fits[[i]])))


### Again, the final format of the results should look
### like the following:  "#-#-#-#, X#, X#, X#, X#, etc. 
# Important note! BE can wing up with a different number 
# of X#s to select.
## print out the output formatted the way Dr. Davies asked for!
write.csv(data.frame(DS_name = DS_names_list, 
                     Variables_selected = sapply(IVs_selected_by_FS, toString)), 
          file = "IVs_Selected_by_FS.csv", row.names = FALSE)