# Spencer Marlen-Starr's RStudio script of functions, methods, code, and 
# code comments required for running Benchmarks #2 & 3, namely the 
# Backward Elimination & Forward Selection versions of Stepwise Regression
# on some of our random synthetic observations to see how well it 
# does so we can compare its results with Dr. Davies' EER procedure.


# find out which working directory R has defaulted to
getwd()
setwd("~/GMU folders (local)/DAEN_698/MCS_BMs 2 & 3")
# Or, click the Session option in the Ribbon, and in the
# Set Working Directory's dropdown list, select Choose Directory
# in order to set it manually.


### Benchmark #2: 
### One of the two main versions of the classical method
### of automated optimal variable selection back before
### the modern machine learning revolution; namely,
### the 'Backward Elimination' version of Stepwise Regression.


# load all necessary packages using only 1 command/line
library_list <- c(library(stats),library(dplyr),library(tidyverse),
                  library(leaps),library(lars),library(tibble),
                  library(readr),library(stringi),library(purrr))
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
# in the file folder called 'spencer' which is filled
# random synthetic observations to run Lasso, Stepwise, and eventually EER
# on to compare the results. There are 58.5k spreadsheets in this folder
filepath <- "~/DAEN_698/spencer"
filepath <- "~/GMU folders (local)/DAEN_698/spencer"
filepaths_list <- list.files(path = filepath, full.names = TRUE, recursive = TRUE)
length(filepaths_list)
str(filepaths_list)

# reformat the names of each of the csv file formatted datasets
DS_names_list <- basename(filepaths_list)
DS_names_list <- tools::file_path_sans_ext(DS_names_list)
head(DS_names_list, n = 4)

# sort both of the list of file names so that they are in the proper order
my_order = DS_names_list |> 
  # split apart the numbers
  strsplit(split = "-", fixed = TRUE) |>
  unlist() |> 
  # convert them to numeric and get them in a data frame
  as.numeric() |> matrix(nrow = length(DS_names_list), byrow = TRUE) |>
  as.data.frame() |>
  # get the appropriate ordering to sort the data frame
  do.call(order, args = _)
my_order
DS_names_list = DS_names_list[my_order]

filepaths_list = filepaths_list[my_order]


# Import all 58.5k of the individual csv files, each of which
# containing 500 rows by 30 columns worth of randomly generated 
# (synthetic) observations below.
# In order to accomplish this, I will be using the readr library in R.
## This line reads all of the data in each of the csv files 
## using the name of each store in the list we just created.
datasets <- lapply(filepaths_list, read.csv)
### Step 1 is complete



### Step 3: Run a Backward Elimination Stepwise Regression
### function on each of the 58,500 datasets.
### Assign the full models to their corresponding datasets and
### store these in the object "all_regressors_models"
set.seed(11)      # for reproducibility
full_models <- vector("list", length = length(datasets))
BE_fits <- vector("list", length = length(datasets))


set.seed(11)      # for reproducibility
for(i in seq_along(datasets)) {
  full_models[[i]] <- lm(formula = Y ~ ., data = datasets[[i]])
  BE_fits[[i]] <- step(object = full_models[[i]], 
                        scope = formula(full_models[[i]]),
                        direction = 'backward', trace = 0) }

# extract all of the coefficients estimated by the 59.5k BEs ran
BE_Coeffs <- lapply(seq_along(BE_fits), function(i) coef(BE_fits[[i]]))

# extract the names of all IVs selected by them + the intercept
Models_Selected_by_BE <- lapply(seq_along(BE_fits), 
                              \(i) names(coef(BE_fits[[i]])))

# extract the names of all IVs selected by them without the intercept
IVs_Selected_by_BE <- lapply(seq_along(BE_fits), 
                             \(i) names(coef(BE_fits[[i]])[-1]))


### Again, the final format of the results should look
### like the following:  "#-#-#-#, X#, X#, X#, X#, etc. 
# Important note! BE can wing up with a different number 
# of X#s to select.
## print out the output formatted the way Dr. Davies asked for!
write.csv(data.frame(DS_name = DS_names_list, 
                     IVs_selected = sapply(IVs_Selected_by_BE, 
                                                 toString)), 
          file = "IVs_Selected_by_BE.csv", row.names = FALSE)

BM2_models_2cols <- data.frame(DS_name = DS_names_list, 
                          IVs_Selected = sapply(IVs_Selected_by_BE, 
                                                      toString))

BM2_models_1col <- paste(BM2_models_2cols$DS_name, ";", 
                         BM2_models_2cols$IVs_Selected)






### Step 4: Run a Forward Selection Stepwise Regression
### function on each of the 58,500 datasets.
### Assign the null models to their corresponding datasets and
### store these in the object "null_models"
set.seed(11)      # for reproducibility
null_models <- vector("list", length = length(datasets))
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
write.csv(data.frame(DS_name = DS_names_list, 
                     Variables_selected = sapply(IVs_Selected_by_FS, toString)), 
          file = "IVs_Selected_by_FS.csv", row.names = FALSE)

BM3_models_2cols <- data.frame(DS_name = DS_names_list, 
                          IVs_selected = sapply(IVs_Selected_by_FS, 
                                                      toString))

BM3_models_1col <- paste(BM3_models_2cols$DS_name, ";", 
                         BM3_models_2cols$IVs_Selected)
