# This script required the user to hit Run either by the button
# in the top right corner of this panel, or by hitting Ctrl+Enter

# find out which working directory R has defaulted to
getwd()

# copy+paste whatever the path is for your file folder with all the 
# random synthetic csvs you have created inside a set of quotation marks
# inside of the setwd() function below and then press Run (or hit Ctrl+Enter)
setwd()
# Or, click the Session option in the Ribbon, and in the
# Set Working Directory's drop down list, select Choose Directory
# in order to set it manually.

# load all necessary packages using only 1 command/line
library_list <- c(library(plyr),library(dplyr),library(stringi),
                  library(purrr),library(stats),library(data.table),
                  library(leaps),library(lars),library(parallel))

library(plyr)
library(dplyr)
library(readr)
library(stringi)
library(purrr)
library(stats)
library(leaps)
library(lars)
library(glmnet)
library(data.table)
library(parallel)



# these 2 lines together create a simple character list of 
# all the file names in the file folder of datasets you created
folderpath <- "C:/Users/Spencer/Documents/EER Project/Data/top 100"
paths_list <- list.files(path = folderpath, full.names = T, recursive = T)
paths_list

# reformat the names of each of the csv file formatted dataset
DS_names_list <- basename(paths_list)
DS_names_list <- tools::file_path_sans_ext(DS_names_list)
DS_names_list

# sort both of the list of file names so that they are in the proper order
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

DS_names_list = DS_names_list[my_order]

paths_list = paths_list[my_order]



# The code below reads the data into the RStudio Workspace from
# each of the 40 datasets in an iterative manner in such a way 
# that it assigns each of them to the corresponding name of that 
# dataset in the file folder they are stored in.
system.time( datasets <- lapply(paths_list, fread) )
class(datasets)   # "list"
str(datasets)

# change column names of all the columns in the data.table 'datasets'
datasets <- lapply(datasets, function(dataset_i) { 
  colnames(dataset_i) <- c("Y","X1","X2","X3","X4","X5","X6","X7","X8",
                           "X9","X10","X11","X12","X13","X14","X15",
                           "X16","X17","X18","X19","X20","X21","X22", 
                           "X23","X24","X25","X26","X27","X28","X29","X30")
  dataset_i })

Structural_IVs <- lapply(datasets, function(j) {j[1, -1]})
Structural_Variables <- lapply(Structural_IVs, function(i) {names(i)[i == 1]})
head(Structural_Variables)

datasets <- lapply(datasets, function(i) {i[-1:-3, ]})
datasets <- lapply(datasets, \(X) { lapply(X, as.numeric) })
datasets <- lapply(datasets, function(i) { as.data.table(i) })
datasets <- lapply(datasets, \(X) { round(X, 3) })




# This function fits all 40 LASSO regressions for/on
# each of the corresponding 40 datasets stored in the object
# of that name, then outputs standard regression results which 
# are typically called returned for any regression ran using R
set.seed(100)     # to ensure replicability
system.time(LASSO.Lars.fits <- lapply(datasets, function(i) 
  lars(x = as.matrix(select(i, starts_with("X"))), 
         y = i$Y, type = "lasso")))

# This stores and prints out all of the regression 
# equation specifications selected by LASSO when called

LASSO.Coeffs2 <- lapply(LASSO.fits, 
                        function(i) predict(i, s = 0.1, type = "coefficients"))
#IVs_Selected_by_LASSO <- lapply(LASSO.Coeffs2, function(i) names(i[i > 0]))
LASSO.Coeffs2[[1]]@x
LASSO.Coeffs


LASSO.Lars.Coeffs <- lapply(LASSO.Lars.fits, 
                       function(i) predict(i, 
                                           x = as.matrix(dplyr::select(i, starts_with("X"))), 
                                           s = 0.1, mode = "fraction", 
                                           type = "coefficients")[["coefficients"]])

IVs.Selected.by.Lars <- lapply(LASSO.Lars.Coeffs, function(i) names(i[i > 0]))

