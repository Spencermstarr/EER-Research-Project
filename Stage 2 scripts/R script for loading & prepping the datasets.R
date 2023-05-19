# Spencer Marlen-Starr's RStudio script of functions, methods, code, and 
# code comments required for running Benchmarks 2 & 3, namely the 
# Backward Elimination & Forward Selection versions of Stepwise Regression
# on some of our random synthetic observations to see how well it 
# does so we can compare its results with Dr. Davies' EER procedure.

## This script in its entirety can be run by the user by hitting Ctrl+Alt+R.
#rm(list = ls())
#setwd("C:/Users/Spencer/Documents/EER Project")
#setwd("D:/EER")
getwd()

# load all necessary packages
library(plyr)
library(dplyr)
library(stringi)
library(purrr)
library(data.table)
library(parallel)


# Extract all of the individual spreadsheet containing workbooks
# in the file folder called 'Data' which is filled
# random synthetic observations to run FS, Stepwise, and eventually EER
# on to compare the results. There are 260k spreadsheets in this folder
folderpath <- "C:/Users/Spencer/OneDrive/Documents/Analytics Projects/EER Project/Data Folders/csvs/0.25-7-1-1 to 0.25-7-10-500"
system.time(paths_list <- list.files(path = folderpath, 
                                     full.names = TRUE,  
                                     recursive = TRUE))
# shorten the names of each of the datasets corresponding to 
# each file path in paths_list
DS_names_list <- basename(paths_list)
DS_names_list <- tools::file_path_sans_ext(DS_names_list)

# sort both of the list of file names so that they are in the proper order
my_order = DS_names_list |> 
  # split apart the numbers
  strsplit(split = "-", fixed = TRUE) |> unlist() |> 
  # convert them to numeric and get them in a data frame
  as.numeric() |> matrix(nrow = length(DS_names_list), byrow = TRUE) |>
  as.data.frame() |>
  # get the appropriate ordering to sort the data frame
  do.call(order, args = _)

DS_names_list = DS_names_list[my_order]
paths_list = paths_list[my_order]

## Import all 260k of the individual csv files below.
## In order to accomplish this, I will be using the readr library in R.
## This line reads all of the data in each of the csv files 
## using the name of each store in the list we just created.
system.time( datasets <- lapply(paths_list, fread) )
#CL <- makeCluster(detectCores() - 5L)
#clusterExport(CL, c('paths_list'))
#system.time(datasets <- parLapply(cl = CL, X = paths_list, 
#                                  fun = data.table::fread))

# change column names of all the columns in the data.table 'datasets'
datasets <- lapply(datasets, function(dataset_i) { 
  colnames(dataset_i) <- c("Y","X1","X2","X3","X4","X5","X6","X7","X8",
                           "X9","X10","X11","X12","X13","X14","X15",
                           "X16","X17","X18","X19","X20","X21","X22",
                           "X23","X24","X25","X26","X27","X28","X29","X30")
  dataset_i })

Structural_IVs <- lapply(datasets, function(j) {j[1, -1]})
Structural_Variables <- lapply(Structural_IVs, function(i) {names(i)[i == 1]})
Nonstructural_Variables <- lapply(Structural_IVs, function(i) {
  names(i)[i == 0] })

# assign all 30 candidate regressor names to an object
var_names <- c("X1","X2","X3","X4","X5","X6","X7","X8",
               "X9","X10","X11","X12","X13","X14","X15",
               "X16","X17","X18","X19","X20","X21","X22", 
               "X23","X24","X25","X26","X27","X28","X29","X30")


# truncate & transform the datasets list before running the regressions
datasets <- lapply(datasets, function(i) {i[-1:-3, ]})
system.time(datasets <- lapply(datasets, \(X) { lapply(X, as.numeric) }))
datasets <- lapply(datasets, function(i) { as.data.table(i) })

#stopCluster(CL)
#rm(CL)


#save.image("D:/EER/Saved WorkSpaces/Workspaces for dataset folders starting with '0.25'/datasets WorkSpace for 0.25-7-1-1 to 0.25-7-10-500.RData")
save.image("C:/Users/Spencer/OneDrive/Documents/Analytics Projects/EER Project/Saved WorkSpaces/datasets WorkSpace for '0.25-7-1-1 to 0.25-7-10-500'.RData")
#save.image("D:/EER folder/WorkSpaces/datasets WorkSpace for 0.25-7-1-1 to 0.25-7-10-500 datasets.RData")



