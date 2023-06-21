# Spencer Marlen-Starr's RStudio script of functions, methods, code, and 
# code comments required for running loading and preparing a file folder full
# of csv file formatted sample datasets for Benchmarking at a time.

## This script in its entirety can be run by the user by hitting Ctrl+Alt+R.
rm(list = ls())
#setwd("C:/Users/Spencer/OneDrive/Documents/Analytics Projects/EER project")
getwd()

# load all necessary packages
library(plyr)
library(dplyr)
library(stringi)
library(purrr)
library(data.table)
library(parallel)


# Extract all of the individual spreadsheet containing csv files
# in the file folder called 'Data' which is filled random synthetic observations 
# to run FS, Stepwise, and possibly EER on so as to enable proper comparison of their results. 
# There are 260k spreadsheets in this folder though,
# so only do this if you have a very powerful computer at hand, my new 32 GB of 
# RAM having laptop was nowhere near up for that task!
# So, I run this on subsets I copy+pastad from the Data folder into new folders
# of either 5,000, 10,000, or 15,000 at a time and just named them by the range of datasets they include.
folderpath <- "C:/Users/Spencer/OneDrive/Documents/Analytics Projects/EER project/Data Folders/csvs/0.5-12-1-1 to 0.5-12-10-500"
system.time(paths_list <- list.files(path = folderpath, 
                                     full.names = TRUE,  
                                     recursive = TRUE))

# shorten the names of each of the datasets corresponding to each file path in paths_list
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
#system.time( datasets <- lapply(paths_list, fread) )
# Depending on how many of the datasets you run this on at a time, it can 
# really take a long time still though, even using fread, I ended up only doing subsets of 5k & 10k at a time. 
# So, in order to speed things up even more, when I am not doing anything else on my laptop that is taxing,
# I parallelize this importation task.
CL <- makeCluster(detectCores() - 2L)
clusterExport(CL, c('paths_list'))
system.time(datasets <- parLapply(cl = CL, X = paths_list, 
                                  fun = data.table::fread))

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
All_Variable_Names <- lapply(datasets, function(k) {names(k)})

# assign all 30 candidate regressor names to an object
var_names <- c("X1","X2","X3","X4","X5","X6","X7","X8",
               "X9","X10","X11","X12","X13","X14","X15",
               "X16","X17","X18","X19","X20","X21","X22", 
               "X23","X24","X25","X26","X27","X28","X29","X30")

### truncate & transform the datasets list before running the regressions
# Step 1: remove the first 2 non-numerical rows of each dataset.
datasets <- lapply(datasets, function(i) {i[-1:-3, ]})
# Step 2: they are chr rather than numeric, fix that.
system.time(datasets <- lapply(datasets, \(X) { lapply(X, as.numeric) }))
# Step 3: convert all of them to data.tables, a faster alternative to data.frames
datasets <- lapply(datasets, function(i) { as.data.table(i) })

stopCluster(CL)
rm(CL)
rm(paths_list, my_order, folderpath)

#save.image("C:/Users/Spencer/OneDrive/Documents/Analytics Projects/EER project/Saved WorkSpaces/Workspaces for dataset folders starting with '0.5'/loaded WorkSpace for datasets from '0.5-12-1-1 to 0.5-12-10-500'.RData")

