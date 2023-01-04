# This script required the user to hit Run either by the button
# in the top right corner of this panel, or by hitting Ctrl+Enter

getwd()
# copy+paste whatever the path is for your file folder with all the 
# random synthetic datasets you have created inside a set of quotation marks
# inside of the setwd() function below and then press Run (or hit Ctrl+Enter)
setwd()
# Or, click the Session option in the Ribbon, and in the
# Set Working Directory's drop down list, select Choose Directory
# in order to set it manually.

# load all necessary packages using only 1 command/line
library_list <- c(library(plyr),library(dplyr),library(tidyverse),
                  library(readr),library(tibble),library(stringi),
                  library(janitor),library(purrr),library(stats),
                  library(leaps),library(lars),library(elasticnet),
                  library(vroom),library(data.table),library(parallel),
                  library(microbenchmark))


# these 2 lines together create a simple character list of 
# all the file names in the file folder of datasets you created
folderpath <- "C:/Users/Spencer/Documents/EER Project/datasets folder"
filepaths_list <- list.files(path = folderpath, full.names = TRUE, 
                             recursive = TRUE)
# reformat the names of each of the csv file formatted datasets
DS_names_list <- basename(filepaths_list)
DS_names_list <- tools::file_path_sans_ext(DS_names_list)


# sort both of the list of file names so that they are in the proper order
my_order = DS_names_list |> 
  # split apart the numbers, convert them to numeric 
  strsplit(split = "-", fixed = TRUE) |>  unlist() |> as.numeric() |>
  # get them in a data frame
  matrix(nrow = length(DS_names_list), byrow = TRUE) |> as.data.frame() |>
  # get the appropriate ordering to sort the data frame
  do.call(order, args = _)

DS_names_list = DS_names_list[my_order]
filepaths_list = filepaths_list[my_order]


# this line reads all of the data in each of the csv files 
# using the name of each store in the list we just created
CL1 <- makeCluster(detectCores() - 1L)
clusterExport(CL1, c('filepaths_list'))
system.time( datasets <- lapply(filepaths_list, fread) )
stopCluster(CL1)


# change column names of all the columns in the data.table 'datasets'
CL2 <- makeCluster(detectCores() - 1L)
clusterExport(CL2, c('filepaths_list'))
datasets <- lapply(datasets, function(dataset_i) { 
  colnames(dataset_i) <- c("Y", "X1", "X2", "X3", "X4", "X5", "X6", "X7", 
                           "X8", "X9", "X10", "X11", "X12", "X13", "X14", 
                           "X15", "X16", "X17", "X18", "X19", "X20", "X21", 
                           "X22", "X23", "X24", "X25", "X26", "X27", "X28", 
                           "X29", "X30")
  dataset_i })
stopCluster(CL2)

Structural_IVs_chr <- lapply(datasets, function(j) {j[1, -1]})
Structural_IVs_num <- lapply(Structural_IVs_chr, \(X) { lapply(X, as.numeric) })

True_IVs <- lapply(datasets, function(j) {j[1:3, -1]})
True_IVs <- lapply(True_IVs, function(j) {j[-2, ]})

True_Regressors <- lapply(Structural_IVs_chr, function(i) {
  names(i)[i == 1] })


datasets <- lapply(datasets, function(i) {i[-1:-3, ]})
datasets <- lapply(datasets, \(X) { lapply(X, as.numeric) })
datasets <- lapply(datasets, function(i) { as.data.table(i) })
datasets <- lapply(datasets, \(X) { round(X, 3) })



# This function fits all 260,000 LASSO regressions for/on
# each of the corresponding 260k datasets stored in the object
# of that name, then outputs standard regression results which 
# are typically called returned for any regression ran using R
set.seed(11)     # to ensure replicability
CL3 <- makeCluster(detectCores() - 1L)
clusterExport(CL3, c('datasets'))
LASSO_fits <- lapply(datasets, function(i) 
               enet(x = as.matrix(select(i, starts_with("X"))), 
               y = i$Y, lambda = 0, normalize = FALSE))
stopCluster(CL3)

# This stores and prints out all of the regression 
# equation specifications selected by LASSO when called
# Press Run or hit Ctrl+Enter for the single line below
set.seed(11)     # to ensure replicability
# Press Run or hit Ctrl+Enter only once for the 5 lines below
LASSO_Coeffs <- lapply(LASSO_fits, 
                       function(i) predict(i, 
                                           x = as.matrix(select(i, starts_with("X"))), 
                                           s = 0.1, mode = "fraction", 
                                           type = "coefficients")[["coefficients"]])

### Write my own custom function which will separate out and return a 
### new list containing just the Independent Variables
### which are 'selected' or chosen for each individual dataset.
# Press Run or hit Ctrl+Enter
IVs_selected_by_LASSO <- lapply(LASSO_Coeffs, function(i) names(i[i > 0]))


### Count up how many Variables Selected match  the true 
### structural equation variables for that dataset in order
### to measure LASSO's performance.
Total_Positives <- lapply(True_Regressors, function(i) { length(i) })

system.time(True_Pos_list <- lapply(seq_along(datasets), \(i)
                                    sum(IVs_Selected_by_LASSO[[i]] %in% 
                                          True_Regressors[[i]]))) 

BM1_TPRs = lapply(seq_along(datasets), \(j)
                  j <- (True_Pos_list[[j]]/Total_Positives[[j]]) )