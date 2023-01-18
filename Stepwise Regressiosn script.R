# Spencer Marlen-Starr's RStudio script of functions, methods, code, and 
# code comments required for running Benchmarks 2 & 3, namely the 
# Backward Elimination & Forward Selection versions of Stepwise Regression
# on some of our random synthetic observations to see how well it 
# does so we can compare its results with Dr. Davies' EER procedure.

## This script in its entirety can be run by hitting Ctrl+Alt+R.
#rm(list = ls())
setwd("D:/EER")
getwd()

# load all necessary packages
library(plyr)
library(dplyr)
library(readr)
library(stringi)
library(purrr)
library(stats)
library(leaps)
library(lars)
library(data.table)
library(parallel)


# Extract all of the individual spreadsheet containing workbooks
# in the file folder called 'Data' which is filled
# random synthetic observations to run FS, Stepwise, and eventually EER
# on to compare the results. There are 260k spreadsheets in this folder
fldpth <- "C:/Users/Spencer/Documents/EER Project/Data/csvs/0.25-6-1-1 to 0.25-6-10-500"
paths_list <- list.files(path = fldpth, full.names = T, recursive = T)

# shorten the names of each of the datasets corresponding to 
# each file path in paths_list
DS_names_list <- basename(paths_list)
DS_names_list <- tools::file_path_sans_ext(DS_names_list)


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
DS_names_list = DS_names_list[my_order]
paths_list = paths_list[my_order]


## Import all 260k of the individual csv files below.
## In order to accomplish this, I will be using the readr library in R.
## This line reads all of the data in each of the csv files 
## using the name of each store in the list we just created.
#system.time( datasets <- lapply(paths_list, fread) )
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

system.time(Structural_IVs_chr <- lapply(datasets, function(j) {j[1, -1]}))
system.time(
  True_Regressors <- lapply(Structural_IVs_chr, function(i) 
    { names(i)[i == 1] }))

# change column names of all the columns in the dataframe 'structural_IVs'
Structural_IVs_chr <- lapply(Structural_IVs_chr, function(dataset_i) {
  colnames(dataset_i) <- c("X1","X2", "X3", "X4","X5", "X6", "X7", "X8", "X9", 
                           "X10","X11", "X12", "X13","X14", "X15", "X16","X17",
                           "X18", "X19","X20", "X21", "X22","X23", "X24", "X25", 
                           "X26", "X27", "X28","X29", "X30")
  dataset_i})

# truncate & transform the datasets list before running the regressions
system.time(datasets <- lapply(datasets, function(i) {i[-1:-3, ]}))
system.time(datasets <- lapply(datasets, \(X) { lapply(X, as.numeric) }))
system.time(datasets <- lapply(datasets, function(i) { as.data.table(i) }))

save.image("D:/EER/Saved WorkSpaces/Workspaces for dataset folders starting with '0.25'/datasets WorkSpace for '0.25-6-1-1 to 0.25-6-10-500'.RData")








### Step 2: Run a Backward Elimination Stepwise Regression
### function on each of the 260,000 datasets.
### Assign the full models to their corresponding datasets and
### store these in the object "full_models"
#library(parallel)
#CL <- makeCluster(detectCores() - 4L)
#clusterExport(CL, c('datasets'))
set.seed(11)      # for reproducibility
system.time( BE.fits <- parLapply(cl = CL, X = datasets, \(X) {
  full_models <- lm(X$Y ~ ., X)
  back <- stats::step(full_models, scope = formula(full_models), 
                      direction = 'back', trace = FALSE) }) )

BE_Coeffs <- lapply(seq_along(BE.fits), function(i) coef(BE.fits[[i]]))
#stopCluster(CL)

# extract the names of all IVs selected by them without their intercepts
IVs_Selected_by_BE <- lapply(seq_along(BE.fits), 
                             \(i) names(coef(BE.fits[[i]])[-1]))


### Count up how many Variables Selected match  the true 
### structural equation variables for that dataset in order
### to measure Backward Stepwise's performance.
# the True Positive Rate
num_of_Positives <- lapply(True_Regressors, function(i) { length(i) })

system.time(BE_TPs <- lapply(seq_along(datasets), \(i)
                              sum(IVs_Selected_by_BE[[i]] %in% 
                                    True_Regressors[[i]]))) 
BE_TPRs = lapply(seq_along(datasets), \(j)
                  j <- (BE_TPs[[j]]/num_of_Positives[[j]]) )

# the number of False Positives & True Negative for each Regression
num_of_Negatives <- lapply(True_Regressors, function(i) {30 - length(i)})
BE_FPs <- lapply(seq_along(datasets), \(i)
                 sum(!(IVs_Selected_by_BE[[i]] %in% 
                         True_Regressors[[i]]))) 
BE_TNs <- lapply(seq_along(datasets), \(K) 30 - BE_TPs[[K]])

# the False Positive Rate = FP/(FP + TN)
BE_FPRs = lapply(seq_along(datasets), \(j)
                 j <- (BE_FPs[[j]])/(BE_FPs[[j]] + BE_TNs[[j]]))
BM2_FPRs = lapply(seq_along(datasets), \(i)
                  i <- (BE_FPs[[i]])/(num_of_Negatives[[i]]))

# the True Negative Rate
BE_TNRs <- lapply(BE_FPRs, \(i) 
                   i <- (1 - i))
BM2_TNRs <- lapply(seq_along(datasets), \(j) 
                   j <- (BE_TNs[[j]])/((num_of_Negatives[[j]])))

## Write one or more lines of code which determine whether each selected 
## model is "Underspecified", "Correctly Specified", or "Overspecified".
# True Positive Rates as a vector rather than a list
BM2_TPRs <- unlist(BE_TPRs)
BM2_mean_TPR <- round(mean(BM2_TPRs), 3)
# number of selected regressions with at least one omitted variable
num_OMVs <- sum(BM2_TPRs < 1, na.rm = TRUE)

# False Positive Rate as a vector rather than a list
BM2_FPRs <- unlist(BE_FPRs)
BM2_mean_FPR <- round(mean(BM2_FPRs), 3)
num_null_FPR <- sum(BM2_FPRs == 0, na.rm = TRUE)

# True Negative Rates as a vector rather than a list
BM2_TNRs <- unlist(BE_TNRs)
BM2_mean_TNR <- round(mean(BM2_TNRs), 3)

# number of models with at least one extraneous variable selected
num_Extraneous <- sum(BM2_FPRs > 0)
# Overspecified Regression Specifications Selected by FS
N_Over = sum( (BM2_TPRs > 0) & (BM2_TPRs == 1) & (BM2_TNRs == 1) )
# Number of Underspecified Regression Specifications Selected by BE
N_Under = sum( (BM2_TPRs < 1) & (BM2_FPRs == 0) )
# Number of Correctly Specified Regressions Selected by BE
N_Correct <- sum( (BM2_TPRs == 1) & (BM2_FPRs == 0) & (BM2_TNRs == 1) )

Headers <- c("True Positive Rate", "True Negative Rate", "False Positive Rate")
BM2_PMs1 <- data.frame(BM2_mean_TPR, BM2_mean_TNR, BM2_mean_FPR)
colnames(BM2_PMs1) <- Headers

Headers <- c("Underspecified Models Selected", 
             "Correctly Specified Models Selected",
             "Overspecified Models Selected")
BM2_PMs2 <- data.frame(N_Under, N_Correct, N_Over)
colnames(BM2_PMs2) <- Headers

Headers <- c("Models with at least one Omitted Variable",
             "Models with at least one Extra Variable")
BM2_PMs3 <- data.frame(num_OMVs, num_Extraneous)
colnames(BM2_PMs3) <- Headers

# Or, just print out this instead of having to print out 3 different things
BE_performance_metrics <- list(BM2_PMs1, BM2_PMs2, BM2_PMs3)








### Step 3: Run a Forward Selection Stepwise Regression
### function on each of the 260,000 datasets.
### Assign the null models to their corresponding datasets 
### and store these in the object "nulls".
#CL <- makeCluster(detectCores() - 4L)
#clusterExport(CL, c('datasets'))
set.seed(11)      # for reproducibility
system.time( FS.fits <- parLapply(cl = CL, X = datasets, \(X) {
  nulls <- lm(X$Y ~ 1, X)
  full_models <- lm(X$Y ~ ., X)
  forward <- stats::step(object = nulls, direction = 'forward',
                         scope = formula(full_models), trace = FALSE) }) )

FS_Coeffs <- lapply(seq_along(FS.fits), function(i) coef(FS.fits[[i]]))
#stopCluster(CL)

# assign all regressors selected by Forward Stepwise Regression,
# not including the Intercepts
IVs_Selected_by_FS <- lapply(seq_along(FS.fits), 
                             \(i) names(coef(FS.fits[[i]])[-1]))


### Count up how many Variables Selected match  the true 
### structural equation variables for that dataset in order
### to measure Forward Stepwise's performance.
# the True Positive Rate
system.time(FS_TPs <- lapply(seq_along(datasets), \(i)
                              sum(IVs_Selected_by_FS[[i]] %in% 
                                    True_Regressors[[i]]))) 
FS_TPRs = lapply(seq_along(datasets), \(j)
                  j <- (FS_TPs[[j]]/num_of_Positives[[j]]) )

# the number of total Negatives & False Positives for each Regression
FS_FPs <- lapply(seq_along(datasets), \(i)
                 sum(!(IVs_Selected_by_FS[[i]] %in% 
                         True_Regressors[[i]]))) 
# now calculate the # of True Negatives for each
FS_TNs <- lapply(seq_along(datasets), \(K)  30 - FS_TPs[[K]])
# the False Positive Rate = FP/(FP + TN)
FS_FPRs = lapply(seq_along(datasets), \(j)
                 j <- (FS_FPs[[j]])/(FS_FPs[[j]] + FS_TNs[[j]]))
BM3_FPRs = lapply(seq_along(datasets), \(i)
               i <- (FS_FPs[[i]])/(num_of_Negatives[[i]]))

# the True Negative Rate
FS_TNRs <- lapply(FS_FPRs, \(i)  i <- (1 - i))
#Specificity <- lapply(FS_FPRs, \(z) 
#                      z <- (FS_TNs[[z]])/(FS_TNs[[z]] + FS_FPs[[z]]))
BM3_TNRs <- lapply(seq_along(datasets), \(j) 
                   j <- (FS_TNs[[j]])/((num_of_Negatives[[j]])))
stopCluster(CL)
rm(CL)

## Write one or more lines of code which determine whether each selected 
## model is "Underspecified", "Correctly Specified", or "Overspecified".
# True Positive Rates as a vector rather than a list
BM3_TPRs <- unlist(FS_TPRs)
BM3_mean_TPR <- round(mean(BM3_TPRs), 3)
# number of selected regressions with at least one omitted variable
num_OMVs <- sum(BM3_TPRs < 1, na.rm = TRUE)

# False Positive Rate as a vector rather than a list
BM3_FPRs <- unlist(FS_FPRs)
BM3_mean_FPR <- round(mean(BM3_FPRs), 3)
num_null_FPR <- sum(BM3_FPRs == 0, na.rm = TRUE)

# True Negative Rates as a vector rather than a list
BM3_TNRs <- unlist(FS_TNRs)
BM3_mean_TNR <- round(mean(BM3_TNRs), 3)

# number of models with at least one extraneous variable selected
num_Extraneous <- sum(FS_FPRs > 0)
# Overspecified Regression Specifications Selected by FS
N_Over = sum( (FS_FPRs > 0) & (FS_TPRs == 1) & (FS_TNRs == 1) )
# Number of Underspecified Regression Specifications Selected by FS
N_Under = sum( (FS_TPRs < 1) & (FS_FPRs == 0) )
# Number of Correctly Specified Regressions Selected by FS
N_Correct <- sum( (FS_TPRs == 1) & (FS_FPRs == 0) & (FS_TNRs == 1) )

Headers <- c("True Positive Rate", "True Negative Rate", "False Positive Rate")
BM3_PMs1 <- data.frame(BM3_mean_TPR, BM3_mean_TNR, BM3_mean_FPR)
colnames(BM3_PMs1) <- Headers

Headers <- c("Underspecified Models Selected", 
             "Correctly Specified Models Selected",
             "Overspecified Models Selected")
BM3_PMs2 <- data.frame(N_Under, N_Correct, N_Over)
colnames(BM3_PMs2) <- Headers

Headers <- c("Models with at least one Omitted Variable",
             "Models with at least one Extra Variable")
BM3_PMs3 <- data.frame(num_OMVs, num_Extraneous)
colnames(BM3_PMs3) <- Headers

# Or, just print out this instead of having to print out 3 different things
FS_performance_metrics <- list(BM3_PMs1, BM3_PMs2, BM3_PMs3)

df <- data.frame(BE = BE_performance_metrics, FS = FS_performance_metrics)
write.csv(df, 
          file = "SR's Performance on the DSs from 0.25-6-1-1 to 0.25-6-10-500.csv",
          row.names = FALSE)


## Create a single csv file that has the Regressors selected by both!
write.csv(data.frame(DS_name = DS_names_list, 
                     BE_Regressors_Selected = sapply(IVs_Selected_by_BE, toString), 
                     FS_Regressors_Selected = sapply(IVs_Selected_by_FS, toString)), 
          file = "Regressors Selected by SR for DSs from 0.25-6-1-1 to 0.25-6-10-500.csv",
          row.names = FALSE)


length(datasets)
head(DS_names_list)
tail(DS_names_list)

#write.csv(x = data.frame(first_6_datasets = head(DS_names_list),
#                             last_6_datasets = tail(DS_names_list)), 
#           file = "Dataset range for the 3rd subset of 10k dataset.csv",
#           row.names = FALSE)


