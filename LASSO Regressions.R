# This script required the user to hit Run either by the button
# in the top right corner of this panel, or by hitting Ctrl+Alt+R
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
library(elasticnet)
library(data.table)
library(parallel)


# these 2 lines together create a simple character list of 
# all the file names in the file folder of datasets you created
folderpath <- "C:/Users/Spencer/Documents/EER Project/csvs/top 100"
paths_list <- list.files(path = folderpath, full.names = T, recursive = T)

# shorten the names of each of the datasets corresponding to 
# each file path in paths_list
DS_names_list <- basename(paths_list)
DS_names_list <- tools::file_path_sans_ext(DS_names_list)

# sort both of the list of file names so that they are in the proper order
my_order = DS_names_list |> 
  # split apart the listed numbers, convert them to numeric 
  strsplit(split = "-", fixed = TRUE) |>  unlist() |> as.numeric() |>
  # get them in a data frame
  matrix(nrow = length(DS_names_list), byrow = TRUE) |> as.data.frame() |>
  # get the appropriate ordering to sort the data frame
  do.call(order, args = _)
DS_names_list = DS_names_list[my_order]
paths_list = paths_list[my_order]


# this line reads all of the data in each of the csv files 
# using the name of each store in the list we just created
library(parallel)
CL <- parallel::makeCluster(detectCores() - 4L)
parallel::clusterExport(CL, c('paths_list'))
system.time(datasets <- parLapply(cl = CL, X = paths_list, 
                                  fun = data.table::fread))
#stopCluster(CL)

# change column names of all the columns in the data.table 'datasets'
datasets <- lapply(datasets, function(dataset_i) { 
  colnames(dataset_i) <- c("Y","X1","X2","X3","X4","X5","X6","X7","X8",
                            "X9","X10","X11","X12","X13","X14","X15",
                            "X16","X17","X18","X19","X20","X21","X22", 
                            "X23","X24","X25","X26","X27","X28","X29","X30")
  dataset_i })

system.time(Structural_IVs_chr <- lapply(datasets, function(j) {j[1, -1]}))
True_Regressors <- lapply(Structural_IVs_chr, function(i) {
  names(i)[i == 1] })

# truncate & transform the datasets list before running the regressions
system.time(datasets <- lapply(datasets, function(i) {i[-1:-3, ]}))
system.time(datasets <- lapply(datasets, \(X) { lapply(X, as.numeric) }))
system.time(datasets <- lapply(datasets, function(i) { as.data.table(i) }))





# This function fits all 260,000 LASSO regressions for/on
# each of the corresponding 260k datasets stored in the object
# of that name, then outputs standard regression results which 
# are typically called returned for any regression ran using R.
library(elasticnet)
set.seed(11)     # to ensure replicability
system.time(LASSO_fits <- lapply(X = datasets, function(i) 
               elasticnet::enet(x = as.matrix(dplyr::select(i, 
                                         starts_with("X"))), 
               y = i$Y, lambda = 0, normalize = FALSE)))

## This stores and prints out all of the regression 
## equation specifications selected by LASSO when called.
# Press Run or hit Ctrl+Enter only once for the 5 lines below
LASSO_Coeffs <- lapply(LASSO_fits, 
                          function(i) predict(i, 
                                           x = as.matrix(dplyr::select(i, starts_with("X"))), 
                                           s = 0.1, mode = "fraction", 
                                           type = "coefficients")[["coefficients"]])
stopCluster(CL)

### Write my own custom function which will separate out and return a 
### new list containing just the Independent Variables
### which are 'selected' or chosen for each individual dataset.
IVs_Selected_by_LASSO <- lapply(LASSO_Coeffs, function(i) names(i[i > 0]))

write.csv(
  data.frame(DS_name = DS_names_list, 
             IVs_selected = sapply(IVs_Selected_by_LASSO, toString)), 
  file = "LASSO's Selections for the top 100.csv", 
  row.names = FALSE)





### Count up how many Variables Selected match  the true 
### structural equation variables for that dataset in order
### to measure LASSO's performance.
BM1_NPs <- lapply(True_Regressors, function(i) { length(i) })

system.time(BM1_TPs <- lapply(seq_along(datasets), \(i)
                              sum(IVs_Selected_by_LASSO[[i]] %in% 
                                    True_Regressors[[i]]))) 

BM1_TPRs = lapply(seq_along(datasets), \(j)
                  j <- (BM1_TPs[[j]]/BM1_NPs[[j]]) )

# the number of False Positives & True Negative for each Regression
BM1_NNs <- lapply(True_Regressors, function(i) {30 - length(i)})
BM1_FPs <- lapply(seq_along(datasets), \(i)
                  sum(!(IVs_Selected_by_LASSO[[i]] %in% 
                          True_Regressors[[i]]))) 
BM1_TNs <- lapply(seq_along(datasets), \(K) 30 - BM1_TPs[[K]])
# the False Positive Rate = FP/(FP + TN)
BM1_FPRs = lapply(seq_along(datasets), \(j)
                 j <- (BM1_FPs[[j]])/(BM1_FPs[[j]] + BM1_TNs[[j]]))

# the True Negative Rate
BM1_TNRs <- lapply(BM1_FPRs, \(i) 
                   i <- (1 - i))


## Write one or more lines of code which determine whether each selected 
## model is "Underspecified", "Correctly Specified", or "Overspecified".
# True Positive Rates as a vector rather than a list
TPRs <- unlist(BM1_TPRs)
mean_TPR <- round(mean(TPRs), 3)
# number of selected regressions with at least one omitted variable
num_OMVs <- sum(TPRs < 1, na.rm = TRUE)

# False Positive Rate as a vector rather than a list
FPRs <- unlist(BM1_FPRs)
mean_FPR <- round(mean(FPRs), 3)
num_null_FPR <- sum(FPRs == 0, na.rm = TRUE)
# number of models with at least one extraneous variable selected
num_Extraneous <- sum(FPRs > 0)

# True Negative Rates as a vector rather than a list
TNRs <- unlist(BM1_TNRs)
mean_TNR <- round(mean(TNRs), 3)


# Number of Underspecified Regression Specifications Selected by LASSO
N_Under = sum( (TPRs < 1) & (FPRs == 0) )

# Number of Correctly Specified Regressions Selected by LASSO
N_Correct <- sum( (TPRs == 1) & (FPRs == 0) & (TNRs == 1) )

# Overspecified Regression Specifications Selected by LASSO
N_Over = sum( (FPRs > 0) & (TPRs == 1) & (TNRs == 1) )


Headers <- c("Mean True Positive Rate", "Mean True Negative Rate", 
             "Mean False Positive Rate")
PMs1 <- data.frame(mean_TPR, mean_TNR, mean_FPR)
colnames(PMs1) <- Headers
rm(mean_TPR, mean_TNR, mean_FPR)

Headers <- c("Underspecified Models Selected", 
              "Correctly Specified Models Selected",
              "Overspecified Models Selected")
PMs2 <- data.frame(N_Under, N_Correct, N_Over)
colnames(PMs2) <- Headers
rm(N_Under, N_Correct, N_Over)

Headers <- c("Models with at least one Omitted Variable",
              "Models with at least one Extra Variable")
PMs3 <- data.frame(num_OMVs, num_Extraneous)
colnames(PMs3) <- Headers
rm(num_OMVs, num_Extraneous)

# Or, just print out this instead of having to print out 3 different things
performance_metrics <- list(PMs1, PMs2, PMs3)

write.csv(performance_metrics, 
          file = "LASSO's Performance on the top 100.csv", 
          row.names = FALSE)

length(datasets)
head(DS_names_list)
tail(DS_names_list)
#write.csv(x = data.frame(first_6_datasets = head(DS_names_list), 
#                         last_6_datasets = tail(DS_names_list)), 
#          file = "Dataset range for 'top 100'.csv", 
#          row.names = FALSE)
length(datasets)
rm(CL, Structural_IVs_chr)
rm(IVs_Selected_by_LASSO, LASSO_fits, LASSO_Coeffs)
rm(BM1_FPRs, BM1_FPs, BM1_NNs, BM1_NPs, BM1_TNRs, BM1_TPRs, BM1_TPs)
rm(FPRs, TPRs, TNRs, Headers, num_null_FPR)

length(datasets)
save.image("D:/EER/Saved WorkSpaces/datasets WorkSpace for top 100.RData")
rm(performance_metrics, PMs1, PMs2)
rm(PMs3)
