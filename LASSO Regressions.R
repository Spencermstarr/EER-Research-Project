### This script required the user to hit Run either by the button
### in the top right corner of this panel, or by hitting Ctrl+Alt+R

#rm(list = ls())
#setwd("D:/EER")
#setwd("D:/EER folder")
#setwd("C:/Users/Spencer/OneDrive/Documents/Analytics Projects/EER/1st Benchmark")
getwd()
#system.time( load("C:/Users/Spencer/OneDrive/Documents/Analytics Projects/Estimated Exhaustive Regression Project/Saved WorkSpaces/datasets WorkSpace for '0.25-15-1-1 to 0.25-15-10-500'.RData") )

# assign all 30 candidate regressor names to an object
var_names <- c("X1","X2","X3","X4","X5","X6","X7","X8",
               "X9","X10","X11","X12","X13","X14","X15",
               "X16","X17","X18","X19","X20","X21","X22", 
               "X23","X24","X25","X26","X27","X28","X29","X30")


# load all necessary packages
library(plyr)
library(dplyr)
library(stringi)
library(stats)
library(leaps)
library(lars)
library(elasticnet)
library(data.table)
library(parallel)


# these 2 lines together create a simple character list of 
# all the file names in the file folder of datasets you created
folderpath <- "C:/Users/Spencer/Documents/EER Project/Data/top 50"
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
stopCluster(CL)
rm(CL)

# change column names of all the columns in the data.table 'datasets'
datasets <- lapply(datasets, function(dataset_i) { 
  colnames(dataset_i) <- c("Y","X1","X2","X3","X4","X5","X6","X7","X8",
                            "X9","X10","X11","X12","X13","X14","X15",
                            "X16","X17","X18","X19","X20","X21","X22", 
                            "X23","X24","X25","X26","X27","X28","X29","X30")
  dataset_i })

# assign all 30 candidate regressor names to an object
var_names <- c("X1","X2","X3","X4","X5","X6","X7","X8",
               "X9","X10","X11","X12","X13","X14","X15",
               "X16","X17","X18","X19","X20","X21","X22", 
               "X23","X24","X25","X26","X27","X28","X29","X30")


Structural_IVs <- lapply(datasets, function(j) {j[1, -1]})
Structural_Variables <- lapply(Structural_IVs, function(i) {names(i)[i == 1]})
Nonstructural_Variables <- lapply(Structural_IVs, function(i) {names(i)[i == 0]})


# truncate & transform the datasets list before running the regressions
system.time(datasets <- lapply(datasets, function(i) {i[-1:-3, ]}))
system.time(datasets <- lapply(datasets, \(X) { lapply(X, as.numeric) }))
system.time(datasets <- lapply(datasets, function(i) { as.data.table(i) }))

#save.image("C:/Users/Spencer/OneDrive/Documents/Analytics Projects/EER/Saved WorkSpaces/datasets WorkSpace for '0.25-15-1-1 to 0.25-15-10-500'.RData")






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
#stopCluster(CL)
#rm(CL)


### Write my own custom function which will separate out and return a 
### new list containing just the Independent Variables
### which are 'selected' or chosen for each individual dataset.
IVs_Selected <- lapply(LASSO_Coeffs, function(i) names(i[i > 0]))

IVs_Not_Selected <- lapply(LASSO_Coeffs, function(j) names(j[j == 0]))

write.csv(
  data.frame(DS_name = DS_names_list, 
             IVs_selected = sapply(IVs_Selected, toString),
             Structural_Variables = sapply(Structural_Variables, 
                                           toString),
             Nonstructural_Variables = sapply(Nonstructural_Variables, 
                                              toString)),
  file = "LASSO's Selections for the DSs from top 50.csv", 
  row.names = FALSE)





### Count up how many Variables Selected match  the true 
### structural equation variables for that dataset in order
### to measure LASSO's performance.
# all of the "Positives", i.e. all the Structural Regressors
enet_NPs <- lapply(Structural_Variables, function(i) { length(i) })
# all of the "Negatives", i.e. all the Nonstructural Regressors
enet_NNs <- lapply(Structural_Variables, function(i) {30 - length(i)})


# the number True Positives for each LASSO
enet_TPs <- lapply(seq_along(datasets), \(i)
                   sum(IVs_Selected[[i]] %in% 
                         Structural_Variables[[i]]))
# the number True Negatives for each LASSO
enet_TNs <- lapply(seq_along(datasets), \(k)
                   sum(IVs__Not_Selected[[k]] %in% 
                         Nonstructural_Variables[[k]]))

# the number of False Positives
enet_FPs <- lapply(seq_along(datasets), \(i)
                   sum(IVs_Selected[[i]] %in% 
                         Nonstructural_Variables[[i]]))
# the number of False Negatives Selected by each LASSO
enet_FNs <- lapply(seq_along(datasets), \(i)
                   sum(IVs__Not_Selected[[i]] %in% 
                         Structural_Variables[[i]]))


# the True Positive Rate
enet_TPRs = lapply(seq_along(datasets), \(j)
                   j <- (enet_TPs[[j]]/enet_NPs[[j]]))

# the False Positive Rate = FP/(FP + TN)
enet_FPRs = lapply(seq_along(datasets), \(j)
                   j <- (enet_FPs[[j]])/(enet_FPs[[j]] + enet_TNs[[j]]))
enet_FPRs2 = lapply(seq_along(datasets), \(j)
                    j <- (enet_FPs[[j]])/(enet_FPs[[j]] + enet_TNs[[j]]))

# the True Negative Rate
enet_TNRs <- lapply(enet_FPRs, \(i) 
                    i <- (1 - i))
enet_TNRs2 <- lapply(seq_along(datasets), \(w)
                     w <- (enet_TNs[[w]]/(enet_FPs[[w]] + enet_TNs[[w]])))


## Write one or more lines of code which determine whether each selected 
## model is "Underspecified", "Correctly Specified", or "Overspecified".
# True Positive Rates as a vector rather than a list
TPRs <- unlist(enet_TPRs)
mean_TPR <- round(mean(TPRs), 3)
# number of selected regressions with at least one omitted variable
num_OMVs <- sum(TPRs < 1, na.rm = TRUE)

# False Positive Rate as a vector rather than a list
FPRs <- unlist(enet_FPRs)
mean_FPR <- round(mean(FPRs), 3)
num_null_FPR <- sum(FPRs == 0, na.rm = TRUE)

# True Negative Rates as a vector rather than a list
TNRs <- unlist(enet_TNRs)
mean_TNR <- round(mean(TNRs), 3)


# Number of Underspecified Regression Specifications Selected by LASSO
N_Under = sum( (TPRs < 0) & (FPRs == 0) )

# Number of Correctly Specified Regressions Selected by LASSO
N_Correct <- sum( (TPRs == 1) & (TNRs == 1) )

# Overspecified Regression Specifications Selected by LASSO
N_Over = sum( (TPRs == 1) & (FPRs > 0) )
# number of models with at least one extraneous variable selected
num_Extraneous <- sum(FPRs > 0, na.rm = TRUE)

# sum of all the 3 specification categories
Num_Under_Correct_or_Over = N_Under + N_Correct + N_Over


## Create a single table with all model performance metrics in it.
Headers <- c("True Positive Rate", "True Negative Rate", 
             "False Positive Rate")
PMs1 <- data.frame(mean_TPR, mean_TNR, mean_FPR)
colnames(PMs1) <- Headers
rm(mean_TPR, mean_TNR, mean_FPR)

Headers <- c("Underspecified Models Selected", 
              "Correctly Specified Models Selected",
              "Overspecified Models Selected")
PMs2 <- data.frame(N_Under, N_Correct, N_Over)
colnames(PMs2) <- Headers
rm(N_Under, N_Correct, N_Over)

Headers <- c("All Correct, Over, and Underspecified Models", 
             "Models with at least one Omitted Variable",
             "Models with at least one Extra Variable")
PMs3 <- data.frame(Num_Under_Correct_or_Over, num_OMVs, num_Extraneous)
colnames(PMs3) <- Headers
rm(num_OMVs, num_Extraneous)

# Or, just print out this instead of having to print out 3 different things
performance_metrics <- data.frame(PMs1, PMs2, PMs3)
performance_metrics

write.csv(performance_metrics, 
          file = "LASSO's Performance on the datasets from '0.25-15-1-1 to 0.25-15-10-500'.csv", 
          row.names = FALSE)


length(datasets)
head(DS_names_list)
tail(DS_names_list)
#write.csv(x = data.frame(first_6_datasets = head(DS_names_list), 
#                         last_6_datasets = tail(DS_names_list)), 
#          file = "Dataset range for 'top 50'.csv", 
#          row.names = FALSE)
length(datasets)
#rm(CL, Structural_Variables)
#rm(IVs_Selected, LASSO_fits, LASSO_Coeffs)
#rm(BM1_FPRs, BM1_FPs, BM1_NNs, BM1_NPs, BM1_TNRs, BM1_TPRs, BM1_TPs)
#rm(FPRs, TPRs, TNRs, Headers, num_null_FPR)

length(datasets)
#save.image("D:/EER/Saved WorkSpaces/datasets WorkSpace for '0.25-15-1-1 to 0.25-15-10-500'.RData")
#rm(performance_metrics, PMs1, PMs2, PMs3)

