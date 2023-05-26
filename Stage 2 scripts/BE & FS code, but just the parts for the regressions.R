### Spencer Marlen-Starr's RStudio script of functions, methods, code, and 
### code comments required for running Benchmarks 2 & 3, namely the 
### Backward Elimination & Forward Selection versions of Stepwise Regression
### on some of our random synthetic observations to see how well it 
### does so we can compare its results with Dr. Davies' EER procedure.

## This script in its entirety can be run by the user by hitting Ctrl+Alt+R.
rm(list = ls())
#setwd("D:/EER")
#setwd("D:/EER folder")
getwd()
#system.time( load("C:/Users/Spencer/OneDrive/Documents/Analytics Projects/EER Project/Saved WorkSpaces/datasets WorkSpace for '0.5-7-1-1 to 0.5-7-10-500'.RData") )
system.time( load("C:/Users/Spencer/OneDrive/Documents/Analytics Projects/EER Project/Saved WorkSpaces/Workspaces for dataset folders starting with '0.5'/datasets WorkSpace for '0.5-7-1-1 to 0.5-7-10-500'.RData") )
# assign all 30 candidate regressor names to an object
var_names <- c("X1","X2","X3","X4","X5","X6","X7","X8",
               "X9","X10","X11","X12","X13","X14","X15",
               "X16","X17","X18","X19","X20","X21","X22", 
               "X23","X24","X25","X26","X27","X28","X29","X30")
length(datasets)
head(DS_names_list)
tail(DS_names_list)


# load all necessary packages
library(plyr)
library(dplyr)
library(stringi)
library(purrr)
library(stats)
library(leaps)
library(lars)
library(data.table)
library(parallel)



### Step 2: Run a Backward Elimination Stepwise Regression
### function on each of the 260,000 datasets.
### Assign the full models to their corresponding datasets and
### store these in the object "full_models"
CL <- makeCluster(detectCores() - 5L)
system.time( clusterExport(CL, c('datasets')) )
set.seed(11)      # for reproducibility
system.time( BE.fits <- parLapply(cl = CL, X = datasets, \(ds_i) {
  full_models <- lm(ds_i$Y ~ ., data = ds_i)
  back <- stats::step(object = full_models, scope = formula(full_models), 
                      direction = 'back', trace = FALSE) }) )
#system.time( BE.fits <- lapply(X = datasets, \(ds_i) {
#  full_models <- lm(ds_i$Y ~ ., data = ds_i)
#  back <- stats::step(object = full_models, scope = formula(full_models), 
#                      direction = 'back', trace = FALSE) }) )
stopCluster(CL)
rm(CL)

# Extract the names of all IVs selected (besides their intercepts) by each 
# of the 260k Backward Elimination Stepwise Regressions estimated above 
# and store those estimates in the list object 'IVs_Selected_by_BE'
IVs_Selected_by_BE <- lapply(seq_along(BE.fits), 
                             \(i) names(coef(BE.fits[[i]])[-1]))

BE_IV_Candidates <- function(var_names, IVs_Selected_by_BE) {
  Candidate_Vars <- setdiff(var_names, IVs_Selected_by_BE)
  return(Candidate_Vars)}
# now do the same but for the candidate factors NOT selected
IVs_Not_Selected_by_BE <- lapply(seq_along(BE.fits), \(j)
                                 j <- (BE_IV_Candidates(var_names, IVs_Selected_by_BE[[j]])))




### Count up how many Variables Selected match  the true 
### structural equation variables for that dataset in order
### to measure Backward Stepwise's performance.
# the True Positive Rate
num_of_Positives <- lapply(Structural_Variables, function(i) { length(i) })

BE_TPs_list <- lapply(seq_along(datasets), \(i)
                             sum(IVs_Selected_by_BE[[i]] %in% 
                                   Structural_Variables[[i]]))

BE_TPRs_list = lapply(seq_along(datasets), \(j)
                 j <- (BE_TPs_list[[j]]/num_of_Positives[[j]]) )

# the number of False Positives & True Negative for each Regression
num_of_Negatives <- lapply(Structural_Variables, function(i) {30 - length(i)})
BE_FPs_list <- lapply(seq_along(datasets), \(i)
                 sum(!(IVs_Selected_by_BE[[i]] %in% 
                         Structural_Variables[[i]]))) 
BE_TNs_list <- lapply(seq_along(datasets), \(K) 30 - BE_TPs_list[[K]])

# the False Positive Rate = FP/(FP + TN)
BE_FPRs_list = lapply(seq_along(datasets), \(j)
                 j <- (BE_FPs_list[[j]])/(BE_FPs_list[[j]] + BE_TNs_list[[j]]))
BE_FPRs_list2 = lapply(seq_along(datasets), \(i)
                  i <- (BE_FPs_list[[i]])/(num_of_Negatives[[i]]))

# the True Negative Rate
BE_TNRs_list <- lapply(seq_along(datasets), \(j) 
                        j <- (BE_TNs_list[[j]])/((num_of_Negatives[[j]])))
BE_TNRs_list2 <- lapply(BE_FPRs_list, \(i) 
                  i <- (1 - i))



## Write one or more lines of code which determine whether each selected 
## model is "Underspecified", "Correctly Specified", or "Overspecified".
# True Positive Rates as a vector rather than a list
BE_TPRs <- unlist(BE_TPRs_list)
BE_mean_TPR <- round(mean(BE_TPRs), 3)
# number of selected regressions with at least one omitted variable
BE_num_OMVs <- sum(BE_TPRs < 1, na.rm = TRUE)

# False Positive Rate as a vector rather than a list
BE_FPRs <- unlist(BE_FPRs_list)
BE_mean_FPR <- round(mean(BE_FPRs), 3)
BE_num_null_FPR <- sum(BE_FPRs == 0, na.rm = TRUE)
# number of models with at least one extraneous variable selected
BE_num_Extraneous <- sum(BE_FPRs > 0)

# True Negative Rates as a vector rather than a list
BE_TNRs <- unlist(BE_TNRs_list)
BE_mean_TNR <- round(mean(BE_TNRs), 3)


# Number of Underspecified Regression Specifications Selected by BE
BN_Under = sum( (BE_TPRs < 1) & (BE_FPRs == 0) )
# Number of Correctly Specified Regressions Selected by BE
BN_Correct <- sum( (BE_TPRs == 1) & (BE_TNRs == 1) )
# Overspecified Regression Specifications Selected by FS
BN_Over = sum( (BE_TPRs == 1) & (BE_FPRs > 0) )

# sum of all the 3 specification categories
BE_Num_Un_Corr_Ov = BN_Under + BN_Correct + BN_Over

# create dataframes to store the performance metrics in
Headers <- c("True Positive Rate", "True Negative Rate", 
             "False Positive Rate")
BE_PMs1 <- data.frame(BE_mean_TPR, BE_mean_TNR, BE_mean_FPR)
colnames(BE_PMs1) <- Headers

Headers <- c("Underspecified Models Selected", 
             "Correctly Specified Models Selected",
             "Overspecified Models Selected")
BE_PMs2 <- data.frame(BN_Under, BN_Correct, BN_Over)
colnames(BE_PMs2) <- Headers

Headers <- c("All Correct, Over, and Underspecified Models",
             "Models with at least one Omitted Variable",
             "Models with at least one Extra Variable")
BE_PMs3 <- data.frame(BE_Num_Un_Corr_Ov, BE_num_OMVs, BE_num_Extraneous)
colnames(BE_PMs3) <- Headers

# Or, just print out this instead of having to print out 3 different things
BE_performance_metrics <- list(BE_PMs1, BE_PMs2, BE_PMs3)

BE_df <- data.frame(BE = BE_performance_metrics)
write.csv(BE_df, 
          file = "BE's Performance on the DSs from 0.5-7-1-1 to 0.5-7-10-500.csv",
          row.names = FALSE)











### Step 3: Run a Forward Selection Stepwise Regression
### function on each of the 260,000 datasets.
### Assign the null models to their corresponding datasets 
### and store these in the object "nulls".
CL <- makeCluster(detectCores() - 2L)
system.time( clusterExport(CL, c('datasets')) )
set.seed(11)      # for reproducibility
system.time( FS.fits <- parLapply(cl = CL, X = datasets, \(ds_i) {
  nulls <- lm(ds_i$Y ~ 1, data = ds_i)
  full_models <- lm(ds_i$Y ~ ., data = ds_i)
  forward <- stats::step(object = nulls, scope = formula(full_models), 
                         direction = 'forward', trace = FALSE) }) )
stopCluster(CL)
rm(CL)

set.seed(11)      # for reproducibility
system.time( FS.fits <- lapply(X = datasets, \(ds_i) {
  nulls <- lm(ds_i$Y ~ 1, data = ds_i)
  full_models <- lm(ds_i$Y ~ ., data = ds_i)
  forward <- stats::step(object = nulls, scope = formula(full_models), 
                         direction = 'forward', trace = FALSE) }) )
#stopCluster(CL)
#rm(CL)


# assign all regressors selected by Forward Stepwise Regression,
# not including the Intercepts
IVs_Selected_by_FS <- lapply(seq_along(FS.fits), 
                             \(i) names(coef(FS.fits[[i]])[-1]))

FS_IV_Candidates <- function(var_names, IVs_Selected_by_FS) {
  Candidate_Vars <- setdiff(var_names, IVs_Selected_by_FS)
  return(Candidate_Vars)}

IVs_Not_Selected_by_FS <- lapply(seq_along(FS.fits), \(j)
                                 j <- (FS_IV_Candidates(var_names, IVs_Selected_by_FS[[j]])))



### Count up how many Variables Selected match  the true 
### structural equation variables for that dataset in order
### to measure Forward Stepwise's performance.
# the True Positive Rate
FS_TPs_list <- lapply(seq_along(datasets), \(i)
                             sum(IVs_Selected_by_FS[[i]] %in% Structural_Variables[[i]]))

FS_TPRs_list = lapply(seq_along(datasets), \(j)
                 j <- (FS_TPs_list[[j]]/num_of_Positives[[j]]) )

# the number of False Positives
FS_FPs_list <- lapply(seq_along(datasets), \(i)
                      sum(IVs_Selected_by_FS[[i]] %in% Nonstructural_Variables[[i]]))

# now calculate the # of True Negatives for each
FS_TNs_list <- lapply(seq_along(datasets), \(K)  30 - FS_TPs_list[[K]])

# the False Positive Rate = FP/(FP + TN)
FS_FPRs_list = lapply(seq_along(datasets), \(j)
                 j <- (FS_FPs_list[[j]])/(FS_FPs_list[[j]] + FS_TNs_list[[j]]))

# the True Negative Rate
FS_TNRs_list <- lapply(FS_FPRs_list, \(i)  i <- (1 - i))


## Write one or more lines of code which determine whether each selected 
## model is "Underspecified", "Correctly Specified", or "Overspecified".
# True Positive Rates as a vector rather than a list
FS_TPRs <- unlist(FS_TPRs_list)
FS_mean_TPR <- round(mean(FS_TPRs), 3)
# number of selected regressions with at least one omitted variable
FS_num_OMVs <- sum(FS_TPRs < 1, na.rm = TRUE)

# False Positive Rate as a vector rather than a list
FS_FPRs <- unlist(FS_FPRs_list)
FS_mean_FPR <- round(mean(FS_FPRs), 3)
FS_num_null_FPR <- sum(FS_FPRs == 0, na.rm = TRUE)
# number of models with at least one extraneous variable selected
FS_num_Extraneous <- sum(FS_FPRs > 0)

# True Negative Rates as a vector rather than a list
FS_TNRs <- unlist(FS_TNRs_list)
FS_mean_TNR <- round(mean(FS_TNRs_list), 3)


# Number of Underspecified Regression Specifications Selected by FS
FN_Under = sum( (FS_TPRs < 1) & (FS_FPRs == 0) )
# Number of Correctly Specified Regressions Selected by FS
FN_Correct <- sum( (FS_TPRs == 1) & (FS_TNRs == 1) )
# Overspecified Regression Specifications Selected by FS
FN_Over = sum( (FS_TPRs == 1) & (FS_FPRs > 0) )

# sum of all the 3 specification categories
FS_Num_Un_Corr_Ov = FN_Under + FN_Correct + FN_Over


Headers <- c("True Positive Rate", "True Negative Rate", 
             "False Positive Rate")
FS_PMs1 <- data.frame(FS_mean_TPR, FS_mean_TNR, FS_mean_FPR)
colnames(FS_PMs1) <- Headers

Headers <- c("Underspecified Models Selected", 
             "Correctly Specified Models Selected",
             "Overspecified Models Selected")
FS_PMs2 <- data.frame(FN_Under, FN_Correct, FN_Over)
colnames(FS_PMs2) <- Headers

Headers <- c("All Correct, Over, and Underspecified Models", 
             "Models with at least one Omitted Variable",
             "Models with at least one Extra Variable")
FS_PMs3 <- data.frame(FS_Num_Un_Corr_Ov, FS_num_OMVs, FS_num_Extraneous)
colnames(FS_PMs3) <- Headers

# Or, just print out this instead of having to print out 3 different things
FS_performance_metrics <- list(FS_PMs1, FS_PMs2, FS_PMs3)

FS_df <- data.frame(FS = FS_performance_metrics)
write.csv(FS_df, 
          file = "FS's Performance on the DSs from 0.5-7-1-1 to 0.5-7-10-500.csv", 
          row.names = FALSE)
length(BE_performance_metrics)
length(FS_performance_metrics)

df <- data.frame(BE = BE_performance_metrics, FS = FS_performance_metrics)
write.csv(df, 
          file = "SR's Performance on the DSs from 0.5-7-1-1 to 0.5-7-10-500.csv", 
          row.names = FALSE)

## Create a single csv file that has the Regressors selected by both!
write.csv(data.frame(DS_name = DS_names_list, 
                     Regressors_Selected_by_BE = sapply(IVs_Selected_by_BE, toString), 
                     Regressors_Not_Selected_by_BE = sapply(IVs_Not_Selected_by_BE, toString),
                     Regressors_Selected_by_FS = sapply(IVs_Selected_by_FS, toString),
                     Regressors_Not_Selected_by_FS = sapply(IVs_Not_Selected_by_FS, toString),
                     Structural_Variables = sapply(Structural_Variables, 
                                                   toString),
                     Nonstructural_Variables = sapply(Nonstructural_Variables, 
                                                      toString)),
          file = "Regressors Selected by SR for DSs from 0-12-1-1 to 0-12-10-500.csv", 
          row.names = FALSE)





length(datasets)
head(DS_names_list)
tail(DS_names_list)
#write.csv(x = data.frame(first_6_datasets = head(DS_names_list),
#                             last_6_datasets = tail(DS_names_list)), 
#           file = "Dataset range for 0.datasets WorkSpace for '0.5-7-1-1 to 0.5-7-10-500'.csv",
#           row.names = FALSE)

#save.image("D:/EER/Saved WorkSpaces/BE & FS WorkSpace for the 10k '0.5-7-1-1 to 0.5-7-10-500' datasets.RData")
#save.image("D:/EER folder/WorkSpaces/BE & FS WorkSpace for the 10k '0.5-7-1-1 to 0.5-7-10-500' datasets.RData")
