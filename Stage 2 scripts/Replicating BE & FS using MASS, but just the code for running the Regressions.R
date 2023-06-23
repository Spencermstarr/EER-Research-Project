# Spencer Marlen-Starr's RStudio script of functions, methods, code, and 
# code comments required for running Benchmarks 2 & 3, namely the 
# Backward Elimination & Forward Selection versions of Stepwise Regression
# on some of our random synthetic observations to see how well it 
# does so we can compare its results with Dr. Davies' EER procedure.

## This script in its entirety can be run by hitting Ctrl+Alt+R.
#rm(list = ls())
#setwd("C:/Users/Spencer/Documents/EER Project")
#setwd("D:/EER folder")
#setwd("D:/EER")
getwd()
#system.time( load("C:/Users/Spencer/OneDrive/Documents/Analytics Projects/EER Project/Saved WorkSpaces/Workspaces for dataset folders starting with '0'/datasets WorkSpace for '0-3-1-1 to 0-3-10-500'.RData") )
system.time( load("C:/Users/Spencer/OneDrive/Documents/Analytics Projects/EER Project/Saved WorkSpaces/Workspaces for dataset folders starting with '0'/loaded WorkSpace for datasets from '0-3-1-1 to 0-3-10-500'.RData") )


# load all necessary packages
library(plyr)
library(dplyr)
library(readr)
library(stringi)
library(purrr)
library(stats)
library(MASS)
library(leaps)
library(lars)
library(parallel)
library(data.table)



### Step 2: Run a Backward Elimination Stepwise Regression
### function on each of the 260,000 datasets.
#CL <- makeCluster(detectCores() - 3L)
#system.time( clusterExport(CL, c('datasets')) )
#set.seed(11)      # for reproducibility
#time_to_fit_BE <- system.time( BE.fits <- parLapply(cl = CL, X = datasets, \(ds_i) {
#  null_models <- lm(ds_i$Y ~ 1, ds_i)
#  full_models <- lm(ds_i$Y ~ ., data = ds_i)
#  back <- MASS::stepAIC(full_models, direction = 'backward', 
#                        scope = list(upper = full_models, lower = null_models), 
#                        trace = FALSE) }) )
#time_to_fit_BE

set.seed(11)
time_to_fit_BE <- system.time( BE.fits <- lapply(X = datasets, \(ds_i) {
  null_models <- lm(ds_i$Y ~ 1, ds_i)
  full_models <- lm(ds_i$Y ~ ., data = ds_i)
  back <- MASS::stepAIC(full_models, direction = 'backward', 
                        scope = list(upper = full_models, lower = null_models), 
                        trace = FALSE) }) )
time_to_fit_BE

# Extract the elapsed time
BE_elapsed_time <- time_to_fit_BE["elapsed"]


# extract the coefficients and their corresponding variable names
set.seed(11)
BE_Coeffs <- lapply(seq_along(BE.fits), function(i) coef(BE.fits[[i]]))
#stopCluster(CLs)
#rm(CL)

# extract the names of all IVs selected by them without their intercepts
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
# the number of all 'positives', i.e., structural factors
num_of_Positives_list <- lapply(Structural_Variables, function(i) { length(i) })

# all of the "Negatives", i.e. all the Nonstructural Regressors
num_of_Negatives_list <- lapply(Nonstructural_Variables, function(i) { length(i) })

BE_TPs_list <- lapply(seq_along(datasets), \(i)
                              sum(IVs_Selected_by_BE[[i]] %in% 
                                    Structural_Variables[[i]]))


## the number of False Positives, True Negatives, & False Negatives for each Regression
BE_FPs_list <- lapply(seq_along(datasets), \(i)
                 sum(!(IVs_Selected_by_BE[[i]] %in% 
                         Structural_Variables[[i]]))) 

BE_TNs_list <- lapply(seq_along(datasets), \(K) 30 - BE_TPs_list[[K]])

BE_FNs_list <- lapply(seq_along(datasets), \(i)
                      sum(IVs_Not_Selected_by_BE[[i]] %in% 
                            Structural_Variables[[i]]))

# the sensitivity
BE_TPRs_list = lapply(seq_along(datasets), \(j)
                 j <- (BE_TPs_list[[j]]/num_of_Positives_list[[j]]) )

# the False Positive Rate = FP/(FP + TN)
BE_FPRs_list = lapply(seq_along(datasets), \(j)
                 j <- (BE_FPs_list[[j]])/(BE_FPs_list[[j]] + BE_TNs_list[[j]]))
BM2_FPRs_list = lapply(seq_along(datasets), \(i)
                  i <- (BE_FPs_list[[i]])/(num_of_Negatives_list[[i]]))

# the True Negative Rate
BE_TNRs_list <- lapply(BE_FPRs_list, \(i) 
                   i <- (1 - i))
BM2_TNRs_list <- lapply(seq_along(datasets), \(j) 
                   j <- (BE_TNs_list[[j]])/((num_of_Negatives_list[[j]])))

# the False Negative Rate = FN/(FN + TP)
BE_FNRs_list = lapply(seq_along(datasets), \(j)
                      j <- (BE_FNs_list[[j]])/(BE_FNs_list[[j]] + BE_TPs_list[[j]]))

## calculate the accuracy and F1 score with help from GPT 4
BE_Accuracy_list <- lapply(seq_along(datasets), function(i)
  (BE_TPs_list[[i]] + BE_TNs_list[[i]])/(BE_TPs_list[[i]] + BE_TNs_list[[i]] + BE_FPs_list[[i]] + BE_FNs_list[[i]]))

# First calculate precision and TPR for each dataset
BE_Precision_list <- lapply(seq_along(datasets), function(i)
  BE_TPs_list[[i]]/(BE_TPs_list[[i]] + BE_FPs_list[[i]]))

# Then calculate F1 score for each dataset
BE_F1_Score_list <- lapply(seq_along(datasets), function(i)
  2 * (BE_Precision_list[[i]] * BE_TPRs_list[[i]])/(BE_Precision_list[[i]] + BE_TPRs_list[[i]]))


## Write one or more lines of code which determine whether each selected 
## model is "Underspecified", "Correctly Specified", or "Overspecified".
# True Positive Rates as a vector rather than a list
BM2_TPRs <- unlist(BE_TPRs_list)
BM2_mean_TPR <- round(mean(BM2_TPRs), 3)

# False Positive Rate as a vector rather than a list
BM2_FPRs <- unlist(BE_FPRs_list)
BM2_mean_FPR <- round(mean(BM2_FPRs), 3)
num_null_FPR <- sum(BM2_FPRs == 0, na.rm = TRUE)

# True Negative Rates as a vector rather than a list
BM2_TNRs <- unlist(BE_TNRs_list)
BM2_mean_TNR <- round(mean(BM2_TNRs), 3)

# False Negative Rates as a vector rather than a list
BM2_FNRs <- unlist(BE_FNRs_list)
BM2_mean_FNR <- round(mean(BM2_FNRs), 3)

# The PPVs/precision as a vector rather than a list
BM2_PPVs <- unlist(BE_Precision_list)
BM2_mean_PPV <- round(mean(BM2_PPVs), 3)

# The Accuracy as a vector rather than a list
BM2_Accs <- unlist(BE_Accuracy_list)
BM2_mean_Accuracy <- round(mean(BM2_Accs), 3)

# The F1 Scores as a vector rather than a list
BM2_F1s <- unlist(BE_F1_Score_list)
BM2_mean_F1_Score <- round(mean(BM2_F1s), 3)


# the number of selected regressions with at least one omitted variable
num_OMVs <- sum(BM2_TPRs < 1, na.rm = TRUE)
# the number of selected regressions with at least two structural variables
# omitted, i.e., 2 False Negatives, and no False Positives.  
#two_OMVs <- sum(BM2_FNRs > 0 & lapply(datasets, \(i)
#                                      length(IVs_Selected_by_BE) < length(Structural_Variables))

# the number of models selected with at least one extraneous variable selected
num_Extraneous <- sum(BM2_FPRs > 0, na.rm = TRUE)

# Overspecified Regression Specifications Selected by FS
N_Over = sum( (BM2_FPRs > 0) & (BM2_TPRs == 1) )
# Number of Underspecified Regression Specifications Selected by BE
N_Under = sum( (BM2_TPRs < 1) & (BM2_FPRs == 0) )
# Number of Correctly Specified Regressions Selected by BE
N_Correct <- sum( (BM2_TPRs == 1) & (BM2_FPRs == 0) & (BM2_TNRs == 1) )


# create dataframes to store the performance metrics in
Headers <- c("Runtime", "Mean Accuracy", "Mean F1 Score", 
             "Mean Positive Predictive Value", "Mean True Positive Rate", 
             "Mean True Negative Rate", "Mean False Positive Rate", "Mean False Negative Rate")
BM2_PMs1 <- data.frame(BE_elapsed_time, BE_mean_Accuracy, BE_mean_F1_Score, BE_mean_PPV, 
                      BE_mean_TPR, BE_mean_TNR, BE_mean_FPR, BE_mean_FNR)
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
#CL <- makeCluster(detectCores() - 3L)
#clusterExport(CL, c('datasets'))
#set.seed(11)
#time_to_fit_FS <- system.time( FS_fits <- parLapply(cl = CL, X = datasets, \(ds_i) {
#  null_models <- lm(ds_i$Y ~ 1, ds_i)
#  full_models <- lm(ds_i$Y ~ ., data = ds_i)
#  forward <- MASS::stepAIC(null_models, direction = 'forward', 
#                           scope = list(lower = null_models,
#                                        upper = full_models), 
#                           trace = FALSE) }) )
#time_to_fit_FS

set.seed(11)
time_to_fit_FS <- system.time( FS_fits <- lapply(X = datasets, \(ds_i) {
  null_models <- lm(ds_i$Y ~ 1, ds_i)
  full_models <- lm(ds_i$Y ~ ., data = ds_i)
  forward <- MASS::stepAIC(null_models, direction = 'forward', 
                           scope = list(lower = null_models,
                                        upper = full_models), 
                           trace = FALSE) }) )
time_to_fit_FS

# Extract the elapsed time
FS_elapsed_time <- time_to_fit_FS["elapsed"]

#stopCluster(CL)
#rm(CL)

set.seed(11)
FS_Coeffs <- lapply(seq_along(FS_fits), function(i) coef(FS_fits[[i]]))


# assign all regressors selected by Forward Stepwise Regression,
# not including the Intercepts, to IVs_Selected_by_FS
IVs_Selected_by_FS <- lapply(seq_along(FS_fits), 
                             \(i) names(coef(FS_fits[[i]])[-1]))

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
                              sum(IVs_Selected_by_FS[[i]] %in% 
                                    Structural_Variables[[i]]))

# the number of total Negatives & False Positives for each Regression
FS_FPs_list <- lapply(seq_along(datasets), \(i)
                 sum(!(IVs_Selected_by_FS[[i]] %in% 
                         Structural_Variables[[i]]))) 
# now calculate the # of True Negatives for each
FS_TNs_list <- lapply(seq_along(datasets), \(K)  30 - FS_TPs_list[[K]])
# calculate the # of False Negatives in each selected model
FS_FNs_list <- lapply(seq_along(datasets), \(i)
                 sum(IVs_Not_Selected_by_FS[[i]] %in%
                       Structural_Variables))

# the True Positive Rate
FS_TPRs_list = lapply(seq_along(datasets), \(j)
                 j <- (FS_TPs_list[[j]]/num_of_Positives_list[[j]]) )

# the False Positive Rate = FP/(FP + TN)
FS_FPRs_list = lapply(seq_along(datasets), \(j)
                 j <- (FS_FPs_list[[j]])/(FS_FPs_list[[j]] + FS_TNs_list[[j]]))
BM3_FPRs_list = lapply(seq_along(datasets), \(i)
               i <- (FS_FPs_list[[i]])/(num_of_Negatives_list[[i]]))

# the True Negative Rate
FS_TNRs_list <- lapply(FS_FPRs_list, \(i)  i <- (1 - i))
BM3_TNRs_list <- lapply(seq_along(datasets), \(j) 
                   j <- (FS_TNs_list[[j]])/((num_of_Negatives_list[[j]])))

# the False Negative Rate = FN/(FN + TP)
FS_FNRs = lapply(seq_along(datasets), \(j)
                      j <- (FS_FNs_list[[j]])/(FS_FNs_list[[j]] + FS_TPs_list[[j]]))

## calculate the accuracy
FS_Accuracies_list <- lapply(seq_along(datasets), function(i)
  (FS_TPs_list[[i]] + FS_TNs_list[[i]])/(FS_TPs_list[[i]] + FS_TNs_list[[i]] + FS_FPs_list[[i]] + FS_FNs_list[[i]]))

# First calculate precision and TPR for each dataset
FS_PPVs_list <- lapply(seq_along(datasets), function(i)
  FS_TPs_list[[i]]/(FS_TPs_list[[i]] + FS_FPs_list[[i]]))

# Then calculate F1 score for each dataset
FS_F1_Scores_list <- lapply(seq_along(datasets), function(i)
  2 * (FS_PPVs_list[[i]] * FS_TPRs_list[[i]])/(FS_PPVs_list[[i]] + FS_TPRs_list[[i]]))


## Write one or more lines of code which determine whether each selected 
## model is "Underspecified", "Correctly Specified", or "Overspecified".
# True Positive Rates as a vector rather than a list
BM3_TPRs <- unlist(FS_TPRs_list)
BM3_mean_TPR <- round(mean(BM3_TPRs), 3)
# number of selected regressions with at least one omitted variable
num_OMVs <- sum(BM3_TPRs < 1, na.rm = TRUE)

# False Positive Rate as a vector rather than a list
BM3_FPRs <- unlist(FS_FPRs_list)
BM3_mean_FPR <- round(mean(BM3_FPRs), 3)
num_null_FPR <- sum(BM3_FPRs == 0, na.rm = TRUE)

# True Negative Rates as a vector rather than a list
BM3_TNRs <- unlist(FS_TNRs_list)
BM3_mean_TNR <- round(mean(BM3_TNRs), 3)

# False Negative Rates as a vector rather than a list
BM3_FNRs <- unlist(FS_FNRs_list)
BM3_mean_FNR <- round(mean(BM3_FNRs), 3)

# The PPVs/precision as a vector rather than a list
BM3_PPVs <- unlist(FS_PPVs_list)
BM3_mean_PPV <- round(mean(BM3_PPVs), 3)

# The Accuracy as a vector rather than a list
BM3_Accs <- unlist(FS_Accuracies_list)
BM3_mean_Accuracy <- round(mean(BM3_Accs), 3)

# The F1 Scores as a vector rather than a list
BM3_F1s <- unlist(FS_F1_Scores_list)
BM3_mean_F1_Score <- round(mean(BM3_F1s), 3)

# number of models with at least one extraneous variable selected
num_Extraneous <- sum(FS_FPRs > 0, na.rm = TRUE)
# Overspecified Regression Specifications Selected by FS
N_Over = sum( (FS_FPRs > 0) & (FS_TPRs == 1))
# Number of Underspecified Regression Specifications Selected by FS
N_Under = sum( (FS_TPRs < 1) & (FS_FPRs == 0) )
# Number of Correctly Specified Regressions Selected by FS
N_Correct <- sum( (FS_TPRs == 1) & (FS_FPRs == 0) & (FS_TNRs == 1) )

Headers <- c("Runtime", "Mean Accuracy", "Mean F1 Score", "Mean Positive Predictive Value", 
             "Mean True Positive Rate", "Mean True Negative Rate", 
             "Mean False Positive Rate", "Mean False Negative Rate")
BM3_PMs1 <- data.frame(FS_elapsed_time, BM3_mean_Accuracy, BM3_mean_F1_Score, BM3_mean_PPV, 
                       BM3_mean_TPR, BM3_mean_TNR, BM3_mean_FPR, BM3_mean_FNR)
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


setwd("C:/Users/Spencer//Documents/EER Project/Reproducing the results using the 'MASS' package")
getwd()
df <- data.frame(BE = BE_performance_metrics, FS = FS_performance_metrics)
write.csv(df, 
          file = "stepAIC's Performance on the DSs from 0-3-1-1 to 0-3-10-500.csv",
          row.names = FALSE)

## Create a single csv file that has the Regressors selected by both!
write.csv(data.frame(DS_name = DS_names_list, 
                     BE_Variables_Selected = sapply(IVs_Selected_by_BE, toString), 
                     FS_Variables_Selected = sapply(IVs_Selected_by_FS, toString),
                     Structural_Variables = sapply(Structural_Variables, 
                                                   toString),
                     Nonstructural_Variables = sapply(Nonstructural_Variables, 
                                                      toString)),
          file = "Regressors Selected by stepAIC for DSs from 0-3-1-1 to 0-3-10-500.csv",
          row.names = FALSE)

length(datasets)
head(DS_names_list)
tail(DS_names_list)
time_taken_to_fit_BE
time_to_fit_FS

#write.csv(x = data.frame(first_6_datasets = head(DS_names_list),
#                             last_6_datasets = tail(DS_names_list)), 
#           file = "Dataset range for the 3rd subset of 10k dataset.csv",
#           row.names = FALSE)

#save.image("D:/EER folder/WorkSpaces/BE & FS replication WorkSpace for the DSs from 0-3-1-1 to 0-3-10-500 datasets.RData")

