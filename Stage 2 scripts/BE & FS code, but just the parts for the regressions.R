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
#system.time( load("C:/Users/Spencer/OneDrive/Documents/Analytics Projects/EER Project/Saved WorkSpaces/datasets WorkSpace for '0.75-15-1-1 to 0.75-15-10-500'.RData") )
system.time( load("C:/Users/Spencer/OneDrive/Documents/Analytics Projects/EER Project/Saved WorkSpaces/Workspaces for dataset folders starting with '0.75'/datasets WorkSpace for '0.75-15-1-1 to 0.75-15-10-500'.RData") )
#system.time( load("C:/Users/Spencer/OneDrive/Documents/Analytics Projects/EER Project/Saved WorkSpaces/Workspaces for dataset folders starting with '0.75'/loaded WorkSpace for datasets from '0.75-15-1-1 to 0.75-15-10-500'.RData") )
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
CL <- makeCluster(detectCores() - 3L)
system.time( clusterExport(CL, c('datasets')) )
set.seed(11)      # for reproducibility
time_taken_to_fit_BE <- system.time( BE.fits <- parLapply(cl = CL, X = datasets, \(ds_i) {
  full_models <- lm(ds_i$Y ~ ., data = ds_i)
  back <- stats::step(object = full_models, scope = formula(full_models), 
                      direction = 'back', trace = FALSE) }) )
time_taken_to_fit_BE


stopCluster(CL)
rm(CL)

#set.seed(11)      # for reproducibility
#time_taken_to_fit_BE <- system.time( BE.fits <- lapply(X = datasets, \(ds_i) {
#  full_models <- lm(ds_i$Y ~ ., data = ds_i)
#  back <- stats::step(object = full_models, scope = formula(full_models), 
#                      direction = 'back', trace = FALSE) }) )
#time_taken_to_fit_BE

# Extract the elapsed time
BE_elapsed_time <- time_taken_to_fit_BE["elapsed"]


# Extract the names of all IVs selected (besides their intercepts) by each 
# of the 260k Backward Elimination Stepwise Regressions estimated above 
# and store those estimates in the list object 'IVs_Selected_by_BE'
set.seed(11)      # for reproducibility
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

# the number of False Positives & True Negatives for each Regression
num_of_Negatives <- lapply(Structural_Variables, function(i) {30 - length(i)})
BE_FPs_list <- lapply(seq_along(datasets), \(i)
                 sum(!(IVs_Selected_by_BE[[i]] %in% 
                         Structural_Variables[[i]]))) 

BE_TNs_list <- lapply(seq_along(datasets), \(k)
                      sum(IVs_Not_Selected_by_BE[[k]] %in% 
                            Nonstructural_Variables[[k]]))

# the number of False Negatives Selected by each BE
BE_FNs_list <- lapply(seq_along(datasets), \(i)
                      sum(IVs_Not_Selected_by_BE[[i]] %in% 
                            Structural_Variables[[i]]))



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

# False Negative Rates as a vector rather than a list
BE_FNRs <- unlist(BE_FNRs_list)
BE_mean_FNR <- round(mean(BE_FNRs), 3)


# The PPVs/precision as a vector rather than a list
BE_PPVs <- unlist(BE_Precision_list)
BE_mean_PPV <- round(mean(BE_PPVs), 3)

# The Accuracy as a vector rather than a list
BE_Accs <- unlist(BE_Accuracy_list)
BE_mean_Accuracy <- round(mean(BE_Accs), 3)

# The F1 Scores as a vector rather than a list
BE_F1s <- unlist(BE_F1_Score_list)
BE_mean_F1_Score <- round(mean(BE_F1s), 3)


# Number of Underspecified Regression Specifications Selected by BE
BE_Under = sum( (BE_TPRs < 1) & (BE_FPRs == 0) )
# Number of Correctly Specified Regressions Selected by BE
BE_Correct <- sum( (BE_TPRs == 1) & (BE_TNRs == 1) )
# Overspecified Regression Specifications Selected by FS
BE_Over = sum( (BE_TPRs == 1) & (BE_FPRs > 0) )

# sum of all the 3 specification categories
BE_Num_Un_Corr_Ov = BE_Under + BE_Correct + BE_Over


# create dataframes to store the performance metrics in
Headers <- c("Runtime", "Mean Accuracy", "Mean F1 Score", "Mean Positive Predictive Value", 
             "Mean True Positive Rate", "Mean True Negative Rate", "Mean False Positive Rate", 
             "Mean False Negative Rate")
BE_PMs1 <- data.frame(BE_elapsed_time, BE_mean_Accuracy, BE_mean_F1_Score, BE_mean_PPV, 
                      BE_mean_TPR, BE_mean_TNR, BE_mean_FPR, BE_mean_FNR)
colnames(BE_PMs1) <- Headers

Headers <- c("Underspecified Models Selected", 
             "Correctly Specified Models Selected",
             "Overspecified Models Selected")
BE_PMs2 <- data.frame(BE_Under, BE_Correct, BE_Over)
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
          file = "BE's Performance on the DSs from 0.75-15-1-1 to 0.75-15-10-500.csv",
          row.names = FALSE)











### Step 3: Run a Forward Selection Stepwise Regression
### function on each of the 260,000 datasets.
### Assign the null models to their corresponding datasets 
### and store these in the object "nulls".
CL <- makeCluster(detectCores() - 3L)
system.time( clusterExport(CL, c('datasets')) )
set.seed(11)      # for reproducibility
time_taken_to_fit_FS <- system.time( FS.fits <- parLapply(cl = CL, X = datasets, \(ds_i) {
  nulls <- lm(ds_i$Y ~ 1, data = ds_i)
  full_models <- lm(ds_i$Y ~ ., data = ds_i)
  forward <- stats::step(object = nulls, scope = formula(full_models), 
                         direction = 'forward', trace = FALSE) }) )
time_taken_to_fit_FS

stopCluster(CL)
rm(CL)

#set.seed(11)      # for reproducibility
#time_taken_to_fit_FS <- system.time( FS.fits <- lapply(X = datasets, \(ds_i) {
#  nulls <- lm(ds_i$Y ~ 1, data = ds_i)
#  full_models <- lm(ds_i$Y ~ ., data = ds_i)
#  forward <- stats::step(object = nulls, scope = formula(full_models), 
#                         direction = 'forward', trace = FALSE) }) )
#time_taken_to_fit_FS

# Extract the elapsed time
 FS_elapsed_time <- time_taken_to_fit_FS["elapsed"]


# assign all regressors selected by Forward Stepwise Regression,
# not including the Intercepts, to an object
set.seed(11)      # for reproducibility
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

# the number of False Negatives Selected by each LASSO
FS_FNs_list <- lapply(seq_along(datasets), \(i)
                  sum(IVs_Not_Selected_by_FS[[i]] %in% 
                        Structural_Variables[[i]]))

# now calculate the # of True Negatives for each
FS_TNs_list <- lapply(seq_along(datasets), \(k)
                         sum(IVs_Not_Selected_by_FS[[k]] %in% 
                               Nonstructural_Variables[[k]]))

# the False Positive Rate = FP/(FP + TN)
FS_FPRs_list = lapply(seq_along(datasets), \(j)
                 j <- (FS_FPs_list[[j]])/(FS_FPs_list[[j]] + FS_TNs_list[[j]]))

# the True Negative Rate
FS_TNRs_list <- lapply(seq_along(datasets), \(j) 
                       j <- (FS_TNs_list[[j]])/((num_of_Negatives[[j]])))


# the False Negative Rate = FN/(FN + TP)
FS_FNRs_list = lapply(seq_along(datasets), \(j)
                      j <- (FS_FNs_list[[j]])/(FS_FNs_list[[j]] + FS_TPs_list[[j]]))

## calculate the accuracy and F1 score with help from GPT 4
FS_Accuracy_list <- lapply(seq_along(datasets), function(i)
  (FS_TPs_list[[i]] + FS_TNs_list[[i]])/(FS_TPs_list[[i]] + FS_TNs_list[[i]] + FS_FPs_list[[i]] + FS_FNs_list[[i]]))

# First calculate precision and TPR for each dataset
FS_Precision_list <- lapply(seq_along(datasets), function(i)
  FS_TPs_list[[i]]/(FS_TPs_list[[i]] + FS_FPs_list[[i]]))

# Then calculate F1 score for each dataset
FS_F1_Score_list <- lapply(seq_along(datasets), function(i)
  2 * (FS_Precision_list[[i]] * FS_TPRs_list[[i]])/(FS_Precision_list[[i]] + FS_TPRs_list[[i]]))

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
FS_num_Extraneous <- sum(FS_FPRs > 0, na.rm = TRUE)

# True Negative Rates as a vector rather than a list
FS_TNRs <- unlist(FS_TNRs_list)
FS_mean_TNR <- round(mean(FS_TNRs), 3)

# False Negative Rates as a vector rather than a list
FS_FNRs <- unlist(FS_FNRs_list)
FS_mean_FNR <- round(mean(FS_FNRs), 3)

# The PPVs/precision as a vector rather than a list
FS_PPVs <- unlist(FS_Precision_list)
FS_mean_PPV <- round(mean(FS_PPVs), 3)

# The Accuracy as a vector rather than a list
FS_Accs <- unlist(FS_Accuracy_list)
FS_mean_Accuracy <- round(mean(FS_Accs), 3)

# The F1 Scores as a vector rather than a list
FS_F1s <- unlist(FS_F1_Score_list)
FS_mean_F1_Score <- round(mean(FS_F1s), 3)

# Number of Underspecified Regression Specifications Selected by FS
FS_Under = sum( (FS_TPRs < 1) & (FS_FPRs == 0) )
# Number of Correctly Specified Regressions Selected by FS
FS_Correct <- sum( (FS_TPRs == 1) & (FS_TNRs == 1) )
# Overspecified Regression Specifications Selected by FS
FS_Over = sum( (FS_TPRs == 1) & (FS_FPRs > 0) )

# sum of all the 3 specification categories
FS_Num_Un_Corr_Ov = FS_Under + FS_Correct + FS_Over


Headers <- c("Runtime", "Mean Accuracy", "Mean F1 Score", "Mean Positive Predictive Value", "Mean True Positive Rate", 
             "Mean True Negative Rate", "Mean False Positive Rate", "Mean False Negative Rate")
FS_PMs1 <- data.frame(FS_elapsed_time, FS_mean_Accuracy, FS_mean_F1_Score, 
                      FS_mean_PPV, FS_mean_TPR, FS_mean_TNR, FS_mean_FPR, FS_mean_FNR)
colnames(FS_PMs1) <- Headers

Headers <- c("Underspecified Models Selected", 
             "Correctly Specified Models Selected",
             "Overspecified Models Selected")
FS_PMs2 <- data.frame(FS_Under, FS_Correct, FS_Over)
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
          file = "FS's Performance on the DSs from 0.75-15-1-1 to 0.75-15-10-500.csv", 
          row.names = FALSE)
length(BE_performance_metrics)
length(FS_performance_metrics)

df <- data.frame(BE = BE_performance_metrics, "\n", FS = FS_performance_metrics)
# Combine BE and FS data frames with an empty row in between
#SR_df <- data.frame(BE = rbind(BE_PMs1, BE_PMs2, BE_PMs3, ""), 
#                 FS = rbind("", FS_PMs1, FS_PMs2, FS_PMs3))
# Combine BE and FS data frames with an empty row in between
#empty_row <- rep("", ncol(BE_PMs1))  # Create an empty row with the same number of columns as BE_PMs1

#SR_df <- data.frame(BE = rbind(BE_PMs1, BE_PMs2, BE_PMs3, empty_row), 
#                 FS = rbind(empty_row, FS_PMs1, FS_PMs2, FS_PMs3))
library(readr)
write_csv(df, file = "SR's Performance on the DSs from 0.75-15-1-1 to 0.75-15-10-500.csv")


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
          file = "Regressors Selected by SR for DSs from 0.75-15-1-1 to 0.75-15-10-500.csv")

length(datasets)
head(DS_names_list)
tail(DS_names_list)
#write.csv(x = data.frame(first_6_datasets = head(DS_names_list),
#                             last_6_datasets = tail(DS_names_list)), 
#           file = "Dataset range for 0.datasets WorkSpace for '0.75-15-1-1 to 0.75-15-10-500'.csv",
#           row.names = FALSE)

#save.image("D:/EER/Saved WorkSpaces/BE & FS WorkSpace for the 10k '0.75-15-1-1 to 0.75-15-10-500' datasets.RData")
#save.image("D:/EER folder/WorkSpaces/BE & FS WorkSpace for the 10k '0.75-15-1-1 to 0.75-15-10-500' datasets.RData")
time_taken_to_fit_BE
BE_elapsed_time
time_taken_to_fit_FS
FS_elapsed_time
