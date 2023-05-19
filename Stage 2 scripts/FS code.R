

system.time( load("C:/Users/Spencer/OneDrive/Documents/Analytics Projects/EER Project/Saved WorkSpaces/Workspaces for dataset folders starting with '0.5'/datasets WorkSpace for '0.5-14-1-1 to 0.5-15-10-500'.RData") )


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



### Step 3: Run a Forward Selection Stepwise Regression
### function on each of the 260,000 datasets.
### Assign the null models to their corresponding datasets 
### and store these in the object "nulls".
set.seed(11)      # for reproducibility
system.time( FS.fits <- lapply(X = datasets, \(ds_i) {
  nulls <- lm(ds_i$Y ~ 1, data = ds_i)
  full_models <- lm(ds_i$Y ~ ., data = ds_i)
  forward <- stats::step(object = nulls, direction = 'forward',
                         scope = formula(full_models), trace = FALSE) }) )


# assign all regressors selected by Forward Stepwise Regression,
# not including the Intercepts
IVs_Selected_by_FS <- lapply(seq_along(FS.fits), 
                             \(i) names(coef(FS.fits[[i]])[-1]))

FS_IV_Candidates <- function(var_names, IVs_Selected_by_FS) {
  Candidate_Vars <- setdiff(var_names, IVs_Selected_by_FS)
  return(Candidate_Vars)}

IVs_Not_Selected_by_FS <- lapply(seq_along(FS.fits), \(j)
                                 j <- (FS_IV_Candidates(var_names, IVs_Selected_by_FS[[j]])))


## Create a single csv file that has the Regressors selected by both!
write.csv(data.frame(DS_name = DS_names_list, 
                     FS_Regressors_Selected = sapply(IVs_Selected_by_FS, toString),
                     Regressors_Not_Selected_by_FS = sapply(IVs_Not_Selected_by_FS, toString),
                     Structural_Variables = sapply(Structural_Variables, 
                                                   toString),
                     Nonstructural_Variables = sapply(Nonstructural_Variables, 
                                                      toString)),
          file = "Regressors Selected by FS for DSs from 0.5-14-1-1 to 0.5-15-10-500.csv", 
          row.names = FALSE)






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
          file = "FS's Performance on the DSs from 0.5-14-1-1 to 0.5-15-10-500.csv", 
          row.names = FALSE)
length(BE_performance_metrics)
length(FS_performance_metrics)




