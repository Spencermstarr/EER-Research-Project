### Spencer Marlen-Starr's RStudio script of functions, methods, code, and 
### code comments required for running Benchmarks 2 & 3, namely the 
### Backward Elimination & Forward Selection versions of Stepwise Regression
### on some of our random synthetic observations to see how well it 
### does so we can compare its results with Dr. Davies' EER procedure.

## This script in its entirety can be run by the user by hitting Ctrl+Alt+R.
#rm(list = ls())
#setwd("D:/EER")
#setwd("D:/EER folder")
getwd()

#system.time( load("C:/Users/Spencer/OneDrive/Documents/Analytics Projects/Estimated Exhaustive Regression Project/Saved WorkSpaces/datasets WorkSpace for '0.75-11-1-1 to 0.75-11-10-500'.RData") )


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
#CL <- makeCluster(detectCores() - 4L)
#system.time( clusterExport(CL, c('datasets')) )
set.seed(11)      # for reproducibility
#system.time( BE.fits <- parLapply(cl = CL, X = datasets, \(ds_i) {
#  full_models <- lm(ds_i$Y ~ ., data = ds_i)
#  back <- stats::step(full_models, scope = formula(full_models), 
#                      direction = 'back', trace = FALSE) }) )
#system.time( BE.fits <- lapply(X = datasets, \(ds_i) {
#  full_models <- lm(ds_i$Y ~ ., data = ds_i)
#  back <- stats::step(full_models, scope = formula(full_models), 
#                      direction = 'back', trace = FALSE) }) )

BE_Coeffs <- lapply(seq_along(BE.fits), function(i) coef(BE.fits[[i]]))
#stopCluster(CL)
#rm(CL)

# extract the names of all IVs selected by them without their intercepts
IVs_Selected_by_BE <- lapply(seq_along(BE.fits), 
                             \(i) names(coef(BE.fits[[i]])[-1]))



### Count up how many Variables Selected match  the true 
### structural equation variables for that dataset in order
### to measure Backward Stepwise's performance.
# the True Positive Rate
num_of_Positives <- lapply(Structural_Variables, function(i) { length(i) })

BE_TPs <- lapply(seq_along(datasets), \(i)
                             sum(IVs_Selected_by_BE[[i]] %in% 
                                   Structural_Variables[[i]]))

BE_TPRs = lapply(seq_along(datasets), \(j)
                 j <- (BE_TPs[[j]]/num_of_Positives[[j]]) )

# the number of False Positives & True Negative for each Regression
num_of_Negatives <- lapply(Structural_Variables, function(i) {30 - length(i)})
BE_FPs <- lapply(seq_along(datasets), \(i)
                 sum(!(IVs_Selected_by_BE[[i]] %in% 
                         Structural_Variables[[i]]))) 
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
N_Over = sum( (BM2_FPRs > 0) & (BM2_TPRs == 1) )
# Number of Underspecified Regression Specifications Selected by BE
N_Under = sum( (BM2_TPRs < 1) & (BM2_FPRs == 0) )
# Number of Correctly Specified Regressions Selected by BE
N_Correct <- sum( (BM2_TPRs == 1) & (BM2_FPRs == 0) & (BM2_TNRs == 1) )

# create dataframes to store the performance metrics in
Headers <- c("True Positive Rate", "True Negative Rate", 
             "False Positive Rate")
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

BE_df <- data.frame(BE = BE_performance_metrics)
write.csv(BE_df, 
          file = "BE's Performance on the DSs from 0.75-5-1-1 to 0.75-6-10-500.csv",
          row.names = FALSE)











### Step 3: Run a Forward Selection Stepwise Regression
### function on each of the 260,000 datasets.
### Assign the null models to their corresponding datasets 
### and store these in the object "nulls".
#CL <- makeCluster(detectCores() - 5L)
#system.time( clusterExport(CL, c('datasets')) )
#set.seed(11)      # for reproducibility
#system.time( FS.fits <- parLapply(cl = CL, X = datasets, \(ds_i) {
#  nulls <- lm(ds_i$Y ~ 1, data = ds_i)
#  full_models <- lm(ds_i$Y ~ ., data = ds_i)
#  forward <- stats::step(object = nulls, direction = 'forward',
#                         scope = formula(full_models), trace = FALSE) }) )
set.seed(11)      # for reproducibility
system.time( FS.fits <- lapply(X = datasets, \(ds_i) {
  nulls <- lm(ds_i$Y ~ 1, data = ds_i)
  full_models <- lm(ds_i$Y ~ ., data = ds_i)
  forward <- stats::step(object = nulls, direction = 'forward',
                         scope = formula(full_models), trace = FALSE) }) )

FS_Coeffs <- lapply(seq_along(FS.fits), function(i) coef(FS.fits[[i]]))
#stopCluster(CL)
#rm(CL)

# assign all regressors selected by Forward Stepwise Regression,
# not including the Intercepts
IVs_Selected_by_FS <- lapply(seq_along(FS.fits), 
                             \(i) names(coef(FS.fits[[i]])[-1]))



### Count up how many Variables Selected match  the true 
### structural equation variables for that dataset in order
### to measure Forward Stepwise's performance.
# the True Positive Rate
FS_TPs <- lapply(seq_along(datasets), \(i)
                             sum(IVs_Selected_by_FS[[i]] %in% 
                                   Structural_Variables[[i]]))

FS_TPRs = lapply(seq_along(datasets), \(j)
                 j <- (FS_TPs[[j]]/num_of_Positives[[j]]) )

# the number of total Negatives & False Positives for each Regression
FS_FPs <- lapply(seq_along(datasets), \(i)
                 sum(!(IVs_Selected_by_FS[[i]] %in% 
                         Structural_Variables[[i]]))) 
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
N_Over = sum( (FS_FPRs > 0) & (FS_TPRs == 1) )
# Number of Underspecified Regression Specifications Selected by FS
N_Under = sum( (FS_TPRs < 1) & (FS_FPRs == 0) )
# Number of Correctly Specified Regressions Selected by FS
N_Correct <- sum( (FS_TPRs == 1) & (FS_FPRs == 0) & (FS_TNRs == 1) )

Headers <- c("True Positive Rate", "True Negative Rate", 
             "False Positive Rate")
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

FS_df <- data.frame(FS = FS_performance_metrics)
write.csv(FS_df, 
          file = "FS's Performance on the DSs from 0.75-8-1-1 to 0.75-8-10-500.csv", 
          row.names = FALSE)

length(BE_performance_metrics)
length(FS_performance_metrics)


df <- data.frame(BE = BE_performance_metrics, FS = FS_performance_metrics)
write.csv(df, 
          file = "SR's Performance on the DSs from 0.75-11-1-1 to 0.75-11-10-500.csv", 
          row.names = FALSE)


## Create a single csv file that has the Regressors selected by both!
write.csv(data.frame(DS_name = DS_names_list, 
                     BE_Regressors_Selected = sapply(IVs_Selected_by_BE, toString), 
                     FS_Regressors_Selected = sapply(IVs_Selected_by_FS, toString),
                     Structural_Variables = sapply(Structural_Variables, 
                                                   toString)),
          file = "Regressors Selected by SR for DSs from 0.75-11-1-1 to 0.75-11-10-500.csv", 
          row.names = FALSE)


length(datasets)
head(DS_names_list)
tail(DS_names_list)
#write.csv(x = data.frame(first_6_datasets = head(DS_names_list),
#                             last_6_datasets = tail(DS_names_list)), 
#           file = "Dataset range for 0.datasets WorkSpace for '0.75-11-1-1 to 0.75-11-10-500'.csv",
#           row.names = FALSE)

#save.image("D:/EER/Saved WorkSpaces/BE & FS WorkSpace for the 10k '0.75-11-1-1 to 0.75-11-10-500' datasets.RData")
#save.image("D:/EER folder/WorkSpaces/BE & FS WorkSpace for the 10k '0.75-11-1-1 to 0.75-11-10-500' datasets.RData")


