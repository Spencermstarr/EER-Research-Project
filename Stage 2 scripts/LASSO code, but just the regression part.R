### This script can be run by the user by hitting Ctrl+Alt+R.
rm(list = ls())
load("C:/Users/Spencer/OneDrive/Documents/Analytics Projects/EER project/Saved WorkSpaces/Workspaces for dataset folders starting with '0.5'/datasets WorkSpace for '0.5-14-1-1 to 0.5-15-10-500'.RData")
load("C:/Users/Spencer/OneDrive/Documents/Analytics Projects/EER project/Saved WorkSpaces/Workspaces for dataset folders starting with '0.5'/loaded WorkSpace for datasets from '0.5-14-1-1 to 0.5-15-10-500'.RData")
setwd("C:/Users/Spencer/OneDrive/Documents/Analytics Projects/EER project/1st Benchmark/results for enet() with s = 1")
getwd()
# load all necessary packages
library(dplyr)
library(stats)
library(leaps)
library(lars)
library(elasticnet)
library(data.table)
library(parallel)


# This function fits all 260,000 LASSO regressions for/on
# each of the corresponding 260k datasets stored in the object
# of that name, then outputs standard regression results which 
# are typically called returned for any regression ran using R
set.seed(11)
time_to_fit <- system.time(enet_LASSO_fits <- lapply(X = datasets, function(i) 
               elasticnet::enet(x = as.matrix(dplyr::select(i, starts_with("X"))), 
               y = i$Y, lambda = 0, normalize = FALSE)))
time_to_fit

time_elapsed <- time_to_fit["elapsed"]

## This stores and prints out the estimates for all of the regression 
## equation specifications selected by LASSO when called.
set.seed(11)
system.time(enet_LASSO_Coeffs <- lapply(enet_LASSO_fits, 
                                   function(i) predict(i, 
                                                       x = as.matrix(dplyr::select(i, starts_with("X"))), 
                                                       s = 0.25, mode = "fraction", 
                                                       type = "coefficients")[["coefficients"]]))

### Write my own custom function which will separate out and return a 
### new list containing just names of the Independent Variables
### which are 'selected' or chosen for each individual dataset.
IVs_Selected <- lapply(enet_LASSO_Coeffs, function(i) names(i[i > 0]))
IVs_Not_Selected <- lapply(enet_LASSO_Coeffs, function(j) names(j[j == 0]))

#setwd("C:/Users/Spencer/OneDrive/Documents/Analytics Projects/EER/1st Benchmark/Variables Selected")
write.csv(data.frame(DS_name = DS_names_list,
                     Variables_Selected = sapply(IVs_Selected, toString),
                     Structural_Variables = sapply(Structural_Variables, toString),
                     Variables_Not_Selected = sapply(IVs_Not_Selected, toString), 
                     NonStructural_Variables = sapply(Nonstructural_Variables, toString)),
          file = "enet's selections for the DSs from 0.5-14-1-1 to 0.5-15-10-500.csv", 
          row.names = FALSE)





### Count up how many Variables Selected match the variables in the  
### structural equation which truthfully describes the population where the observations 
### in the sample dataset came from, or alternatively, the process it represents, 
### in order to measure LASSO's performance.
# all of the "Positives", i.e. all the Structural Regressors
enet_NPs <- lapply(Structural_Variables, function(i) { length(i) })
# all of the "Negatives", i.e. all the Nonstructural Regressors
enet_NNs <- lapply(Nonstructural_Variables, function(i) { length(i) })
# I am adding in alternative ways of cacluclating several of these just in case my first attempt is wrong
enet_NNs2 <- lapply(Structural_Variables, function(i) { 30 - length(i) })

# the number True Positives for each set of factors selected by LASSO
enet_TPs <- lapply(seq_along(datasets), \(i)
                   sum(IVs_Selected[[i]] %in% 
                         Structural_Variables[[i]]))
# the number True Negatives for each set of factors selected by LASSO
enet_TNs <- lapply(seq_along(datasets), \(k)
                   sum(IVs_Not_Selected[[k]] %in% 
                         Nonstructural_Variables[[k]]))

# the number of False Positives
enet_FPs <- lapply(seq_along(datasets), \(i)
                   sum(IVs_Selected[[i]] %in% 
                         Nonstructural_Variables[[i]]))
# the number of False Negatives 
enet_FNs <- lapply(seq_along(datasets), \(i)
                   sum(IVs_Not_Selected[[i]] %in% 
                         Structural_Variables[[i]]))


# the True Positive Rate aka the sensitivity
enet_TPRs = lapply(seq_along(datasets), \(j)
                   j <- (enet_TPs[[j]]/enet_NPs[[j]]))

# the False Positive Rate = FP/(FP + TN)
enet_FPRs = lapply(seq_along(datasets), \(j)
                   j <- (enet_FPs[[j]])/(enet_FPs[[j]] + enet_TNs[[j]]))

# the True Negative Rate aka the specificity
enet_TNRs <- lapply(seq_along(datasets), \(w)
                     w <- (enet_TNs[[w]]/(enet_FPs[[w]] + enet_TNs[[w]])))
enet_TNRs2 <- lapply(enet_FPRs, \(i) 
                    i <- (1 - i))
                           
# the False Negative Rate
enet_FNRs <- lapply(seq_along(datasets), \(i)
                   i <- ((enet_FNs[[i]])/(enet_TPs[[i]] + enet_FNs[[i]])))


## calculate the Accuracy and F1 Score with help from GPT 4
enet_Accuracy <- lapply(seq_along(datasets), function(i)
  (enet_TPs[[i]] + enet_TNs[[i]])/(enet_TPs[[i]] + enet_TNs[[i]] + enet_FPs[[i]] + enet_FNs[[i]]))

# First calculate the Precision for each dataset
enet_Precision <- lapply(seq_along(datasets), function(i)
  enet_TPs[[i]]/(enet_TPs[[i]] + enet_FPs[[i]]))

# Then calculate F1 Score for each dataset
enet_F1_Score <- lapply(seq_along(datasets), function(i)
  2 * (enet_Precision[[i]] * enet_TPRs[[i]])/(enet_Precision[[i]] + enet_TPRs[[i]]))


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

# Accuracy scores as a vector rather than a list
Acc <- unlist(enet_Accuracy)
mean_Accuracy <- round(mean(Acc), 3)

# F1 Scores as a vector rather than a list
F1_Scores <- unlist(enet_F1_Score)
mean_F1 <- round(mean(F1_Scores), 3)


## Now calculate the total number of underspecified, overspecified,
## and correctly specified regressions selected by enet.
# number of models with at least one extraneous variable selected
num_Extraneous <- sum(FPRs > 0, na.rm = TRUE)
# Overspecified Regression Specifications Selected by LASSO
N_Over = sum( (TPRs == 1) & (FPRs > 0) )

# Number of Underspecified Regression Specifications Selected by LASSO
N_Under = sum( (FPRs == 0) & (TPRs < 1) )

# Number of Correctly Specified Regressions Selected by LASSO
N_Correct <- sum( (TPRs == 1) & (TNRs == 1) )
Num_Under_Correct_or_Over = N_Under + N_Correct + N_Over


## Create a single table with all model performance metrics in it.
Headers <- c("Mean Accuracy", "Mean F1 Score", "Mean True Positive Rate", 
             "Mean True Negative Rate", "Mean False Positive Rate")
PMs1 <- data.frame(mean_Accuracy, mean_F1, mean_TPR, mean_TNR, mean_FPR)
colnames(PMs1) <- Headers
rm(Headers)

Headers <- c("Underspecified Models Selected", 
             "Correctly Specified Models Selected",
             "Overspecified Models Selected")
PMs2 <- data.frame(N_Under, N_Correct, N_Over)
colnames(PMs2) <- Headers

Headers <- c("Models with at least one Omitted Variable",
             "Models with at least one Extra Variable")
PMs3 <- data.frame(num_OMVs, num_Extraneous)
colnames(PMs3) <- Headers

# Or, just print out this instead of having to print out 3 different things
performance_metrics <- data.frame(PMs1, PMs2, PMs3)
performance_metrics

#setwd("C:/Users/Spencer/OneDrive/Documents/Analytics Projects/EER/1st Benchmark/Performance")
write.csv(performance_metrics, 
          file = "LASSO's Performance on the datasets from 0.5-14-1-1 to 0.5-15-10-500.csv", 
          row.names = FALSE)

length(datasets)
head(DS_names_list)
tail(DS_names_list)
getwd()
