### This script can be run by the user by hitting Ctrl+Alt+R.

#rm(list = ls())
#load("C:/Users/Spencer/OneDrive/Documents/Analytics Projects/EER/Saved WorkSpaces/Workspaces for dataset folders starting with '0.75'/datasets WorkSpace for '0-5-1-1 to 0-6-10-500'.RData")
#load("C:/Users/Spencer/Documents/EER Project/WorkSpaces/datasets WorkSpace for '0-5-1-1 to 0-6-10-500'.RData")
#setwd("D:/Estimated Exhaustive Regression/LASSO Regressions")
#setwd("C:/Users/Spencer/OneDrive/Documents/Analytics Projects/EER/1st Benchmark")
getwd()

# load all necessary packages
library(dplyr)
library(stats)
library(leaps)
library(lars)
library(elasticnet)
library(data.table)
library(parallel)


#Structural_IVs <- Structural_Variables
#rm(Structural_Variables, fldpth, my_order)
#Structural_Variables <- Correctly_Selected_Variables
#Structural_Variables <- True_Regressors


# This function fits all 260,000 LASSO regressions for/on
# each of the corresponding 260k datasets stored in the object
# of that name, then outputs standard regression results which 
# are typically called returned for any regression ran using R
#CL <- makeCluster(detectCores() - 5L)
#system.time( clusterExport(CL, c('datasets')) )
#set.seed(11)     # to ensure replicability
#system.time(LASSO_fits <- parLapply(cl = CL, X = datasets, function(i) 
#               elasticnet::enet(x = as.matrix(dplyr::select(i, 
#                                         starts_with("X"))), 
#               y = i$Y, lambda = 0, normalize = FALSE)))
set.seed(11)
system.time(LASSO_fits <- lapply(X = datasets, function(i) 
               elasticnet::enet(x = as.matrix(dplyr::select(i, 
                                         starts_with("X"))), 
               y = i$Y, lambda = 0, normalize = FALSE)))


## This stores and prints out the estimates for all of the regression 
## equation specifications selected by LASSO when called.
LASSO_Coeffs <- lapply(LASSO_fits, 
                       function(i) predict(i, 
                                           x = as.matrix(dplyr::select(i, starts_with("X"))), 
                                           s = 0.1, mode = "fraction", 
                                           type = "coefficients")[["coefficients"]])
#stopCluster(CL)
#rm(CL)

### Write my own custom function which will separate out and return a 
### new list containing just names of the Independent Variables
### which are 'selected' or chosen for each individual dataset.
IVs_Selected <- lapply(LASSO_Coeffs, function(i) names(i[i > 0]))
IVs_Not_Selected <- lapply(LASSO_Coeffs, function(j) names(j[j == 0]))

#setwd("C:/Users/Spencer/OneDrive/Documents/Analytics Projects/EER/1st Benchmark/Variables Selected")
write.csv(data.frame(DS_name = DS_names_list, 
                     Variables_Selected = sapply(IVs_Selected, 
                                                 toString),
                     Structural_Variables = sapply(Structural_Variables, 
                                                   toString)), 
          file = "LASSO's Selections for the DSs from 0-5-1-1 to 0-6-10-500.csv", 
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
                   sum(IVs_Not_Selected[[k]] %in% 
                         Nonstructural_Variables[[k]]))

# the number of False Positives
enet_FPs <- lapply(seq_along(datasets), \(i)
                   sum(IVs_Selected[[i]] %in% 
                         Nonstructural_Variables[[i]]))
# the number of False Negatives Selected by each LASSO
enet_FNs <- lapply(seq_along(datasets), \(i)
                   sum(IVs_Not_Selected[[i]] %in% 
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

# number of models with at least one extraneous variable selected
num_Extraneous <- sum(FPRs > 0)
# Overspecified Regression Specifications Selected by LASSO
N_Over = sum( (FPRs > 0) & (TPRs == 1) )

# Number of Underspecified Regression Specifications Selected by LASSO
N_Under = sum( (TPRs > 0) & (FPRs == 0) & (TPRs < 1) )

# Number of Correctly Specified Regressions Selected by LASSO
N_Correct <- sum( (TPRs == 1) & (FPRs == 0) & (TNRs == 1) )
Num_Under_Correct_or_Over = N_Under + N_Correct + N_Over

## Create a single table with all model performance metrics in it.
Headers <- c("True Positive Rate", "True Negative Rate", 
             "False Positive Rate")
PMs1 <- data.frame(mean_TPR, mean_TNR, mean_FPR)
colnames(PMs1) <- Headers

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
          file = "LASSO's Performance on the datasets from 0-5-1-1 to 0-6-10-500.csv", 
          row.names = FALSE)

length(datasets)
head(DS_names_list)
tail(DS_names_list)
