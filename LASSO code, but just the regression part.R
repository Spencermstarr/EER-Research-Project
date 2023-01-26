### This script can be run by the user by hitting Ctrl+Alt+R.

#rm(list = ls())
#system.time( load("C:/Users/Spencer/OneDrive/Documents/Analytics Projects/EER/Saved WorkSpaces/datasets WorkSpace for '0.5-5-1-1 to 0.5-6-10-500'.RData") )
#setwd("D:/Estimated Exhaustive Regression/LASSO Regressions")
setwd("C:/Users/Spencer/OneDrive/Documents/Analytics Projects/EER/1st Benchmark")
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
# Press Run or hit Ctrl+Enter
IVs_Selected_by_LASSO <- lapply(LASSO_Coeffs, function(i) names(i[i > 0]))

write.csv(data.frame(DS_name = DS_names_list, 
                     Variables_Selected = sapply(IVs_Selected_by_LASSO, 
                                                 toString),
                     Structural_Variables = sapply(Structural_Variables, 
                                                   toString)), 
          file = "LASSO's Selections for the DSs from 0.5-5-1-1 to 0.5-6-10-500.csv", 
          row.names = FALSE)





### Count up how many Variables Selected match  the true 
### structural equation variables for that dataset in order
### to measure LASSO's performance.
BM1_NPs <- lapply(Structural_Variables, function(i) { length(i) })

system.time(BM1_TPs <- lapply(seq_along(datasets), \(i)
                              sum(IVs_Selected_by_LASSO[[i]] %in% 
                                    Structural_Variables[[i]]))) 

BM1_TPRs = lapply(seq_along(datasets), \(j)
                  j <- (BM1_TPs[[j]]/BM1_NPs[[j]]))

# the number of False Positives & True Negative for each Regression
BM1_NNs <- lapply(Structural_Variables, function(i) {30 - length(i)})
BM1_FPs <- lapply(seq_along(datasets), \(i)
                  sum(!(IVs_Selected_by_LASSO[[i]] %in% 
                          Structural_Variables[[i]]))) 
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

# True Negative Rates as a vector rather than a list
TNRs <- unlist(BM1_TNRs)
mean_TNR <- round(mean(TNRs), 3)

# number of models with at least one extraneous variable selected
num_Extraneous <- sum(FPRs > 0)
# Overspecified Regression Specifications Selected by LASSO
N_Over = sum( (FPRs > 0) & (TPRs == 1) )

# Number of Underspecified Regression Specifications Selected by LASSO
N_Under = sum( (TPRs < 1) & (FPRs == 0) )

# Number of Correctly Specified Regressions Selected by LASSO
N_Correct <- sum( (TPRs == 1) & (FPRs == 0) & (TNRs == 1) )

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

write.csv(performance_metrics, 
          file = "LASSO's Performance on the datasets from 0.5-5-1-1 to 0.5-6-10-500.csv", 
          row.names = FALSE)

length(datasets)
head(DS_names_list)
tail(DS_names_list)
#write.csv(x = data.frame(first_6_datasets = head(DS_names_list),
#                         last_6_datasets = tail(DS_names_list)), 
#          file = "Dataset range for 7th run (10k).csv",
#          row.names = FALSE)

