### This script required the user to hit Run either by the button
### in the top right corner of this panel, or by hitting Ctrl+Enter

#rm(list = ls())
# find out which working directory R has defaulted to
getwd()

#Structural_Variables <- True_Regressors

# load all necessary packages using only 1 command/line
library_list <- c(library(dplyr),library(stringi),
                  library(purrr),library(stats),library(data.table),
                  library(leaps),library(lars),library(parallel))


# This function fits all 40 LASSO regressions for/on
# each of the corresponding 40 datasets stored in the object
# of that name, then outputs standard regression results which 
# are typically called returned for any regression ran using R
set.seed(100)     # to ensure replicability
system.time(LASSO.Lars.fits <- lapply(datasets, function(i) 
  lars(x = as.matrix(select(i, starts_with("X"))), 
         y = i$Y, type = "lasso")))

# This stores and prints out all of the regression 
# equation specifications selected by LASSO when called
LASSO.Lars.Coeffs <- lapply(LASSO.Lars.fits, 
                       function(i) predict(i, 
                                           x = as.matrix(dplyr::select(i, starts_with("X"))), 
                                           s = 0.1, mode = "fraction", 
                                           type = "coefficients")[["coefficients"]])

IVs.Selected.by.Lars <- lapply(LASSO.Lars.Coeffs, function(i) names(i[i > 0]))
IVs.Not.Selected <- lapply(LASSO.Lars.Coeffs, function(j) names(j[j == 0]))

write.csv(data.frame(DS_name = DS_names_list, 
                     Variables_Selected = sapply(IVs.Selected.by.Lars, 
                                                 toString),
                     Structural_Variables = sapply(Structural_Variables, 
                                                   toString)), 
          file = "Lars's Selections for the DSs from 0.5-10-1-1 to 0.5-11-10-500.csv", 
          row.names = FALSE)





### Count up how many Variables Selected match  the true 
### structural equation variables for that dataset in order
### to measure LASSO's performance.
BM1_NPs <- lapply(Structural_Variables, function(i) { length(i) })

system.time(BM1_TPs <- lapply(seq_along(datasets), \(i)
                              sum(IVs.Selected.by.Lars[[i]] %in% 
                                    Structural_Variables[[i]]))) 

BM1_TPRs = lapply(seq_along(datasets), \(j)
                  j <- (BM1_TPs[[j]]/BM1_NPs[[j]]))

# the number of False Positives & True Negative for each Regression
BM1_NNs <- lapply(Structural_Variables, function(i) {30 - length(i)})
BM1_FPs <- lapply(seq_along(datasets), \(i)
                  sum(!(IVs.Selected.by.Lars[[i]] %in% 
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
          file = "Lars's Performance on the datasets from 0.5-10-1-1 to 0.5-11-10-500.csv", 
          row.names = FALSE)
