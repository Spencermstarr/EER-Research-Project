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


# This function fits all n LASSO regressions for/on
# each of the corresponding n datasets stored in the object
# of that name, then outputs standard regression results which 
# are typically called returned for any regression ran using R
CL <- makeCluster(detectCores() - 4L)
system.time( clusterExport(CL, c('datasets')) )
set.seed(11)     # to ensure replicability
system.time(LASSO.Lars.fits <- parLapply(cl = CL, X = datasets, function(i) 
  lars(x = as.matrix(select(i, starts_with("X"))), 
       y = i$Y, type = "lasso")))

#set.seed(11)     # to ensure replicability
#system.time(LASSO.Lars.fits <- lapply(datasets, function(i) 
#  lars(x = as.matrix(select(i, starts_with("X"))), 
#         y = i$Y, type = "lasso")))


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
                     Unselected_Variables = sapply(IVs.Not.Selected, 
                                                 toString),
                     Structural_Variables = sapply(Structural_Variables, 
                                                   toString)), 
          file = "Lars's Selections for the DSs from 0-3-1-1 to 0-4-10-500.csv", 
          row.names = FALSE)





### Count up how many Variables Selected match  the true 
### structural equation variables for that dataset in order
### to measure LASSO's performance.
Num_of_Positives <- lapply(Structural_Variables, function(i) { length(i) })

True_Positives <- lapply(seq_along(datasets), \(i)
                              sum(IVs.Selected.by.Lars[[i]] %in% 
                                    Structural_Variables[[i]])) 

Sensitivity = lapply(seq_along(datasets), \(j)
                  j <- (True_Positives[[j]]/Num_of_Positives[[j]]))

# the number of False Positives & True Negative for each Regression
Number_of_Negatives <- lapply(Structural_Variables, function(i) 
  {30 - length(i)})
False_Positives <- lapply(seq_along(datasets), \(i)
                  sum(!(IVs.Selected.by.Lars[[i]] %in% 
                          Structural_Variables[[i]]))) 

True_Negatives <- lapply(seq_along(datasets), \(K) 30 - True_Positives[[K]])

# the False Positive Rate = FP/(FP + TN)
FalsePosRate = lapply(seq_along(datasets), \(j)
                  j <- (False_Positives[[j]])/
                    (False_Positives[[j]] + True_Negatives[[j]]))

# the True Negative Rate = TN/(FP + TN)
Specificity <- lapply(seq_along(datasets), \(j) 
                   j <- (True_Negatives[[j]]/Number_of_Negatives[[j]]))


## Write one or more lines of code which determine whether each selected 
## model is "Underspecified", "Correctly Specified", or "Overspecified".
# True Positive Rates as a vector rather than a list
TPR <- unlist(Sensitivity)
mean_TPR <- round(mean(TPR), 3)
# number of selected regressions with at least one omitted variable
num_of_OM_IVs <- sum(TPR < 1, na.rm = TRUE)

# False Positive Rate as a vector rather than a list
FPR <- unlist(FalsePosRate)
mean_FPR <- round(mean(FPR), 3)
num_of_null_FPRs <- sum(FPR == 0, na.rm = TRUE)

# True Negative Rates as a vector rather than a list
TNR <- unlist(Specificity)
mean_TNR <- round(mean(TNR), 3)

# number of models with at least one extraneous variable selected
num_of_Extraneous_IVs <- sum(FPR > 0)
# Overspecified Regression Specifications Selected by LASSO
Over = sum( (FPR > 0) & (TPR == 1) )

# Number of Underspecified Regression Specifications Selected by LASSO
Under = sum( (TPR < 1) & (FPR == 0) )

# Number of Correctly Specified Regressions Selected by LASSO
Correct <- sum( (TPR == 1) & (FPR == 0) & (TNR == 1) )

Headers <- c("True Positive Rate", "True Negative Rate", 
             "False Positive Rate")
PMsA <- data.frame(mean_TPR, mean_TNR, mean_FPR)
colnames(PMsA) <- Headers

Headers <- c("Underspecified Models Selected", 
             "Correctly Specified Models Selected",
             "Overspecified Models Selected")
PMsB <- data.frame(Under, Correct, Over)
colnames(PMsB) <- Headers

Headers <- c("Models with at least one Omitted Variable",
             "Models with at least one Extra Variable")
PMsC <- data.frame(num_of_OM_IVs, num_of_Extraneous_IVs)
colnames(PMsC) <- Headers

# Or, just print out this instead of having to print out 3 different things
Lars_performance <- data.frame(PMsA, PMsB, PMsC)
Lars_performance

write.csv(Lars_performance, 
          file = "Lars's Performance on the datasets from 0-3-1-1 to 0-4-10-500.csv", 
          row.names = FALSE)

stopCluster(CL)
rm(CL)