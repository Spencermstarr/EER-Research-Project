### This script required the user to hit Run either by the button
### in the top right corner of this panel, or by hitting Ctrl+Enter

rm(list = ls())
# find out which working directory R has defaulted to
getwd()
load("C:/Users/Spencer/OneDrive/Documents/Analytics Projects/EER Project/Saved WorkSpaces/Workspaces for dataset folders starting with '0'/datasets WorkSpace for '0-3-1-1 to 0-4-10-500'.RData")
#Structural_Variables <- True_Regressors

# load all necessary packages using only 1 command/line
library_list <- c(library(dplyr),library(stringi),library(stats),
                  library(data.table),library(leaps),library(lars),
                  library(parallel))

# This function fits all n LASSO regressions for/on
# each of the corresponding n datasets stored in the object
# of that name, then outputs standard regression results which 
# are typically called returned for any regression ran using R
set.seed(11)     # to ensure replicability
time_taken_to_fit <- system.time(LASSO.Lars.fits <- lapply(X = datasets, function(i) 
  lars(x = as.matrix(select(i, starts_with("X"))), 
       y = i$Y, type = "lasso", normalize =  FALSE)))
# Extract the elapsed time
time_elapsed_fitting <- time_taken_to_fit["elapsed"]

# This stores and prints out all of the regression 
# equation specifications selected by LASSO when called
set.seed(11)     # to ensure replicability
system.time(LASSO.Lars.Coeffs <- lapply(LASSO.Lars.fits, 
                            function(i) predict(i, 
                                                x = as.matrix(dplyr::select(i, starts_with("X"))), 
                                                s = 0.2, mode = "fraction", 
                                                type = "coefficients")[["coefficients"]]))

IVs.Selected.by.Lars <- lapply(LASSO.Lars.Coeffs, function(i) names(i[i != 0]))
IVs.Not.Selected.by.Lars <- lapply(LASSO.Lars.Coeffs, function(j) names(j[j == 0]))

write.csv(data.frame(DS_name = DS_names_list, 
                     Variables_Selected = sapply(IVs.Selected.by.Lars, 
                                                 toString),
                     Structural_Variables = sapply(Structural_Variables, 
                                                   toString),
                     Unselected_Variables = sapply(IVs.Not.Selected.by.Lars, 
                                                   toString),
                     NonStructural_Variables = sapply(Nonstructural_Variables, 
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
                          sum(IVs.Selected.by.Lars[[i]] %in% 
                                Nonstructural_Variables[[i]])) 

True_Negatives <- lapply(seq_along(datasets), \(k)
                         sum(IVs.Not.Selected.by.Lars[[k]] %in% 
                               Nonstructural_Variables[[k]]))

# the number of False Negatives Selected by each LASSO
False_Negatives <- lapply(seq_along(datasets), \(i)
                          sum(IVs.Not.Selected.by.Lars[[i]] %in% 
                                Structural_Variables[[i]]))

# the False Positive Rate = FP/(FP + TN)
FalsePosRate = lapply(seq_along(datasets), \(j)
                      j <- (False_Positives[[j]])/
                        (False_Positives[[j]] + True_Negatives[[j]]))

# the True Negative Rate = TN/(FP + TN)
Specificity <- lapply(seq_along(datasets), \(j) 
                      j <- (True_Negatives[[j]]/Number_of_Negatives[[j]]))


## calculate the accuracy and F1 score with help from GPT 4
Accuracy <- lapply(seq_along(datasets), function(i)
  (True_Positives[[i]] + True_Negatives[[i]])/(True_Positives[[i]] + True_Negatives[[i]] + False_Positives[[i]] + False_Negatives[[i]]))

# First calculate precision and recall for each dataset
Precision <- lapply(seq_along(datasets), function(i)
  True_Positives[[i]]/(True_Positives[[i]] + False_Positives[[i]]))
Recall <- Sensitivity  # You have already calculated recall as True Positive Rate (TPR)

# Then calculate F1 score for each dataset
F1_Score <- lapply(seq_along(datasets), function(i)
  2 * (Precision[[i]] * Recall[[i]])/(Precision[[i]] + Recall[[i]]))


## Write one or more lines of code which determine whether each selected 
## model is "Underspecified", "Correctly Specified", or "Overspecified".
# True Positive Rates as a vector rather than a list
TPR <- unlist(Sensitivity)
mean_TPR <- round(mean(TPR), 3)
# number of selected regressions with at least one omitted variable
num_OMVs <- sum(TPR < 1, na.rm = TRUE)

# False Positive Rate as a vector rather than a list
FPR <- unlist(FalsePosRate)
mean_FPR <- round(mean(FPR), 3)
num_of_null_FPR <- sum(FPR == 0, na.rm = TRUE)

# True Negative Rates as a vector rather than a list
TNR <- unlist(Specificity)
mean_TNR <- round(mean(TNR), 3)


# Number of Underspecified Regression Specifications Selected by LASSO
Under = sum( (TPR < 1) & (FPR == 0) )

# Number of Correctly Specified Regressions Selected by LASSO
Correct <- sum( (TPR == 1) & (TNR == 1) )

# number of models with at least one extraneous variable selected
num_Extraneous <- sum(FPR > 0, na.rm = TRUE)
# Overspecified Regression Specifications Selected by LASSO
Over = sum( (TPR == 1) & (FPR > 0) )

# sum of all the 3 specification categories
Num_Under_Correct_or_Over = Under + Correct + Over


PMsA <- data.frame(mean(unlist(Accuracy), round = 3), 
                   mean(unlist(F1_Score), round = 3))
colnames(PMsA) <- c("Mean Accuracy", "Mean F1 Score")

Headers <- c("True Positive Rate", "True Negative Rate", 
             "False Positive Rate")
PMsB <- data.frame(mean_TPR, mean_TNR, mean_FPR)
colnames(PMsB) <- Headers

Headers <- c("Underspecified Models Selected", 
             "Correctly Specified Models Selected",
             "Overspecified Models Selected")
PMsC <- data.frame(Under, Correct, Over)
colnames(PMsC) <- Headers

Headers <- c("All Correct, Over, and Underspecified Models", 
             "Models with at least one Omitted Variable",
             "Models with at least one Extra Variable")
PMsD <- data.frame(Num_Under_Correct_or_Over, num_OMVs, num_Extraneous)
colnames(PMsD) <- Headers

# Or, just print out this instead of having to print out 3 different things
Lars_performance <- data.frame(PMsA, PMsB, PMsC, PMsD)
Lars_performance

write.csv(Lars_performance, 
          file = "Lars's Performance on the datasets from 0-3-1-1 to 0-4-10-500.csv", 
          row.names = FALSE)

length(datasets)
head(DS_names_list)
tail(DS_names_list)
getwd()
time_elapsed_fitting
