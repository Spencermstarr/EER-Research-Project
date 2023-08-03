

rm(list = ls())
getwd()
load("~/Analytics Projects/EER project/Saved WorkSpaces/Workspaces for dataset folders starting with '0'/loaded WorkSpace for datasets from '0-3-1-1 to 0-3-10-500'.RData")
selections_set <- readr::read_csv("LASSO's Selections via glmnet for the DSs from '0-3-1-1 to 0-4-10-500'.csv", col_names = TRUE)

# Number of rows per subset
rows_per_subset <- 500

# Number of subsets
n_subsets <- nrow(selections_set) %/% rows_per_subset

# Create a list to store the subsets
subsets <- list()


# Loop through and create the subsets
for(i in 1:n_subsets) {
  start_row <- (i - 1) * rows_per_subset + 1
  end_row <- i * rows_per_subset
  subsets[[i]] <- data_subset[start_row:end_row,]
}

# If there are remaining rows that don't fit into a full 500-row subset
remaining_rows <- nrow(selections_set) %% rows_per_subset
if (remaining_rows > 0) {
  subsets[[n_subsets + 1]] <- selections_set[(n_subsets * rows_per_subset + 1):nrow(data_subset), ]
}


setwd("C:/Users/spenc/OneDrive/Documents/Analytics Projects/EER project/1st Benchmark/Replications using 'glmnet'/Variables selected by glmnet/cv.glmnet with lambda.1se")
# Inside the loop
for(i in 1:n_subsets) {
  start_row <- (i - 1) * rows_per_subset + 1
  end_row <- i * rows_per_subset
  subset <- data_subset[start_row:end_row,]
  subsets[[i]] <- subset
  write.csv(subset, paste0("subset_", i, ".csv"), row.names = FALSE)
}




###
##### OR
###


data_subset <- readr::read_csv("0-3-1-1 to 0-3-1-500.csv", col_names = TRUE)
#attach(data_subset)
Structural_Variables <- data_subset$Structural_Variables
class(Structural_Variables)
Variables.Selected.by.glmnet <- data_subset$Variables.Selected.by.glmnet
Nonstructural_Variables <- data_subset$Nonstructural_Variables
Variables.Not.Selected.by.glmnet <- data_subset$Variables.Not.Selected.by.glmnet



### Count up how many Variables Selected match  the true 
### structural equation variables for that dataset in order
### to measure LASSO's performance.
# all of the "Positives", i.e. all the Structural Regressors
BM1_NPs <- lapply(Structural_Variables, function(i) { length(i) })

# all of the "Negatives", i.e. all the Nonstructural Regressors
BM1_NNs <- lapply(Structural_Variables, function(i) {30 - length(i)})

# all the "True Positives"
BM1_TPs <- lapply(seq_along(Structural_Variables), \(i)
                  sum(Variables.Selected.by.glmnet[[i]] %in% 
                        Structural_Variables[[i]]))
# the number True Negatives for each LASSO
BM1_TNs <- lapply(seq_along(Structural_Variables), \(k)
                  sum(Variables.Not.Selected.by.glmnet[[k]] %in% 
                        Nonstructural_Variables[[k]]))

# the number of False Positives
BM1_FPs <- lapply(seq_along(Structural_Variables), \(i)
                  sum(Variables.Selected.by.glmnet[[i]] %in% 
                        Nonstructural_Variables[[i]]))
# the number of False Negatives Selected by each LASSO
BM1_FNs <- lapply(seq_along(Structural_Variables), \(i)
                  sum(Variables.Not.Selected.by.glmnet[[i]] %in% 
                        Structural_Variables[[i]]))

# the True Positive Rate
BM1_TPR = lapply(seq_along(Structural_Variables), \(j)
                 j <- (BM1_TPs[[j]]/BM1_NPs[[j]]) )

# the False Positive Rate = FP/(FP + TN)
BM1_FPR = lapply(seq_along(Structural_Variables), \(j)
                  j <- (BM1_FPs[[j]]/BM1_NNs[[j]]))
BM1_FPR2 = lapply(seq_along(Structural_Variables), \(j)
                 j <- (BM1_FPs[[j]])/(BM1_FPs[[j]] + BM1_TNs[[j]]))


# the True Negative Rate = TN/(FP + TN)
BM1_TNR <- lapply(seq_along(Structural_Variables), \(j) 
                  j <- (BM1_TNs[[j]]/BM1_NNs[[j]]))
BM1_TNR2 <- lapply(seq_along(Structural_Variables), \(w)
                   w <- (BM1_TNs[[w]]/(BM1_FPs[[w]] + BM1_TNs[[w]])))

# the False Negative Rate = FN/(FN + TP)
BM1_FNR = lapply(seq_along(Structural_Variables), \(j)
                 j <- (BM1_FNs[[j]]/BM1_NPs[[j]]))
BM1_FNR2 = lapply(seq_along(Structural_Variables), \(j)
                 j <- (BM1_FNs[[j]])/(BM1_FNs[[j]] + BM1_TPs[[j]]))

## calculate the accuracy and F1 score with help from GPT 4
BM1_Accuracy <- lapply(seq_along(Structural_Variables), function(i)
  (BM1_TPs[[i]] + BM1_TNs[[i]])/(BM1_TPs[[i]] + BM1_TNs[[i]] + BM1_FPs[[i]] + BM1_FNs[[i]]))

# First calculate precision and recall for each dataset
BM1_Precision <- lapply(seq_along(Structural_Variables), function(i)
  BM1_TPs[[i]]/(BM1_TPs[[i]] + BM1_FPs[[i]]))
BM1_Recall <- BM1_TPR  # You have already calculated recall as True Positive Rate (TPR)

# Then calculate F1 score for each dataset
BM1_F1_Score <- lapply(seq_along(Structural_Variables), function(i)
  2 * (BM1_Precision[[i]] * BM1_Recall[[i]])/(BM1_Precision[[i]] + BM1_Recall[[i]]))


## Write one or more lines of code which determine whether each selected 
## model is "Underspecified", "Correctly Specified", or "Overspecified".
# True Positive Rates as a vector rather than a list
TPR <- unlist(BM1_TPR)
mean_TPR <- round(mean(TPR), 3)
# number of selected regressions with at least one omitted variable
num_OMVs <- sum(TPR < 1, na.rm = TRUE)

# False Positive Rate as a vector rather than a list
FPR <- unlist(BM1_FPR)
mean_FPR <- round(mean(FPR), 3)
num_null_FPR <- sum(FPR == 0, na.rm = TRUE)
# number of models with at least one extraneous variable selected
num_Extraneous <- sum(FPR > 0, na.rm = TRUE)

# True Negative Rates as a vector rather than a list
TNR <- unlist(BM1_TNR)
TNR2 <- unlist(BM1_TNR2)
mean_TNR <- round(mean(TNR), 3)
mean_TNR2 <- round(mean(TNR2), 3)

# the False Negative Rates vectorized too
FNR <- unlist(BM1_FNR)
mean_FNR <- round(mean(FNR), 3)

# The Accuracy as a vector rather than a list
Acc <- unlist(BM1_Accuracy)
mean_Accuracy <- round(mean(Acc), 3)

# The F1 Scores as a vector rather than a list
F1 <- unlist(BM1_F1_Score)
mean_F1_Score <- round(mean(F1), 3)

# The Positive Predictive Values as a vector rather than a list
PPV <- unlist(BM1_Precision)
mean_PPV <- round(mean(PPV), 3)

# Number of Underspecified Regression Specifications Selected by LASSO
N_Under = sum( (TPR < 1) & (FPR == 0) )
N_Under2 = sum( (FNR > 0) & (FPR == 0) )

# Number of Correctly Specified Regressions Selected by LASSO
N_Correct <- sum( (TPR == 1) & (TNR == 1) )
N_Correct2 <- sum( (FPR == 0) & (FNR == 0) )

# Overspecified Regression Specifications Selected by LASSO
N_Over = sum( (TPR == 1) & (FPR > 0) )

# sum of all the 3 specification categories
Un_Corr_Ov = N_Under + N_Correct + N_Over


PMs1 <- data.frame(mean_Accuracy, mean_F1_Score, mean_PPV)
colnames(PMs1) <- c("Mean Accuracy", "Mean F1 Score", 
                    "Mean Positive Predictive Value")

Headers <- c("Mean True Positive Rate", "Mean True Negative Rate", 
             "Mean False Positive Rate", "Mean False Negative Rate")
PMs2 <- data.frame(mean_TPR, mean_TNR, mean_FPR, mean_FNR)
colnames(PMs2) <- Headers

Headers <- c("Underspecified Models Selected", 
             "Correctly Specified Models Selected",
             "Overspecified Models Selected")
PMs3 <- data.frame(N_Under, N_Correct, N_Over)
colnames(PMs3) <- Headers
rm(Headers)

Headers <- c("All Correct, Over, and Underspecified Models", 
             "Models with at least one Omitted Variable",
             "Models with at least one Extra Variable")
PMs4 <- data.frame(Un_Corr_Ov, num_OMVs, num_Extraneous)
colnames(PMs4) <- Headers

# Or, just print out this instead of having to print out 3 different things
performance_metrics <- data.frame(PMs1, PMs2, PMs3, PMs4)
# Combine all of the performance metrics into a data.frame
performance_metrics

setwd("C:/Users/spenc/OneDrive/Documents/Analytics Projects/EER project/TEMPS/temp folder for cv.glmnet/grouped by error variance")
getwd()
write.csv(performance_metrics, 
          file = "glmnet's Performance on the data_subset from '0-3-1-1 to 0-3-1-500'.csv", 
          row.names = FALSE)

