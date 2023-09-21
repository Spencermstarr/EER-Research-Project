rm(list = ls())

library(tidyverse)
Selections_by_Lasso <- read_csv("LASSO's selections via cv.glmnet with lambda.1se for M = 0.75.csv", col_names = TRUE)
head(Selections_by_Lasso, n = 4)
str(Selections_by_Lasso)
attach(Selections_by_Lasso)
Structural_Variables <- Selections_by_Lasso$Structural_Variables
class(Structural_Variables)
Variables_Selected <- Selections_by_Lasso$Variables_Selected
NonStructural_Variables <- Selections_by_Lasso$NonStructural_Variables
Variables_Not_Selected <- Selections_by_Lasso$Variables_Not_Selected



### Count up how many Variables Selected match  the true 
### structural equation variables for that dataset in order
### to measure LASSO's performance.
# all of the "Positives", i.e. all the Structural Regressors
BM1_NPs <- lapply(Structural_Variables, function(i) { length(i) })

# all of the "Negatives", i.e. all the Nonstructural Regressors
BM1_NNs <- lapply(Structural_Variables, function(i) {30 - length(i)})


# all the "True Positives"
BM1_TPs <- lapply(seq_along(Structural_Variables), function(i) {
  selected_vars <- trimws(strsplit(Variables_Selected[[i]], ",")[[1]])
  structural_vars <- trimws(strsplit(Structural_Variables[[i]], ",")[[1]])
  sum(selected_vars %in% structural_vars)
})


# the number True Negatives
BM1_TNs <- lapply(seq_along(Structural_Variables), function(i) {
  selected_vars <- trimws(strsplit(Variables_Not_Selected[[i]], ",")[[1]])
  nonstructural_vars <- trimws(strsplit(NonStructural_Variables[[i]], ",")[[1]])
  sum(selected_vars %in% nonstructural_vars)
})


# the number of False Positives
BM1_FPs <- lapply(seq_along(Structural_Variables), function(i) {
  selected_vars <- trimws(strsplit(Variables_Selected[[i]], ",")[[1]])
  nonstructural_vars <- trimws(strsplit(NonStructural_Variables[[i]], ",")[[1]])
  sum(selected_vars %in% nonstructural_vars)
})


# the number of False Negatives Selected 
BM1_FNs <- lapply(seq_along(Structural_Variables), function(i) {
  not_selected_vars <- trimws(strsplit(Variables_Not_Selected[[i]], ",")[[1]])
  structural_vars <- trimws(strsplit(Structural_Variables[[i]], ",")[[1]])
  sum(not_selected_vars %in% structural_vars)
})


False_Negatives_and_Positives_Selected <- tibble(Data_Set_Name, BM1_FPs, BM1_FNs)
class(False_Negatives_and_Positives_Selected)

False_Negatives_and_Positives_Selected$BM1_FPs <- sapply(False_Negatives_and_Positives_Selected$BM1_FPs, paste, collapse = ",")
False_Negatives_and_Positives_Selected$BM1_FNs <- sapply(False_Negatives_and_Positives_Selected$BM1_FNs, paste, collapse = ",")

write_csv(x = False_Negatives_and_Positives_Selected, 
          file = "False Positives & Negatives Selected by Lasso, M = 0.75.csv", 
          col_names = TRUE)