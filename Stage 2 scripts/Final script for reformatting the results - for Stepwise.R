# load the tidyverse because you will need to have the readr package
library(tidyverse)
Selections_by_Stepwise <- read_csv("All selections made by Stepwise, csv version.csv", col_names = TRUE)
# inspect the data
head(Selections_by_Stepwise, n = 4)
str(Selections_by_Stepwise)

# only need to import one dataset, so just attach it for convenience 
attach(Selections_by_Stepwise)
Structural_Variables <- Selections_by_Stepwise$Structural.Variables
class(Structural_Variables)
NonStructural_Variables <- Selections_by_Stepwise$NonStructural.Variables

# BM2 stands for the second benchmark variable selection algorithm, 
# which is Backward Elimination Stepwise Regression
BM2_Variables_Selected <- Selections_by_Stepwise$Candidate.Variables.Selected.by.Backward.Elimination.Stepwise.Regression
BM2_Variables_Not_Selected <- Selections_by_Stepwise$Candidate.Variables.Not.Selected.by.Backward.Elimination.Stepwise.Regression

# BM3 stands for the third benchmark variable selection algorithm, 
# which is Forward Selection Stepwise Regression
BM3_Variables_Selected <- Selections_by_Stepwise$Candidate.Variables.Selected.by.Forward.Selection.Stepwise.Regression
BM3_Variables_Not_Selected <- Selections_by_Stepwise$Candidate.Variables.Not.Selected.by.Forward.Selection.Stepwise.Regression


### Start out with the 2nd Benchmark, Backward Elimination.
### Count up how many Variables Selected match  the true 
### structural equation variables for that dataset in order
### to measure the performance of Backward Elimination.
# all of the "Positives", i.e. all the Structural Regressors
NPs <- lapply(Structural_Variables, function(i) {
  structural_vars <- trimws(strsplit(i, ",")[[1]])
  length(structural_vars) })

# all of the "Negatives", i.e. all the Nonstructural Regressors
NNs <- lapply(Structural_Variables, function(i) {
  structural_vars <- trimws(strsplit(i, ",")[[1]])
  total_vars <- 30  # Assuming there are 30 total variables
  length(structural_vars)
  nonstructural_count <- total_vars - length(structural_vars)
  return(nonstructural_count) })
NNs2 <- lapply(NonStructural_Variables, function(i) {
  nonstructural_vars <- trimws(strsplit(i, ",")[[1]])
  length(nonstructural_vars) })

# all the "True Positives"
BM2_TPs <- lapply(seq_along(Structural_Variables), function(i) {
  selected_vars <- trimws(strsplit(Variables_Selected[[i]], ",")[[1]])
  structural_vars <- trimws(strsplit(Structural_Variables[[i]], ",")[[1]])
  sum(selected_vars %in% structural_vars) })

# the number True Negatives
BM2_TNs <- lapply(seq_along(Structural_Variables), function(i) {
  selected_vars <- trimws(strsplit(Variables_Not_Selected[[i]], ",")[[1]])
  nonstructural_vars <- trimws(strsplit(NonStructural_Variables[[i]], ",")[[1]])
  sum(selected_vars %in% nonstructural_vars) })

# the number of False Positives
BM2_FPs <- lapply(seq_along(Structural_Variables), function(i) {
  selected_vars <- trimws(strsplit(Variables_Selected[[i]], ",")[[1]])
  nonstructural_vars <- trimws(strsplit(NonStructural_Variables[[i]], ",")[[1]])
  sum(selected_vars %in% nonstructural_vars) })

# the number of False Negatives Selected 
BM2_FNs <- lapply(seq_along(Structural_Variables), function(i) {
  not_selected_vars <- trimws(strsplit(Variables_Not_Selected[[i]], ",")[[1]])
  structural_vars <- trimws(strsplit(Structural_Variables[[i]], ",")[[1]])
  sum(not_selected_vars %in% structural_vars) })

False_Negatives_and_Positives_Selected <- tibble(Dataset.Name, BM2_FPs, BM2_FNs)
class(False_Negatives_and_Positives_Selected)

False_Negatives_and_Positives_Selected$BM2_FPs <- sapply(False_Negatives_and_Positives_Selected$BM2_FPs, paste, collapse = ",")
False_Negatives_and_Positives_Selected$BM2_FNs <- sapply(False_Negatives_and_Positives_Selected$BM2_FNs, paste, collapse = ",")

write_csv(x = False_Negatives_and_Positives_Selected, 
          file = "False Positives & Negatives Selected by BE Stepwise.csv", 
          col_names = TRUE)




### Now, proceed on with the 3rd Benchmark, Forward Selection.
### Count up how many Variables Selected match  the true 
### structural equation variables for that dataset in order
### to measure the performance of Forward Selection.
# all the "True Positives"
BM3_TPs <- lapply(seq_along(Structural_Variables), function(i) {
  selected_vars <- trimws(strsplit(Variables_Selected[[i]], ",")[[1]])
  structural_vars <- trimws(strsplit(Structural_Variables[[i]], ",")[[1]])
  sum(selected_vars %in% structural_vars) })

# the number True Negatives
BM3_TNs <- lapply(seq_along(Structural_Variables), function(i) {
  selected_vars <- trimws(strsplit(Variables_Not_Selected[[i]], ",")[[1]])
  nonstructural_vars <- trimws(strsplit(NonStructural_Variables[[i]], ",")[[1]])
  sum(selected_vars %in% nonstructural_vars) })

# the number of False Positives
BM3_FPs <- lapply(seq_along(Structural_Variables), function(i) {
  selected_vars <- trimws(strsplit(Variables_Selected[[i]], ",")[[1]])
  nonstructural_vars <- trimws(strsplit(NonStructural_Variables[[i]], ",")[[1]])
  sum(selected_vars %in% nonstructural_vars) })

# the number of False Negatives Selected 
BM3_FNs <- lapply(seq_along(Structural_Variables), function(i) {
  not_selected_vars <- trimws(strsplit(Variables_Not_Selected[[i]], ",")[[1]])
  structural_vars <- trimws(strsplit(Structural_Variables[[i]], ",")[[1]])
  sum(not_selected_vars %in% structural_vars) })

False_Negatives_and_Positives_Selected <- tibble(Dataset.Name, BM3_FPs, BM3_FNs)
class(False_Negatives_and_Positives_Selected)

False_Negatives_and_Positives_Selected$BM3_FPs <- sapply(False_Negatives_and_Positives_Selected$BM3_FPs, paste, collapse = ",")
False_Negatives_and_Positives_Selected$BM3_FNs <- sapply(False_Negatives_and_Positives_Selected$BM3_FNs, paste, collapse = ",")

write_csv(x = False_Negatives_and_Positives_Selected, 
          file = "False Positives & Negatives Selected by FS Stepwise.csv", 
          col_names = TRUE)