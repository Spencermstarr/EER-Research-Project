#### My EER script for the sake of practicing,
#### tinkering, debugging, and blueprinting on a 
#### file folder with only 15 datasets instead of 
#### 47,500 datasets.



### If one wanted to run this entire script all at once,
### all he would need do is hit Ctrl+Shift+Enter
getwd()
setwd("~/DAEN_698/EER")
getwd()




# Load all libraries needed for this script.
# The library specifically needed to run a basic ASR is 
# the leaps library.
library(dplyr)
library(tidyverse)

library(ggplot2)
library(lattice)
library(caret)

library(stats)
library(combinat)
library(leaps)
library(purrr)




# Extract all of the individual spreadsheet containing workbooks
# in the file folder called 'Run_3_seven) which is filled
# random synthetic observations to run Lasso, regsubsetswise, and eventually EER
# on to compare the results. There are 531 random spreadsheets in this folder
directory_path <- "~/DAEN_698/other datasets/sample obs"
filepath_list <- list.files(path = directory_path, full.names = TRUE, recursive = TRUE)

# reformat the names of each of the csv file formatted datasets
DS_names_list <- basename(filepath_list)
DS_names_list <- tools::file_path_sans_ext(DS_names_list)


datasets <- lapply(filepath_list, read.csv)


ER_fits <- lapply(datasets, function(i)
                              regsubsets(x = as.matrix(select(i, starts_with("X"))), 
                                         y = i$Y, data = i, nbest = 1, nvmax = NULL, 
                                         force.in = NULL, force.out = NULL,
                                         intercept = TRUE, method = "exhaustive"))
#ER_fits_summary <- summary(ER_fits)
#ER_fits_summary
ER_summaries <- lapply(ER_fits, summary(regsubsets.out))
as.data.frame(ER_summaries$outmat)
which.max(ER_summaries$adjr2)
ER_1_summary <- summary(ER_fits[[1]], regsubsets.out)

summary(ER_fits$which)


which.max(summary_best_subset$adjr2)



ER_Coeffs <- lapply(ER_fits, function(i) coef(i, id = 3:15))

Positive_Coeffs <- lapply(ER_Coeffs, function(i) i[i > 0])
IVs_Selected_by_ER <- lapply(ER_Coeffs, function(i) names(i[i > 0]))
IVs_selected_by_ER <- lapply(seq_along(ER_fits), 
                                   \(i) names(coef(ER_fits[[i]], id = 3:15)))

ER_fits2 <- lapply(datasets, function(i)
  leaps(x = as.matrix(select(i, starts_with("X"))), y = i$Y, 
        int = TRUE, nbest = 1))

ER2_Coeffs <- lapply(ER_fits2, function(i) coef(i, id = 3:15))
ER2_Coeffs <- lapply(ER_fits2, function(i) coef(i))



?sample
#sample(models_selected_by_ASR, n = 15, size = 5, replace = FALSE)
rand_sample <- sample(models_selected_by_ASR, size = 3, replace = FALSE)
rand_sample
#round(rand_sample, 2)
#sample(round(models_selected_by_ASR, size = 3, replace = FALSE), 2)
summary(models_selected_by_ASR)
?round




##### Try using the sample(*, j) function to randomly select the 
##### j (where j = 100, 200, 300, or 400, etc.) regression specifications
##### out of the 12,000 possible specification permutations to run.
library(sampling)
vector1 <- (1:3)
vector1
prms_of_vector1 <- permn(vector1)
prms_of_vector1
set.seed(11)     # to ensure replicability
sample(x = as.matrix(datasets[-1], 5 ))
x1 <- 12
x2 <- 1:12
sample(x1[x1 > 10])
length(sample(x1[x1 > 10]))
sample(x1[x1 > 12])
sample(x1[x1 > 8]) # length 2
?sample()
length(sample(x1[x1 > 10], ))
length(sample(x1[x1 > 11], ))
length(sample(x1[x1 > 12], ))
length(sample(x1[x1 > 13], ))












