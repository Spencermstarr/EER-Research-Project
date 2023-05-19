# This script required the user to hit Run either by the button
# in the top right corner of this panel, or by hitting Ctrl+Enter

rm(list = ls())
# find out which working directory R has defaulted to
getwd()

# copy+paste whatever the path is for your file folder with all the 
# random synthetic csvs you have created inside a set of quotation marks
# inside of the setwd() function below and then press Run (or hit Ctrl+Enter)
#setwd()
# Or, click the Session option in the Ribbon, and in the
# Set Working Directory's drop down list, select Choose Directory
# in order to set it manually.

# load all necessary packages
library(dplyr)
library(stats)
library(leaps)
library(lars)
library(data.table)
library(parallel)



# these 2 lines together create a simple character list of 
# all the file names in the file folder of datasets you created
folderpath <- "C:/Users/Spencer/Documents/EER Project/Data/0.75-11-1-1 to 0.75-11-10-500"
paths_list <- list.files(path = folderpath, full.names = T, recursive = T)
paths_list

# reformat the names of each of the csv file formatted dataset
DS_names_list <- basename(paths_list)
DS_names_list <- tools::file_path_sans_ext(DS_names_list)
DS_names_list

# sort both of the list of file names so that they are in the proper order
my_order = DS_names_list |> 
  # split apart the numbers
  strsplit(split = "-", fixed = TRUE) |>
  unlist() |> 
  # convert them to numeric and get them in a data frame
  as.numeric() |> 
  matrix(nrow = length(DS_names_list), byrow = TRUE) |>
  as.data.frame() |>
  # get the appropriate ordering to sort the data frame
  do.call(order, args = _)

DS_names_list = DS_names_list[my_order]

paths_list = paths_list[my_order]



# The code below reads the data into the RStudio Workspace from
# each of the n datasets in an iterative manner in such a way 
# that it assigns each of them to the corresponding name of that 
# dataset in the file folder they are stored in.
system.time( datasets <- lapply(paths_list, fread) )
class(datasets)   # "list"
str(datasets)

# change column names of all the columns in the data.table 'datasets'
datasets <- lapply(datasets, function(dataset_i) { 
  colnames(dataset_i) <- c("Y","X1","X2","X3","X4","X5","X6","X7","X8",
                           "X9","X10","X11","X12","X13","X14","X15",
                           "X16","X17","X18","X19","X20","X21","X22", 
                           "X23","X24","X25","X26","X27","X28","X29","X30")
  dataset_i })

Structural_IVs <- lapply(datasets, function(j) {j[1, -1]})
Structural_Variables <- lapply(Structural_IVs, function(i) {names(i)[i == 1]})
head(Structural_Variables)
Nonstructural_Variables <- lapply(Structural_IVs, function(i) {names(i)[i == 0]})

# assign all 30 candidate regressor names to an object
var_names <- c("X1","X2","X3","X4","X5","X6","X7","X8",
               "X9","X10","X11","X12","X13","X14","X15",
               "X16","X17","X18","X19","X20","X21","X22", 
               "X23","X24","X25","X26","X27","X28","X29","X30")


datasets <- lapply(datasets, function(i) {i[-1:-3, ]})
datasets <- lapply(datasets, \(X) { lapply(X, as.numeric) })
datasets <- lapply(datasets, function(i) { as.data.table(i) })
datasets <- lapply(datasets, \(X) { round(X, 2) })








# This function fits all n LASSO regressions for/on
# each of the corresponding n datasets stored in the object
# of that name, then outputs standard regression results which 
# are typically called returned for any regression ran using R
set.seed(11)     # to ensure replicability
system.time(LASSO.Lars.fits <- lapply(X = datasets, function(i) 
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
                     Unselected_Variables = sapply(IVs.Not.Selected, 
                                                   toString),
                     Structural_Variables = sapply(Structural_Variables, 
                                                   toString),
                     NonStructural_Variables = sapply(Nonstructural_Variables, 
                                                   toString)),
          file = "Lars's Selections for the DSs from 0.75-15-1-1 to 0.75-15-10-500.csv", 
          row.names = FALSE)




### Count up how many Variables Selected match  the true 
### structural equation variables for that dataset in order
### to measure LASSO's performance.
# all of the "Positives", i.e. all the Structural Regressors
lars_NPs <- lapply(Structural_Variables, function(i) { length(i) })
# all of the "Negatives", i.e. all the Nonstructural Regressors
lars_NNs <- lapply(Structural_Variables, function(i) {30 - length(i)})


# the number True Positives for each LASSO
lars_TPs <- lapply(seq_along(datasets), \(i)
                              sum(IVs.Selected.by.Lars[[i]] %in% 
                                    Structural_Variables[[i]]))
# the number True Negatives for each LASSO
lars_TNs <- lapply(seq_along(datasets), \(k)
                  sum(IVs.Not.Selected[[k]] %in% 
                        Nonstructural_Variables[[k]]))

# the number of False Positives
lars_FPs <- lapply(seq_along(datasets), \(i)
                  sum(IVs.Selected.by.Lars[[i]] %in% 
                        Nonstructural_Variables[[i]]))
# the number of False Negatives Selected by each LASSO
lars_FNs <- lapply(seq_along(datasets), \(i)
                  sum(IVs.Not.Selected[[i]] %in% 
                        Structural_Variables[[i]]))


# the True Positive Rate
lars_TPR = lapply(seq_along(datasets), \(j)
                  j <- (lars_TPs[[j]]/lars_NPs[[j]]))

# the False Positive Rate = FP/(FP + TN)
lars_TNR = lapply(seq_along(datasets), \(j)
                  j <- (lars_FPs[[j]])/(lars_FPs[[j]] + lars_TNs[[j]]))
lars_TNR2 = lapply(seq_along(datasets), \(j)
                  j <- (lars_FPs[[j]])/(lars_FPs[[j]] + lars_TNs[[j]]))

# the True Negative Rate
lars_TNRs <- lapply(lars_TNR, \(i) 
                   i <- (1 - i))
lars_TNRs2 <- lapply(seq_along(datasets), \(w)
                    w <- (lars_TNs[[w]]/(lars_FPs[[w]] + lars_TNs[[w]])))


## Write one or more lines of code which determine whether each selected 
## model is "Underspecified", "Correctly Specified", or "Overspecified".
# True Positive Rates as a vector rather than a list
TPR <- unlist(lars_TPR)
mean_TPR <- round(mean(TPR), 3)
# number of selected regressions with at least one omitted variable
num_OMVs <- sum(TPR < 1, na.rm = TRUE)

# False Positive Rate as a vector rather than a list
TNR <- unlist(lars_TNR)
mean_FPR <- round(mean(TNR), 3)
num_null_FPR <- sum(TNR == 0, na.rm = TRUE)

# True Negative Rates as a vector rather than a list
TNRs <- unlist(lars_TNRs)
mean_TNR <- round(mean(TNRs), 3)


# Number of Underspecified Regression Specifications Selected by LASSO
N_Under = sum( (TPR < 1) & (TNR == 0) )

# Number of Correctly Specified Regressions Selected by LASSO
N_Correct <- sum( (TPR == 1) & (TNRs == 1) )

# number of models with at least one extraneous variable selected
num_Extraneous <- sum(FPR > 0, na.rm = TRUE)
# Overspecified Regression Specifications Selected by LASSO
N_Over = sum( (TPR == 1) & (FPR > 0) )

# sum of all the 3 specification categories
Un_Corr_Ov = N_Under + N_Correct + N_Over


Headers <- c("True Positive Rate", "True Negative Rate", 
             "False Positive Rate")
PMs1 <- data.frame(mean_TPR, mean_TNR, mean_FPR)
colnames(PMs1) <- Headers

Headers <- c("Underspecified Models Selected", 
             "Correctly Specified Models Selected",
             "Overspecified Models Selected", 
             "All Correct, Over, and Underspecified Models")
PMs2 <- data.frame(N_Under, N_Correct, N_Over, Num_Under_Correct_or_Over)
colnames(PMs2) <- Headers

Headers <- c("All Correct, Over, and Underspecified Models", 
             "Models with at least one Omitted Variable",
             "Models with at least one Extra Variable")
PMs3 <- data.frame(Un_Corr_Ov, num_OMVs, num_Extraneous)
colnames(PMs3) <- Headers
rm(num_OMVs, num_Extraneous)

# Or, just print out this instead of having to print out 3 different things
performance_metrics <- data.frame(PMs1, PMs2, PMs3)
performance_metrics

write.csv(performance_metrics, 
          file = "Lars's Performance on the datasets from 0.75-15-1-1 to 0.75-15-10-500.csv", 
          row.names = FALSE)


