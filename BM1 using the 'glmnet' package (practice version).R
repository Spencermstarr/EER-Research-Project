# This script required the user to hit Run either by the button
# in the top right corner of this panel, or by hitting Ctrl+Enter

# find out which working directory R has defaulted to
rm(list = ls())
#setwd("D:/EER")
#setwd("D:/EER folder")
#setwd("C:/Users/Spencer/OneDrive/Documents/Analytics Projects/EER/1st Benchmark")
#getwd()

# copy+paste whatever the path is for your file folder with all the 
# random synthetic csvs you have created inside a set of quotation marks
# inside of the setwd() function below and then press Run (or hit Ctrl+Enter)
#setwd()
# Or, click the Session option in the Ribbon, and in the
# Set Working Directory's drop down list, select Choose Directory
# in order to set it manually.

# load all necessary packages
library(dplyr)
library(stringi)
library(purrr)
library(stats)
library(Matrix)
library(glmnet)
library(data.table)
library(parallel)



# these 2 lines together create a simple character list of 
# all the file names in the file folder of datasets you created
folderpath <- "C:/Users/Spencer/Documents/EER_Project/csvs/0-11-1-1 to 0-11-10-500"
paths_list <- list.files(path = folderpath, full.names = T, recursive = T)

# reformat the names of each of the csv file formatted dataset
DS_names_list <- basename(paths_list)
DS_names_list <- tools::file_path_sans_ext(DS_names_list)

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
# each of the n data sets in an iterative manner in such a way 
# that it assigns each of them to the corresponding name of that 
# dataset in the file folder they are stored in.
#CL <- makeCluster(detectCores() - 5L)
#clusterExport(CL, c('paths_list'))
#system.time( datasets <- parLapply(CL, paths_list, fread))

system.time( datasets <- lapply(paths_list, fread) )
#stopCluster(CL)


# change column names of all the columns in the data.table 'datasets'
datasets <- lapply(datasets, function(dataset_i) { 
  colnames(dataset_i) <- c("Y","X1","X2","X3","X4","X5","X6","X7","X8",
                           "X9","X10","X11","X12","X13","X14","X15",
                           "X16","X17","X18","X19","X20","X21","X22", 
                           "X23","X24","X25","X26","X27","X28","X29","X30")
  dataset_i })

# assign all 30 candidate regressor names to an object
var_names <- c("X1","X2","X3","X4","X5","X6","X7","X8",
               "X9","X10","X11","X12","X13","X14","X15",
               "X16","X17","X18","X19","X20","X21","X22", 
               "X23","X24","X25","X26","X27","X28","X29","X30")

Structural_IVs <- lapply(datasets, function(j) {j[1, -1]})
Structural_Variables <- lapply(Structural_IVs, function(i) {
  names(i)[i == 1] })
Nonstructural_Variables <- lapply(Structural_IVs, function(i) {
  names(i)[i == 0] })


datasets <- lapply(datasets, function(i) {i[-1:-3, ]})
datasets <- lapply(datasets, \(X) { lapply(X, as.numeric) })
datasets <- lapply(datasets, function(i) { as.data.table(i) })
datasets <- lapply(datasets, \(X) { round(X, 2) })

save.image("C:/Users/Spencer/OneDrive/Documents/Analytics Projects/EER Project/Saved WorkSpaces/datasets WorkSpace for '0-11-1-1 to 0-11-10-500'.RData")






# This function fits all n LASSO regressions for/on
# each of the corresponding n datasets stored in the object
# of that name, then outputs standard regression results which 
# are typically called returned for any regression ran using R.
#CL3 <- makeCluster(detectCores() - 1L)
#clusterExport(CL3, c('datasets'))
set.seed(11)     # to ensure replicability
system.time(L.fits <- lapply(X = datasets, function(i) 
               glmnet(x = as.matrix(select(i, starts_with("X"))), 
                      y = i$Y, alpha = 1)))


# This stores and prints out all of the regression 
# equation specifications selected by LASSO when called
L.coefs = L.fits |> 
  Map(f = \(model) coef(model, s = .1))

Variables.Selected <- L.coefs |>
  Map(f = \(matr) matr |> as.matrix() |> 
       as.data.frame() |> filter(s1 != 0) |> rownames())

Variables.Selected = lapply(seq_along(datasets), \(j)
                            j <- (Variables.Selected[[j]][-1]))

Variables.Not.Selected <- L.coefs |>
  Map(f = \(matr) matr |> as.matrix() |> 
        as.data.frame() |> filter(s1 == 0) |> rownames())

#setwd("C:/Users/Spencer/OneDrive/Documents/Analytics Projects/EER/1st Benchmark/Replications using 'glmnet'/Variables selected by glmnet")
write.csv(
  data.frame(DS_name = DS_names_list, 
             Variables.Selected.by.glmnet = sapply(Variables.Selected, toString),
             Structural_Variables = sapply(Structural_Variables, 
                                           toString),
             Nonstructural_Variables = sapply(Nonstructural_Variables, 
                                              toString)),
  file = "LASSO's Selections via glmnet for the DSs from '0-11-1-1 to 0-11-10-500'.csv", 
  row.names = FALSE)





### Count up how many Variables Selected match  the true 
### structural equation variables for that dataset in order
### to measure LASSO's performance.
# all of the "Positives", i.e. all the Structural Regressors
glmnet_NPs <- lapply(Structural_Variables, function(i) { length(i) })

# all of the "Negatives", i.e. all the Nonstructural Regressors
glmnet_NNs <- lapply(Structural_Variables, function(i) {30 - length(i)})
glmnet_NNs2 <- lapply(Nonstructural_Variables, function(i) {length(i)})

# all the "True Positives"
glmnet_TPs <- lapply(seq_along(datasets), \(i)
                  sum(Variables.Selected[[i]] %in% 
                        Structural_Variables[[i]]))
# the number True Negatives for each LASSO
glmnet_TNs <- lapply(seq_along(datasets), \(k)
                   sum(Variables.Not.Selected[[k]] %in% 
                         Nonstructural_Variables[[k]]))

# the number of False Positives
glmnet_FPs <- lapply(seq_along(datasets), \(i)
                  sum(Variables.Selected[[i]] %in% 
                        Nonstructural_Variables[[i]]))
# the number of False Negatives Selected by each LASSO
glmnet_FNs <- lapply(seq_along(datasets), \(i)
                  sum(Variables.Not.Selected[[i]] %in% 
                        Structural_Variables[[i]]))


# the True Positive Rate
glmnet_TPR = lapply(seq_along(datasets), \(j)
                  j <- (glmnet_TPs[[j]]/glmnet_NPs[[j]]) )
glmnet_TPR2 = lapply(seq_along(datasets), \(j)
                  j <- (glmnet_TPs[[j]]/(glmnet_TPs[[j]] + glmnet_FNs[[j]])) )

# the False Positive Rate = FP/(FP + TN)
glmnet_FPR = lapply(seq_along(datasets), \(j)
                  j <- (glmnet_FPs[[j]])/(glmnet_FPs[[j]] + glmnet_TNs[[j]]))
# or, another way to calculate it would be
glmnet_FPR2 = lapply(seq_along(datasets), \(i)
                   i <- (glmnet_FPs[[i]])/(glmnet_NNs[[i]]))


# the True Negative Rate = TN/(FP + TN)
glmnet_TNR <- lapply(seq_along(datasets), \(j) 
                   j <- (glmnet_TNs[[j]]/glmnet_NNs[[j]]))

glmnet_TNR2 <- lapply(seq_along(datasets), \(w)
                    w <- (glmnet_TNs[[w]]/(glmnet_FPs[[w]] + glmnet_TNs[[w]])))
# or, because TNR = 1 - FPR, we can also use
glmnet_TNR3 <- lapply(glmnet_FPR, \(i) 
                    i <- (1 - i))


## Write one or more lines of code which determine whether each selected 
## model is "Underspecified", "Correctly Specified", or "Overspecified".
# True Positive Rates as a vector rather than a list
TPR <- unlist(glmnet_TPR)
mean_TPR <- round(mean(TPR), 3)

TPR2 <- unlist(glmnet_TPR2)
mean_TPR2 <- round(mean(TPR2), 3)
# number of selected regressions with at least one omitted variable
num_OMVs <- sum(TPR < 1, na.rm = TRUE)

# False Positive Rate as a vector rather than a list
FPR <- unlist(glmnet_FPR)
FPR2 <- unlist(glmnet_FPR2)
mean_FPR <- round(mean(FPR), 3)
mean_FPR2 <- round(mean(FPR2), 3)
num_null_FPR <- sum(FPR == 0, na.rm = TRUE)
# number of models with at least one extraneous variable selected
num_Extraneous <- sum(FPR > 0)


# True Negative Rates as a vector rather than a list
TNR <- unlist(glmnet_TNR)
mean_TNR <- round(mean(TNR), 3)

TNR2 <- unlist(glmnet_TNR2)
TNR3 <- unlist(glmnet_TNR3)
mean_TNR2 <- round(mean(TNR2), 3)
mean_TNR3 <- round(mean(TNR3), 3)


# Number of Underspecified Regression Specifications Selected by LASSO
N_Under = sum( (TPR < 0) & (FPR == 0) )

# Number of Correctly Specified Regressions Selected by LASSO
N_Correct <- sum( (TPR == 1) & (TNR == 1) )

# Overspecified Regression Specifications Selected by LASSO
N_Over = sum( (TPR == 1) & (FPR > 0) )

# sum of all the 3 specification categories
Un_Corr_Ov = N_Under + N_Correct + N_Over


Headers <- c("True Positive Rate", "True Negative Rate", 
             "False Positive Rate")
PMs1 <- data.frame(mean_TPR, mean_TNR, mean_FPR)
colnames(PMs1) <- Headers
rm(mean_TPR, mean_TNR, mean_FPR)

Headers <- c("Underspecified Models Selected", 
             "Correctly Specified Models Selected",
             "Overspecified Models Selected")
PMs2 <- data.frame(N_Under, N_Correct, N_Over)
colnames(PMs2) <- Headers
rm(N_Under, N_Correct, N_Over)


Headers <- c("All Correct, Over, and Underspecified Models", 
             "Models with at least one Omitted Variable",
             "Models with at least one Extra Variable")
PMs3 <- data.frame(Un_Corr_Ov, num_OMVs, num_Extraneous)
colnames(PMs3) <- Headers

# Or, just print out this instead of having to print out 3 different things
performance_metrics <- data.frame(PMs1, PMs2, PMs3)
performance_metrics

#setwd("C:/Users/Spencer/OneDrive/Documents/Analytics Projects/EER/1st Benchmark/Replications using 'glmnet'")
write.csv(performance_metrics, 
          file = "glmnet's Performance on the datasets from '0-11-1-1 to 0-11-10-500'.csv", 
          row.names = FALSE)
#stopCluster(CL)

length(datasets)
head(DS_names_list)
tail(DS_names_list)
#rm(CL, Structural_Variables, Variables.Selected, LASSO_fits, LASSO_Coeffs)
#rm(glmnet_FPR, glmnet_FPs, glmnet_NNs, glmnet_NPs, glmnet_TNR, glmnet_TPR, glmnet_TPs)
#rm(FPR, TPR, TNR, Headers, num_null_FPR, performance_metrics, PMs1, PMs2, PMs3)

