# This script required the user to hit Run either by the button
# in the top right corner of this panel, or by hitting Ctrl+Alt+R

# load all necessary packages using only 1 command/line
library_list <- c(library(plyr),library(dplyr),library(tidyverse),
                  library(readr),library(stringi),library(purrr),
                  library(stats),library(leaps),library(lars),
                  library(elasticnet),library(data.table),library(parallel))


# these 2 lines together create a simple character list of 
# all the file names in the file folder of datasets you created
folderpath <- "C:/Users/Spencer/Documents/EER Project/last 40"
paths_list <- list.files(path = folderpath, full.names = T, recursive = T)

# reformat the names of each of the csv file formatted datasets
DS_names_list <- basename(paths_list)
DS_names_list <- tools::file_path_sans_ext(DS_names_list)


# sort both of the list of file names so that they are in the proper order
my_order = DS_names_list |> 
  # split apart the numbers, convert them to numeric 
  strsplit(split = "-", fixed = TRUE) |>  unlist() |> as.numeric() |>
  # get them in a data frame
  matrix(nrow = length(DS_names_list), byrow = TRUE) |> as.data.frame() |>
  # get the appropriate ordering to sort the data frame
  do.call(order, args = _)

DS_names_list = DS_names_list[my_order]
paths_list = paths_list[my_order]



# this line reads all of the data in each of the csv files 
# using the name of each store in the list we just created
#CL <- makeCluster(detectCores() - 1L)
#clusterExport(CL, c('paths_list'))
#system.time( datasets <- parLapply(CL, paths_list, fread) )
datasets <- parLapply(CL, paths_list, function(i) {fread(i)})
#system.time( datasets <- lapply(paths_list, fread) )
#stopCluster(CL)


# change column names of all the columns in the data.table 'datasets'
datasets <- lapply(datasets, function(dataset_i) { 
  colnames(dataset_i) <- c("Y", "X1", "X2", "X3", "X4", "X5", "X6", "X7", 
                           "X8", "X9", "X10", "X11", "X12", "X13", "X14", 
                           "X15", "X16", "X17", "X18", "X19", "X20", "X21", 
                           "X22", "X23", "X24", "X25", "X26", "X27", "X28", 
                           "X29", "X30")
  dataset_i })


Structural_IVs_chr <- lapply(datasets, function(j) {j[1, -1]})
Structural_IVs_num <- lapply(Structural_IVs_chr, \(X) { lapply(X, as.numeric) })


True_Regressors <- lapply(Structural_IVs_chr, function(i) {
  names(i)[i == 1] })


system.time(datasets <- lapply(datasets, function(i) {i[-1:-3, ]}))
system.time(datasets <- lapply(datasets, \(X) { lapply(X, as.numeric) }))
system.time(datasets <- lapply(datasets, function(i) { as.data.table(i) }))
#datasets <- lapply(datasets, \(X) { round(X, 2) })



# This function fits all 260,000 LASSO regressions for/on
# each of the corresponding 260k datasets stored in the object
# of that name, then outputs standard regression results which 
# are typically called returned for any regression ran using R
#CL3 <- makeCluster(detectCores() - 1L)
#clusterExport(CL3, c('datasets'))
library(elasticnet)
set.seed(11)     # to ensure replicability
system.time(LASSO_fits <- parLapply(CL, datasets, function(i) 
               enet(x = as.matrix(select(i, starts_with("X"))), 
               y = i$Y, lambda = 0, normalize = FALSE)))
#stopCluster(CL3)

## This stores and prints out all of the regression 
## equation specifications selected by LASSO when called.
# Press Run or hit Ctrl+Enter only once for the 5 lines below
LASSO_Coeffs <- lapply(LASSO_fits, 
                       function(i) predict(i, 
                                           x = as.matrix(select(i, starts_with("X"))), 
                                           s = 0.1, mode = "fraction", 
                                           type = "coefficients")[["coefficients"]])

stopCluster(CL)

### Write my own custom function which will separate out and return a 
### new list containing just the Independent Variables
### which are 'selected' or chosen for each individual dataset.
# Press Run or hit Ctrl+Enter
IVs_Selected_by_LASSO <- lapply(LASSO_Coeffs, function(i) names(i[i > 0]))
write.csv(data.frame(DS_name = DS_names_list, 
                     IVs_selected = sapply(IVs_Selected_by_LASSO, toString)), 
          file = "IVs_Selected_by_LASSO.csv", row.names = FALSE)




### Count up how many Variables Selected match  the true 
### structural equation variables for that dataset in order
### to measure LASSO's performance.
BM1_NPs <- lapply(True_Regressors, function(i) { length(i) })

system.time(BM1_TPs <- lapply(seq_along(datasets), \(i)
                              sum(IVs_Selected_by_LASSO[[i]] %in% 
                                    True_Regressors[[i]]))) 

BM1_TPRs = lapply(seq_along(datasets), \(j)
                  j <- (BM1_TPs[[j]]/BM1_NPs[[j]]) )

# the False Positive Rate
BM1_NNs <- lapply(True_Regressors, function(i) {30 - length(i)})
BM1_FPs <- lapply(seq_along(datasets), \(i)
                  sum(!(IVs_Selected_by_LASSO[[i]] %in% 
                          True_Regressors[[i]]))) 
BM1_FPRs = lapply(seq_along(datasets), \(j)
                  j <- (BM1_FPs[[j]])/BM1_NPs[[j]])

# the True Negative Rate
BM1_TNRs <- lapply(BM1_FPRs, \(i) 
                   i <- (1 - i))


## Write one or more lines of code which determine whether each selected 
## model is "Underspecified", "Correctly Specified", or "Overspecified".
# True Positive Rates as a vector rather than a list
TPRs <- unlist(BM1_TPRs)
TPR <- round(mean(TPRs), 3)
# number of selected regressions with at least one omitted variable
num_OMVs <- sum(TPRs < 1, na.rm = TRUE)

# False Positive Rate as a vector rather than a list
FPRs <- unlist(BM1_FPRs)
FPR <- round(mean(FPRs), 3)
num_null_FPR <- sum(FPRs == 0, na.rm = TRUE)

# True Negative Rates as a vector rather than a list
TNRs <- unlist(BM1_TNRs)
TNR <- round(mean(TNRs), 3)


# number of models with at least one extraneous variable selected
num_Extraneous <- sum(FPRs > 0)
# Overspecified Regression Specifications Selected by LASSO
N_Over = sum( (FPRs > 0) & (TPRs == 1) & (TNRs == 1) )

# Number of Underspecified Regression Specifications Selected by LASSO
N_Under = sum( (TPRs < 1) & (FPRs == 0) )

# Number of Correctly Specified Regressions Selected by LASSO
N_Correct <- sum( (TPRs == 1) & (FPRs == 0) & (TNRs == 1) )


Headers <- c("True Positive Rate", "True Negative Rate", "False Positive Rate")
Performance1 <- data.frame(TPR, TNR, FPR)
colnames(Performance1) <- Headers

Headers <- c("Underspecified Models Selected", 
              "Correctly Specified Models Selected",
              "Overspecified Models Selected")
Performance2 <- data.frame(N_Under, N_Correct, N_Over)
colnames(Performance2) <- Headers

Headers <- c("Models with at least one Omitted Variable",
              "Models with at least one Extra Variable")
Performance3 <- data.frame(num_OMVs, num_Extraneous)
colnames(Performance3) <- Headers
