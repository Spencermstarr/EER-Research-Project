# Spencer Marlen-Starr's RStudio script of functions, methods, code, and 
# code comments required for running Benchmark #2, namely the 
# Backward Elimination version of Stepwise Regression
# on some of our random synthetic observations to see how well it 
# does so we can compare its results with Dr. Davies EER procedure.


# find out which working directory R has defaulted to
getwd()

# copy+paste whatever the path is for your file folder with all the 
# random synthetic csvs you have created inside a set of quotation marks
# inside of the setwd() function below and then press Run (or hit Ctrl+Enter)
setwd()
# Or, click the Session option in the Ribbon, and in the
# Set Working Directory's drop down list, select Choose Directory
# in order to set it manually.


# load all necessary packages using only 1 command/line
library_list <- c(library(stats),library(dplyr),library(tidyverse),
                  library(leaps),library(lars),library(olsrr),
                  library(readr),library(stringi),library(purrr),
                  library(data.table),library(parallel))

library(data.table)
library(olsrr)
library(parallel)


# Extract all of the individual spreadsheet containing workbooks
# in the file folder called 'last 20' which is filled
# random synthetic observations to run Lasso, Stepwise, and eventually EER
# on to compare the results. There are 40 random spreadsheets in this folder
folderpath <- "C:/Users/Spencer/Documents/EER Project/Data/top 100"
paths_list <- list.files(path = folderpath, full.names = T, recursive = T)
length(paths_list)
str(paths_list)

# reformat the names of each of the csv file formatted csvs
DS_names_list <- basename(paths_list)
DS_names_list <- tools::file_path_sans_ext(DS_names_list)
head(DS_names_list, n = 4)

my_order = DS_names_list |> 
  # split apart the numbers
  strsplit(split = "-", fixed = TRUE) |>  unlist() |> 
  # convert them to numeric and get them in a data frame
  as.numeric() |> 
  matrix(nrow = length(DS_names_list), byrow = TRUE) |>
  as.data.frame() |>
  # get the appropriate ordering to sort the data frame
  do.call(order, args = _)
my_order

DS_names_list = DS_names_list[my_order]
DS_names_list

paths_list = paths_list[my_order]
paths_list


# Import all 20 of the individual csv files, each of which
# containing 500 rows by 31 columns worth of randomly generated 
# (synthetic) observations below.
# In order to accomplish this, I will be using the readr library in R.
## This line reads all of the data in each of the csv files 
## using the name of each store in the list we just created.
#datasets <- lapply(paths_list, fun = data.table::fread)
datasets <- lapply(paths_list, fread)

# change column names of all the columns in the data.table 'datasets'
datasets <- lapply(datasets, function(dataset_i) { 
  colnames(dataset_i) <- c("Y","X1","X2","X3","X4","X5","X6","X7","X8",
                           "X9","X10","X11","X12","X13","X14","X15",
                           "X16","X17","X18","X19","X20","X21","X22", 
                           "X23","X24","X25","X26","X27","X28","X29","X30")
  dataset_i })

Structural_Variables <- lapply(datasets, function(j) {j[1, -1]})
True_Regressors <- lapply(Structural_Variables, function(i) 
{ names(i)[i == 1] })

# change column names of all the columns in the dataframe 'Structural_Variables'
Structural_Variables <- lapply(Structural_Variables, function(dataset_i) {
  colnames(dataset_i) <- c("X1","X2", "X3", "X4","X5", "X6", "X7", "X8", "X9", 
                   "X10","X11", "X12", "X13","X14", "X15", "X16","X17",
                   "X18", "X19","X20", "X21", "X22","X23", "X24", "X25", 
                   "X26", "X27", "X28","X29", "X30")
  dataset_i})


datasets <- lapply(datasets, function(i) {i[-1:-3, ]})
datasets <- lapply(datasets, \(X) { lapply(X, as.numeric) })
datasets <- lapply(datasets, function(i) { as.data.table(i) })
datasets <- lapply(datasets, \(X) { round(X, 3) })



### Benchmark 2: Run a Backward Elimination Stepwise Regression
### function on each of the  csvs.
### Assign the full models to their corresponding csvs and
### store these in the object "all_regressors_models"
set.seed(11)      # for reproducibility
BE_fits <- lapply(X = datasets, \(i) {
    full_models <- lm(i$Y ~ ., data = i)
    backward <- ols_step_backward_aic(full_models) })

BE_fits <- lapply(seq_along(datasets), \(i) {
  full_models <- lm(i$Y ~ ., data = i)
  backward <- ols_step_backward_aic(full_models) })

BE_fits <- lapply(seq_along(datasets), \(i) {
  full_models <- lm(datasets[[i]]$Y ~ ., data = datasets[[i]])
  backward <- ols_step_backward_aic(full_models) })

BE_fits <- lapply(seq_along(datasets), \(i) {
  full_models <- lm(i$Y ~ ., as.data.frame(i))
  backward <- ols_step_backward_aic(full_models) })

full <- lm(datasets[[5]]$Y ~ ., data = datasets[[5]])
backward_elimination <- ols_step_backward_aic(full)

BE.fits <- lapply(X = datasets, \(X) {
  full_models <- lm(X$Y ~ ., X)
  back <- stats::step(full_models, scope = formula(full_models), 
                      direction = 'backward', trace = FALSE) })


BE_fits <- lapply(X = seq_along(datasets), \(ds) {
  full_models <- lm(ds$Y ~ X1+X2+X3+X4+X5+X6+X7+X8+X9+X10+X11+X12+X13+X14+X15+X16+X17+X18+X19+X20+X21+X22+X23+X24+X25+X26+X27+X28+X29+X30, 
                    data = ds)
  backward <- ols_step_backward_aic(full_models) })

BE_fits <- lapply(X = seq_along(datasets), \(ds) {
  full_models <- lm(ds$Y ~ X1+X2+X3+X4+X5+X6+X7+X8+X9+X10+X11+X12+X13+X14+X15+X16+X17+X18+X19+X20+X21+X22+X23+X24+X25+X26+X27+X28+X29+X30, 
                    data = datasets[ds])
  backward <- ols_step_backward_aic(full_models) })





BE_Coeffs <- lapply(seq_along(BE.fits), function(i) coef(BE.fits[[i]]))
stopCluster(CL)

IVs_Selected_by_BE <- lapply(seq_along(BE.fits), 
                             \(i) names(coef(BE.fits[[i]])[-1]))



### Count up how many Variables Selected match  the true 
### structural equation variables for that dataset in order
### to measure BE's performance.
Total_Positives <- lapply(True_Regressors, function(i) { length(i) })

system.time(BE_TPs <- lapply(seq_along(datasets), \(i)
                                    sum(IVs_Selected_by_BE[[i]] %in% 
                                          True_Regressors[[i]]))) 

system.time(BE_TPs <- lapply(seq_along(datasets), \(i)
                                    length(intersect(IVs_Selected_by_BE[[i]], 
                                                     True_Regressors[[i]])) ))

BM2_TPRs = lapply(seq_along(datasets), \(j)
                  j <- (BE_TPs[[j]]/Total_Positives[[j]]) )

BM2_FPRs = lapply(seq_along(datasets), \(i)
                  i <- length(IVs_Selected_by_BE[[i]]) - True_Positives[[i]])

# write one or more lines of code which determine whether each selected 
# model is "Underspecified", "Correctly Specified", or "Overspecified".





### Benchmark 3: Run a Forward Selection Stepwise Regression
### function on each of the 20 csvs.
CL <- makeCluster(detectCores() - 1L)
clusterExport(CL, c('datasets'))
set.seed(11)      # for reproducibility
FS.fits <- parLapply(CL, datasets, \(X) {
  nulls <- lm(X$Y ~ 1, X)
  full_models <- lm(X$Y ~ ., X)
  forward <- step(object = nulls, scope = formula(full_models), 
                  direction = 'forward', trace = FALSE) })

FS_Coeffs <- lapply(seq_along(FS.fits), function(i) coef(FS.fits[[i]]))

Models_Selected_by_FS <- lapply(seq_along(FS.fits), 
                             \(i) names(coef(FS.fits[[i]])))
# without the Intercepts included
IVs_Selected_by_FS <- lapply(seq_along(Models_Selected_by_FS), 
                             \(i) Models_Selected_by_FS[[i]][-1])
stopCluster(CL)



system.time(FS_TPs <- lapply(seq_along(datasets), \(i)
                             sum(IVs_Selected_by_FS[[i]] %in% 
                                   True_Regressors[[i]]))) 

BM3_TPRs = lapply(seq_along(datasets), \(j)
                  j <- (FS_TPs[[j]]/Total_Positives[[j]]) )

BM3_FPRs = lapply(seq_along(datasets), \(i)
                  i <- length(IVs_Selected_by_FS[[i]]) - True_Positives[[i]])

# write one or more lines of code which determine whether each selected 
# model is "Underspecified", "Correctly Specified", or "Overspecified".




### Again, the final format of the results should look
### like the following:  "#-#-#-#, X#, X#, X#, X#, etc. 
# Important note! BE can wing up with a different number 
# of X#s to select.
## print out the output formatted the way Dr. Davies asked for!
#BM3_results_2cols <- data.frame(DS_name = DS_names_list, 
#                                IVs_Selected = sapply(IVs_Selected_by_FS, 
#                                                      toString))

#BM3_results_1col <- paste(BM3_results_2cols$DS_name, ";", 
#                          BM3_results_2cols$IVs_Selected)

#FS_models_1col <- as.data.frame(BM3_results_1col, header = FALSE)
#BM3_models <- read.csv("IVs_Selected_by_FS(20) 1 column.csv", header = FALSE)




n_df <- do.call(
  rbind.data.frame,
  lapply(
    strsplit(BM3_models_1col$BM3_models_1col, ";"),
    function(x) {
      s <- strsplit(x, "-")
      c(s[[1]], s[[2]])
    } 
  )
) |> setNames(
  c("n1", "n2", "n3", "n4", "IV")
)
