# Spencer Marlen-Starr's RStudio script of functions, methods, code, and 
# code comments required for running Benchmark #2, namely the 
# Backward Elimination version of Stepwise Regression
# on some of our random synthetic observations to see how well it 
# does so we can compare its results with Dr. Davies EER procedure.


# find out which working directory R has defaulted to
getwd()

# copy+paste whatever the path is for your file folder with all the 
# random synthetic datasets you have created inside a set of quotation marks
# inside of the setwd() function below and then press Run (or hit Ctrl+Enter)
setwd()
# Or, click the Session option in the Ribbon, and in the
# Set Working Directory's drop down list, select Choose Directory
# in order to set it manually.


# load all necessary packages using only 1 command/line
library_list <- c(library(stats),library(dplyr),library(tidyverse),
                  library(leaps),library(lars),library(tibble),
                  library(readr),library(stringi),library(purrr),
                  library(vroom))



# Extract all of the individual spreadsheet containing workbooks
# in the file folder called 'last 20' which is filled
# random synthetic observations to run Lasso, Stepwise, and eventually EER
# on to compare the results. There are 40 random spreadsheets in this folder
all_data <- vroom(list.files(pattern = 'csv'), id = 'source_file')

filepath <- "C:/Users/Spencer/Documents/EER Project/Data/last 40"
filepaths_list <- list.files(path = filepath, full.names = TRUE, 
                             recursive = TRUE)
length(filepaths_list)
str(filepaths_list)

# reformat the names of each of the csv file formatted datasets
DS_names_list <- basename(filepaths_list)
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
DS_names_list[my_order]
DS_names_list = DS_names_list[my_order]
DS_names_list

filepaths_list[my_order]
filepaths_list = filepaths_list[my_order]
filepaths_list


# Import all 20 of the individual csv files, each of which
# containing 500 rows by 31 columns worth of randomly generated 
# (synthetic) observations below.
# In order to accomplish this, I will be using the readr library in R.
## This line reads all of the data in each of the csv files 
## using the name of each store in the list we just created.
datasets <- lapply(filepaths_list, read.csv, header = FALSE)

df<- read.csv("0-11-3-462.csv", header = FALSE)
All_Headers <- df[3, ]
IV_headers <- df[3, -1]


# change column names of all the columns in the dataframe 'datasets'
datasets <- lapply(datasets, function(i) { 
   colnames(i) <- c("Y", "X1","X2", "X3", "X4","X5", "X6", "X7", "X8", "X9", 
                   "X10","X11", "X12", "X13","X14", "X15", "X16","X17",
                   "X18", "X19","X20", "X21", "X22","X23", "X24", "X25", 
                   "X26", "X27", "X28","X29", "X30") })
                                                       
Structural_IVs <- lapply(datasets, function(j) {j[1, -1]})
True_IVs <- lapply(datasets, function(j) {j[1:3, -1]})
True_IVs <- lapply(True_IVs, function(j) {j[-2, ]})

Structural_IVs <- lapply(True_IVs, function(j) {
  j[[1]][[1]][1] <- as.numeric(j[[1]][[1]][1]) * j[[1]][[1]][2] })

  
datasets <- lapply(datasets, function(i) {i[-1:-3, ]})
datasets <- lapply(datasets, \(X) { lapply(X, as.numeric) })
datasets <- lapply(datasets, function(i) { as.data.frame(i) })
datasets <- lapply(datasets, \(X) { round(X, 3) })

obs_on_IVs <- lapply(datasets, function(i) {i[-1:-2, ]})
obs_on_IVs <- lapply(obs_on_IVs, \(X) { lapply(X, as.numeric) })
obs_on_IVs <- lapply(obs_on_IVs, function(i) { as.data.frame(i) })
obs_on_IVs <- lapply(obs_on_IVs, \(X) { round(X, 3) })



### Benchmark 2: Run a Backward Elimination Stepwise Regression
### function on each of the  datasets.
### Assign the full models to their corresponding datasets and
### store these in the object "all_regressors_models"
library(parallel)
CL <- makeCluster(detectCores() - 1L)
clusterExport(CL, c('datasets'))

set.seed(11)      # for reproducibility
system.time(BE.fits <- parLapply(CL, datasets, \(X) {
    full_models <- lm(X$V1 ~ ., X)
    back <- step(full_models, scope = formula(full_models), 
                    direction = 'back', trace = FALSE) }) )

BE_Coeffs <- lapply(seq_along(BE.fits), function(i) coef(BE.fits[[i]]))
stopCluster(CL)

IVs_Selected_by_BE <- lapply(seq_along(BE.fits), 
                             \(i) names(coef(BE.fits[[i]])[-1]))


### Again, the final format of the results should look
### like the following:  "#-#-#-#, X#, X#, X#, X#, etc. 
# Important note! BE can wing up with a different number 
# of X#s to select.
## print out the output formatted the way Dr. Davies asked for!
BM2_models_2cols <- data.frame(DS_name = DS_names_list, 
                               IVs_selected = sapply(IVs_Selected_by_BE, toString))

BM2_models_1col <- paste(BM2_models_2cols$DS_name, ";", 
                         BM2_models_2cols$IVs_Selected)

write.csv(data.frame(DS_name = DS_names_list, 
                     IVs_selected = sapply(IVs_Selected_by_BE, toString)), 
          file = "IVs_Selected_by_BE(20).csv", row.names = FALSE)



# change column names of all the columns in the dataframe 'structural_IVs'
lapply(Structural_IVs, function(i) { 
  colnames(i) <- c("X1","X2", "X3", "X4","X5", "X6", "X7", "X8", "X9", 
                   "X10","X11", "X12", "X13","X14", "X15", "X16","X17",
                   "X18", "X19","X20", "X21", "X22","X23", "X24", "X25", 
                   "X26", "X27", "X28","X29", "X30") })

True_Regressors <- lapply(Structural_IVs, function(i) {
                          names(i)[i == 1] })
True_Regressors


### Count up how many Variables Selected match  the true 
### structural equation variables for that dataset in order
### to measure BE's performance.
True_Pos_list <- lapply(datasets, function(i) {sum(names(Structural_IVs) %in% 
                              IVs_Selected_by_BE$coefficients) })
Total_Pos_list <- length(True_Regressors)

True_Pos_list <- lapply(seq_along(datasets), \(i)
                        length(intersect(IVs_Selected_by_BE, 
                                         True_Regressors)) )






### Benchmark 3: Run a Forward Selection Stepwise Regression
### function on each of the 20 datasets.
CL <- makeCluster(detectCores() - 1L)
clusterExport(CL, c('datasets'))
set.seed(11)      # for reproducibility
FS.fits <- parLapply(CL, datasets, \(X) {
  nulls <- lm(X$V1 ~ 1, X)
  full_models <- lm(X$V1 ~ ., X)
  forward <- step(object = nulls, scope = formula(full_models), 
                  direction = 'forward', trace = FALSE) })

FS_Coeffs <- lapply(seq_along(FS.fits), function(i) coef(FS.fits[[i]]))

Models_Selected_by_FS <- lapply(seq_along(FS.fits), 
                             \(i) names(coef(FS.fits[[i]])))
# without the Intercepts included
IVs_Selected_by_FS <- lapply(seq_along(Models_Selected_by_FS), 
                             \(i) Models_Selected_by_FS[[i]][-1])
stopCluster(CL)




### Again, the final format of the results should look
### like the following:  "#-#-#-#, X#, X#, X#, X#, etc. 
# Important note! BE can wing up with a different number 
# of X#s to select.
## print out the output formatted the way Dr. Davies asked for!
BM3_models_2cols <- data.frame(DS_name = DS_names_list, 
                          IVs_Selected = sapply(IVs_Selected_by_FS, 
                                                      toString))
write.csv(data.frame(DS_name = DS_names_list, 
                     Variables_selected = sapply(IVs_Selected_by_FS, 
                                                 toString)), 
          file = "IVs_Selected_by_FS(20).csv", row.names = FALSE)

BM3_models_1col <- paste(BM3_models_2cols$DS_name, ";", 
                         BM3_models_2cols$IVs_Selected)
BM3_models_1col <- as.data.frame(BM3_models_1col, header = FALSE)
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
