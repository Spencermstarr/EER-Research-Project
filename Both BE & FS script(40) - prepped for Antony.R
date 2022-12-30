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


### Benchmark 2: 
### One of the two main versions of the classical method
### of automated optimal variable selection back before
### the modern machine learning revolution; namely,
### the 'Backward Elimination' version of Stepwise Regression.

# load all necessary packages using only 1 command/line
library_list <- c(library(stats),library(dplyr),library(tidyverse),
                  library(leaps),library(lars),library(tibble),
                  library(readr),library(stringi),library(purrr))



# Extract all of the individual spreadsheet containing workbooks
# in the file folder called 'sample obs(20 csvs)' which is filled
# random synthetic observations to run Lasso, Stepwise, and eventually EER
# on to compare the results. There are 20 random spreadsheets in this folder
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
datasets <- lapply(filepaths_list, read.csv)
df<- read.csv("0-11-3-462.csv", header = FALSE)
True_IVs <- df[1, -1]

VarNames <- c("Y", "X1","X2", "X3", "X4","X5", "X6", "X7","X8", "X9",
              "X10","X11", "X12", "X13","X14", "X15", "X16","X17", 
              "X18", "X19","X20", "X21", "X22","X23", "X24", "X25",
              "X26", "X27", "X28","X29", "X30")
# change column names of all the columns in the dataframe 'df'
lapply(datasets, function(i) {
  colnames <- c("Y", "X1","X2", "X3", "X4","X5", "X6", "X7","X8", "X9",
                  "X10","X11", "X12", "X13","X14", "X15", "X16","X17", 
                  "X18", "X19","X20", "X21", "X22","X23", "X24", "X25",
                  "X26", "X27", "X28","X29", "X30") })


All_sample_obs <- datasets[-1:-3,]
All_sample_obs <- lapply(All_sample_obs, as.numeric)
All_sample_obs <- as.data.frame(All_sample_obs)
All_sample_obs <- round(All_sample_obs, 3)

Y = df$Y
Y_obs <- Y      #just in case I need to reset
Y_obs <- Y_obs[-1:-3]
Y_obs <- as.numeric(Y_obs)
Y_obs <- round(Y_obs, 3)

df$Y = NULL
IV_headers <- datasets[3, ]







### Benchmark 2: Run a Backward Elimination Stepwise Regression
### function on each of the  datasets.
### Assign the full models to their corresponding datasets and
### store these in the object "all_regressors_models"
library(parallel)
CL <- makeCluster(detectCores() - 1L)
clusterExport(CL, c('datasets'))
set.seed(11)      # for reproducibility
BE.fits <- parLapply(CL, datasets, \(X) {
    full_models <- lm(Y ~ ., X)
    back <- step(full_models, scope = formula(full_models), 
                    dir = 'back', trace = FALSE) })
stopCluster(CL)

BE_Coeffs <- lapply(seq_along(BE.fits), function(i) coef(BE.fits[[i]]))

IVs_Selected_by_BE <- lapply(seq_along(BE.fits), 
                             \(i) names(coef(BE.fits[[i]])[-1]))


### Again, the final format of the results should look
### like the following:  "#-#-#-#, X#, X#, X#, X#, etc. 
# Important note! BE can wing up with a different number 
# of X#s to select.
## print out the output formatted the way Dr. Davies asked for!
BM2_models_2cols <- data.frame(DS_name = DS_names_list, 
                          Variables_selected = sapply(IVs_Selected_by_BE, 
                                                toString))

BM2_models_1col <- paste(BM2_models_2cols$DS_name, ";", 
                         BM2_models_2cols$IVs_Selected)

write.csv(data.frame(DS_name = DS_names_list, 
                     Variables_selected = sapply(IVs_Selected_by_BE, 
                                                 toString)), 
          file = "IVs_Selected_by_BE(20).csv", row.names = FALSE)








### Benchmark 3: Run a Forward Selection Stepwise Regression
### function on each of the 20 datasets.
CL <- makeCluster(detectCores() - 1L)
clusterExport(CL, c('datasets'))
set.seed(11)      # for reproducibility
FS.fits <- parLapply(CL, datasets, \(X) {
  nulls <- lm(Y ~ 1, X)
  full_models <- lm(Y ~ ., X)
  forward <- step(object = nulls, scope = formula(full_models), 
                  dir = 'forward', trace = FALSE) })

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
