# This script required the user to hit Run either by the button
# in the top right corner of this panel, or by hitting Ctrl+Enter

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
library_list <- c(library(plyr),library(dplyr),library(tidyverse),
                  library(readr),library(tibble),library(stringi),
                  library(janitor),library(purrr),library(stats),
                  library(leaps),library(lars),library(elasticnet),
                  library(vroom),library(parallel))



all_data <- vroom(list.files(pattern = 'csv'), id = 'source_file')

# these 2 lines together create a simple character list of 
# all the file names in the file folder of datasets you created
folderpath <- "C:/Users/Spencer/Documents/EER Project/Data/ten"
filepaths_list <- list.files(path = folderpath, full.names = TRUE, 
                             recursive = TRUE)
filepaths_list

# reformat the names of each of the csv file formatted dataset
DS_names_list <- basename(filepaths_list)
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
my_order
DS_names_list[my_order]
DS_names_list = DS_names_list[my_order]

filepaths_list[my_order]
filepaths_list = filepaths_list[my_order]
filepaths_list



# The code below reads the data into the RStudio Workspace from
# each of the 58.5k datasets in an iterative manner in such a way 
# that it assigns each of them to the corresponding name of that 
# dataset in the file folder they are stored in.
datasets <- lapply(filepaths_list, read.csv)
class(datasets)   # "list"
str(datasets)

df<- read.csv("0-3-1-3.csv", header = FALSE)
colnames(df) <- c("Y", "X1", "X2", "X3", "X4", "X5", "X6", "X7", 
                  "X8", "X9", "X10", "X11", "X12", "X13", "X14", 
                  "X15", "X16", "X17", "X18", "X19", "X20", "X21", 
                  "X22", "X23", "X24", "X25", "X26", "X27", "X28", 
                  "X29", "X30")
df_Structural_IVs <- df[1, -1]
df2 <- read.csv("0-3-1-23.csv", header = FALSE)
All_Headers <- df[3, ]
IV_headers <- df[3, -1]


# change column names of all the columns in the dataframe 'csvs'
datasets <- lapply(datasets, function(dataset_i) { 
  colnames(dataset_i) <- c("Y", "X1", "X2", "X3", "X4", "X5", "X6", "X7", 
                           "X8", "X9", "X10", "X11", "X12", "X13", "X14", 
                           "X15", "X16", "X17", "X18", "X19", "X20", "X21", 
                           "X22", "X23", "X24", "X25", "X26", "X27", "X28", 
                           "X29", "X30")
  dataset_i })


Structural_IVs_chr <- lapply(datasets, function(j) {j[1, -1]})
Structural_IVs_num <- lapply(Structural_IVs_chr, \(X) { lapply(X, as.numeric) })

True_IVs <- lapply(datasets, function(j) {j[1:3, -1]})
True_IVs <- lapply(True_IVs, function(j) {j[-2, ]})


True_Regressors <- lapply(Structural_IVs_chr, function(i) {
  names(i)[i == 1] })
True_Regressors


datasets <- lapply(datasets, function(i) {i[-1:-3, ]})
datasets <- lapply(datasets, \(X) { lapply(X, as.numeric) })
datasets <- lapply(datasets, function(i) { as.data.frame(i) })
datasets <- lapply(datasets, \(X) { round(X, 3) })




# This function fits all 20 LASSO regressions for/on
# each of the corresponding 20 datasets stored in the object
# of that name, then outputs standard regression results which 
# are typically called returned for any regression ran using R
set.seed(11)     # to ensure replicability
LASSO_fits <- lapply(datasets, function(i) 
               enet(x = as.matrix(select(i, starts_with("X"))), 
                    y = i$Y, lambda = 0, normalize = FALSE))

# This stores and prints out all of the regression 
# equation specifications selected by LASSO when called
set.seed(11)     # to ensure replicability
LASSO_Coeffs <- lapply(LASSO_fits, 
                       function(i) predict(i, x = as.matrix(select(i, starts_with("X"))), 
                                           s = 0.1, mode = "fraction", 
                                           type = "coefficients")[["coefficients"]])


### Write my own custom function which will separate out and return a 
### new list containing just the Independent Variables/Factors/Predictors
### which are 'selected' or chosen for each individual dataset. 
Positive_Coeffs <- lapply(LASSO_Coeffs, function(i) i[i > 0])

IVs_Selected_by_LASSO <- lapply(LASSO_Coeffs, function(i) names(i[i > 0]))
IVs_Selected_by_LASSO

