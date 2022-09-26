# This script required the user to hit Run either by the button
# in the top right corner of this panel, or by hitting Ctrl+Enter

getwd()
setwd("~/DAEN_698/MCS_BM3")
getwd()



# load all necessary packages using only 1 command/line
library_list1 <- c(library(stats),library(dplyr),library(tidyverse),
                  library(readr),library(stringi))

library_list2 <- c(library(stats), library(leaps),
                   library(lars), library(elasticnet))

# load all necessary packages 1 by 1 instead
library(dplyr)
library(tidyverse)
library(readr)
library(stringi)
library(purrr)

library(stats)
library(leaps)
library(lars)
library(elasticnet)



# these 2 lines together create a simple character list of 
# all the file names in the file folder of datasets you created
directory_path <- "~/DAEN_698/other datasets/sample obs"
filepath_list <- list.files(path = directory_path, full.names = TRUE, 
                        recursive = TRUE)
length(filepath_list)
str(filepath_list)




# The code below reads the data into the RStudio Workspace from
# each of the 47k datasets in an iterative manner in such a way 
# that it assigns each of them to the corresponding name of that 
# dataset in the file folder they are stored in.
datasets <- lapply(list.files(path = "~/DAEN_698/other datasets/sample obs", 
                              full.names = TRUE, recursive = TRUE), read.csv)


# reformat the names of each of the csv file formatted dataset
DS_names_list <- basename(filepath_list)
DS_names_list <- tools::file_path_sans_ext(DS_names_list)
head(DS_names_list, n = 4)
str(DS_names_list)



# This function fits all 47,500 Elastic_Net regressions for/on
# each of the corresponding 47.5k datasets stored in the object
# of that name, then outputs standard regression results which 
# are typically called returned for any regression ran using R
set.seed(11)     # to ensure replicability
Elastic_Net_fits <- lapply(datasets, function(i) 
               enet(x = as.matrix(select(i, starts_with("X"))), 
               y = i$Y, lambda = 0.5, normalize = FALSE))

length(Elastic_Net_fits)
head(str(Elastic_Net_fits), n = 1)
head(Elastic_Net_fits, n = 1)
head(Elastic_Net_fits, n = 3)
summary(Elastic_Net_fits)
coefficients(Elastic_Net_fits[1])




# This stores and prints out all of the regression 
# equation specifications selected by Elastic_Net when called
Features_selected_by_Elastic_Net <- lapply(seq_along(BE_fits), 
                                   \(i) names(coef(BE_fits[[i]])))

set.seed(11)     # to ensure replicability
Elastic_Net_coefs <- lapply(Elastic_Net_fits, 
                       function(i) coef(Elastic_Net_fits))


set.seed(11)     # to ensure replicability
Elastic_Net_coefs <- lapply(Elastic_Net_fits, 
                       function(i) predict(i, x = as.matrix(select(i, starts_with("X"))), 
                                           s = 0.1, mode = "fraction", 
                                           type = "coefficients")[["coefficients"]])
set.seed(11)     # to ensure replicability
Elastic_Net_coefs <- lapply(Elastic_Net_fits, 
                       function(i) predict(i, x = as.matrix(select(i, starts_with("X"))), 
                                           s = 0.1, mode = "fraction", 
                                           type = "coefficients")[["coefficients"]])

Elastic_Net_coefs[[1]]
all_coefs_estimated_by_1st_Elastic_Net <- Elastic_Net_coefs[[1]]
all_coefs_estimated_by_1st_Elastic_Net

Elastic_Net_coefs[[1]][["X1"]]

Elastic_Net_coefs[1]["intercept"]
Elastic_Net_coefs[[1]]["intercept"]
Elastic_Net_coefs[1][["intercept"]]
Elastic_Net_coefs[[1]][["Intercept"]]

### Write my own custom function which will separate out and return a 
### new list containing just the Independent Variables/Factors/Predictors
### which are 'selected' or chosen for each individual dataset (all 
### of which contain 15,000 randomly generated synthetic observations)
### by the unique Elastic_Net Regression ran on it, i.e. the simple linear
### regression specification which Elastic_Net can find that best fits the 
### the synthetic observations within that dataset.
positive_coefs <- lapply(Elastic_Net_coefs, function(i) i[i > 0])
positive_coefs[1]
positive_coefs[[1]]


Features_selected_by_Elastic_Net <- lapply(seq_along(BE_fits), 
                                   \(i) names(coef(BE_fits[[i]])))

IVs_selected_by_Elastic_Net <- lapply(Elastic_Net_coefs, function(i) names(i[i > 0]))
IVs_selected_by_Elastic_Net[[1]]
head(IVs_selected_by_Elastic_Net, n = 4)
class(IVs_selected_by_Elastic_Net)
str = str(IVs_selected_by_Elastic_Net)
str(IVs_selected_by_Elastic_Net[1])
str(IVs_selected_by_Elastic_Net[2])
rownames(IVs_selected_by_Elastic_Net)    # output: NULL
colnames(IVs_selected_by_Elastic_Net)    # output: NULL


## Write all the Factors/Predictors the 
## 47,500 Elastic_Net Regressions to a text file.
getwd()
setwd("~/DAEN_698/MCS_BM13")
setwd("~/DAEN_698")
getwd()

write.csv(data.frame(DS_name = DS_names_list, 
                     IVs_selected = sapply(IVs_selected_by_Elastic_Net, 
                                           toString)), 
          file = "Benchmark1_results.csv", row.names = FALSE)


