# Spencer Marlen-Starr's RStudio script of functions, methods, code, and 
# code comments required for running Benchmarks #2 & 3, namely the 
# Backward Elimination & Forward Selection versions of Stepwise Regression
# on some of our random synthetic observations to see how well it 
# does so we can compare its results with Dr. Davies' EER procedure.


# find out which working directory R has defaulted to
getwd()
setwd("~/GMU folders (local)/DAEN_698/MCS_BMs 2 & 3")
# Or, click the Session option in the Ribbon, and in the
# Set Working Directory's dropdown list, select Choose Directory
# in order to set it manually.


### Benchmarks 2 and 3


# load all necessary packages using only 1 command/line
library_list <- c(library(stats),library(dplyr),library(tidyverse),
                  library(leaps),library(lars),library(tibble),
                  library(readr),library(stringi),library(purrr))
# load all necessary packages
library(stats)
library(plyr)
library(dplyr)
library(tidyverse)
library(tibble)
library(readr)
library(leaps)
library(lars)
library(stringi)
library(purrr)



# these 2 lines together create a simple character list of 
# all the file names in the file folder of datasets you created
setwd("~/EER Project/Data/last 40")
df<- read.csv("0-11-3-462.csv", header = FALSE)
str(df)
# change column names of all the columns in the dataframe 'df'
colnames(df) <- c("Y", "X1","X2", "X3", "X4","X5", "X6", "X7","X8", "X9",
                  "X10","X11", "X12", "X13","X14", "X15", "X16","X17", 
                  "X18", "X19","X20", "X21", "X22","X23", "X24", "X25",
                  "X26", "X27", "X28","X29", "X30")
data <- df

set.seed(11)      # for reproducibility
full_model <- lm(formula = Y ~ ., data = All_sample_obs)
BE.fit <- step(full_model, direction = "backward", trace = 0)

BE_Coeffs <- coef(BE.fit)

Models_Selected_by_BE <- names(BE_Coeffs)
IVs_Selected_by_BE <- names(BE_Coeffs[-1])




### Run a Forward Selection Stepwise Regression
set.seed(11)      # for reproducibility
null_model <- lm(formula = Y ~ 1, data = All_sample_obs)
null_model <- lm(formula = Y ~ X1, data = All_sample_obs)
set.seed(11)      # for reproducibility
FS.fit <- step(object = null_model, scope = formula(full_model), 
               direction = "forward", trace = 0, data = All_sample_obs)


FS_Coeffs <- coef(FS.fit)

Models_Selected_by_FS <- names(FS_Coeffs)
IVs_Selected_by_FS <- names(FS_Coeffs[-1])





Stepwise.fit <- step(null_model, scope = formula(full_model), 
                     direction = "both", trace = 0, data = All_sample_obs)

