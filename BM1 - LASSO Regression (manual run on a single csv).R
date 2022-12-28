# This script required the user to hit Run either by the button
# in the top right corner of this panel, or by hitting Ctrl+Enter

getwd()
setwd("~/GMU folders (local)/DAEN_698/MCS_BM1")

# load all necessary packages 1 by 1 instead
library(dplyr)
library(tidyverse)
library(readr)
library(tibble)
library(stringi)
library(purrr)
library(stats)
library(leaps)
library(lars)
library(elasticnet)



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



Y = df$Y
head(Y)
Y_obs <- Y[-1]
Y_obs <- Y_obs[-1:-2]
head(Y_obs)
Y_obs <- as.numeric(Y_obs)
head(Y_obs)

df$Y = NULL

IV_headers <- df[3, ]
True_IVs <- df[-2:-3,]
head(True_IVs)

sample_obs <- df[-1:-3,]

sample_obs <- lapply(sample_obs, as.numeric)
sample_obs <- as.data.frame(sample_obs)

Xs_matrix <- as.matrix(sample_obs)


# This function fits the LASSO regression
set.seed(11)     # to ensure replicability
LASSO <- enet(x = Xs_matrix, y = Y_obs, lambda = 0, normalize = FALSE)
LASSO <- enet(x = as.matrix(sample_obs), y = Y_obs, 
              lambda = 0, normalize = FALSE)


# This stores and prints out all of the regression 
# equation specifications selected by LASSO when called
set.seed(11)     # to ensure replicability
LASSO_preds <- predict(LASSO, x = Xs_matrix, s = 0.1, mode = "fraction", 
                        type = "coefficients")

LASSO_Coeffs <- LASSO_preds["coefficients"]



Positive_Coeffs <- lapply(LASSO_Coeffs, function(i) i[i > 0])

IVs_Selected_by_LASSO <- lapply(LASSO_Coeffs, function(i) names(i[i > 0]))
IVs_Selected_by_LASSO

True_IVs <- True_IVs[1,]
True_IVs




