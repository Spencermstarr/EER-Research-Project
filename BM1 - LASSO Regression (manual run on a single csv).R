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

data <- df
# change column names of all the columns in the dataframe data
#headers_all 
colnames(data) <- c("Y", "X1","X2", "X3", "X4","X5", "X6", "X7","X8", "X9",
                    "X1","X2", "X3")


Y = df$V1
head(Y)
Y_obs <- Y[-1]
Y_obs <- Y_obs[-1]
head(Y_obs)
Y_obs <- as.numeric(Y_obs)
head(Y_obs)



df$V1 = NULL

True_IVs <- df[-2,]

sample_obs <- df[-2,]
sample_obs <- sample_obs[-1, ]
headers <- sample_obs[1, ]
sample_obs <- sample_obs[-1, ]
sample_obs <- lapply(sample_obs, as.numeric)
sample_obs <- as.data.frame(sample_obs)

Xs_matrix <- as.matrix(Xs_df)
Xs_matrix <- as.numeric(Xs_matrix)


# This function fits the LASSO regression
set.seed(11)     # to ensure replicability
LASSO <- enet(x = Xs_matrix, y = Y_obs, lambda = 0, normalize = FALSE)
LASSO <- enet(x = as.matrix(sample_obs), y = Y_obs, 
              lambda = 0, normalize = FALSE)

# This stores and prints out all of the regression 
# equation specifications selected by LASSO when called
set.seed(11)     # to ensure replicability
LASSO_preds <- LASSO(type = "coefficients")
LASSO_preds <- predict(LASSO, x = Xs_matrix, s = 0.1, mode = "fraction", 
                        type = "coefficients")

LASSO_preds <- coef(LASSO)

LASSO_Coeffs <- LASSO_preds$coefficients
LASSO_Coeffs2 <- LASSO_preds["coefficients"]


IVs_Selected_by_LASSO <- names(LASSO_Coeffs)


Positive_Coeffs <- when(LASSO_Coeffs$coefficients > 0, LASSO)
Positive_Coeffs


Positive_Coeffs <- lapply(LASSO_Coeffs, function(i) i[i > 0])

IVs_Selected_by_LASSO <- lapply(LASSO_Coeffs, names(i[i > 0]))




## Write all the Factors/Predictors the 
## 58,500 LASSO Regressions to a text file.
getwd()
setwd("~/DAEN_698/MCS_BM1")
setwd("~/DAEN_698/MCS_BM1/text & csv files")
getwd()

write.csv(data.frame(DS_name = DS_names_list, 
                     Coeff_Estimates = sapply(Positive_Coeffs, toString)), 
          file = "LASSO_Estimates.csv", row.names = FALSE)


BM1_models_2cols <- data.frame(DS_name = DS_names_list, 
                    IVs_Selected = sapply(IVs_Selected_by_LASSO, toString))
str(BM1_models_2cols)

BM1_models_1col <- paste(BM1_models_2cols$DS_name, ";  ", 
                         BM1_models_2cols$IVs_Selected)
BM1_IVs <- as.data.frame(BM1_models_1col)


n_BM1 <- do.call(rbind.data.frame, lapply(
  strsplit(BM1_IVs$BM1_models_1col, ";  "),
  function(x) {
    s <- strsplit(x, "-")
    c(s[[1]], s[[2]])})) |> setNames(
      c("Multicollinearity", "Number of True Regressors", 
        "Error Variance", "Random Dataset Generated #", 
        "Regresors Selected by LASSO") )


write.csv(data.frame(DS_name = DS_names_list, 
                     IVs_selected = sapply(IVs_Selected_by_LASSO, toString)), 
          file = "IVs_Selected_by_LASSO.csv", row.names = FALSE)



