# Spencer Marlen-Starr's RStudio script of functions, methods, code, and 
# code comments required for running Benchmarks #2 & 3, namely the 
# Backward Elimination & Forward Selection versions of Stepwise Regression
# on some of our random synthetic observations to see how well it 
# does so we can compare its results with Dr. Davies' EER procedure.


# find out which working directory R has defaulted to
getwd()
setwd("~/EER Project/Data/last 40")
setwd("~/EER Project/Scripts")
# Or, click the Session option in the Ribbon, and in the
# Set Working Directory's drop down list, select Choose Directory
# in order to set it manually.


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
True_IVs <- df[1, -1]

All_sample_obs <- df[-1:-3,]
All_sample_obs <- lapply(All_sample_obs, as.numeric)
All_sample_obs <- as.data.frame(All_sample_obs)
All_sample_obs <- round(All_sample_obs, 3)

Y = df$Y
Y_obs <- Y      #just in case I need to reset
Y_obs <- Y_obs[-1:-3]
Y_obs <- as.numeric(Y_obs)
Y_obs <- round(Y_obs, 3)

df$Y = NULL











set.seed(11)      # for reproducibility
full_model <- lm(formula = Y ~ ., data = All_sample_obs)
BE.fit <- step(full_model, direction = "backward", trace = 0)

BE_Coeffs <- coef(BE.fit)

Models_Selected_by_BE <- names(BE_Coeffs)
IVs_Selected_by_BE <- names(BE_Coeffs[-1])


True_Regressors <- names(True_IVs)[True_IVs == 1]
True_Regressors

### Count up how many T_IVs match Selected_IVs in order
### to measure FS's performance.
True_Pos_list <- sum(names(True_IVs) %in% IVs_Selected_by_FS$coefficients)
Total_Pos_list <- length(True_Regressors)

True_Positives2 <- length(intersect(IVs_Selected_by_LASSO$coefficients, 
                                    True_Regressors))
Total_Positives2 <- length(True_Regressors)

TPR = True_Positives/Total_Positives
round(TPR, 4)






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


### Count up how many T_IVs match Selected_IVs in order
### to measure FS's performance.
True_Pos_list <- sum(names(True_IVs) %in% IVs_Selected_by_FS$coefficients)
Total_Pos_list <- length(True_Regressors)







Stepwise.fit <- step(null_model, scope = formula(full_model), 
                     direction = "both", trace = 0, data = All_sample_obs)

