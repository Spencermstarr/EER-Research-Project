# This script required the user to hit Run either by the button
# in the top right corner of this panel, or by hitting Ctrl+Enter

# find out which working directory R has defaulted to
#rm(list = ls())
#setwd("D:/EER")
#setwd("D:/EER folder")
#setwd("C:/Users/Spencer/OneDrive/Documents/Analytics Projects/EER/1st Benchmark")
#getwd()

# copy+paste whatever the path is for your file folder with all the 
# random synthetic csvs you have created inside a set of quotation marks
# inside of the setwd() function below and then press Run (or hit Ctrl+Enter)
#setwd()
# Or, click the Session option in the Ribbon, and in the
# Set Working Directory's drop down list, select Choose Directory
# in order to set it manually.

# load all necessary packages using only 1 command/line
library_list <- c(library(plyr),library(dplyr),library(stringi),
                  library(purrr),library(stats),library(glmnet),
                  library(data.table),library(parallel))
                  


# This function fits all n LASSO regressions for/on
# each of the corresponding n datasets stored in the object
# of that name, then outputs standard regression results which 
# are typically called returned for any regression ran using R
#CL3 <- makeCluster(detectCores() - 1L)
#clusterExport(CL3, c('datasets'))
set.seed(11)     # to ensure replicability
system.time(L.fits <- lapply(X = datasets, function(i) 
               glmnet(x = as.matrix(select(i, starts_with("X"))), 
                      y = i$Y, alpha = 1)))


# This stores and prints out all of the regression 
# equation specifications selected by LASSO when called
L.coefs = L.fits |> 
  Map(f = \(model) coef(model, s = .1))

Variables.Selected <- L.coefs |>
  Map(f = \(matr) matr |> as.matrix() |> 
       as.data.frame() |> filter(s1 != 0) |> rownames())

Variables.Selected = lapply(seq_along(datasets), \(j)
                                         j <- (Variables.Selected[[j]][-1]))

Variables.Not.Selected <- L.coefs |>
  Map(f = \(matr) matr |> as.matrix() |> 
        as.data.frame() |> filter(s1 == 0) |> rownames())

write.csv(
  data.frame(DS_name = DS_names_list, 
             Variables.Selected.by.glmnet = sapply(Variables.Selected, toString),
             Structural_Variables = sapply(Structural_Variables, 
                                           toString),
             Nonstructural_Variables = sapply(Nonstructural_Variables, 
                                              toString)),
  file = "LASSO's Selections via glmnet for the DSs from '0-5-1-1 to 0-6-10-500.csv", 
  row.names = FALSE)



### Count up how many Variables Selected match  the true 
### structural equation variables for that dataset in order
### to measure LASSO's performance.
# all of the "Positives", i.e. all the Structural Regressors
BM1_NPs <- lapply(Structural_Variables, function(i) { length(i) })

# all of the "Negatives", i.e. all the Nonstructural Regressors
BM1_NNs <- lapply(Structural_Variables, function(i) {30 - length(i)})

# all the "True Positives"
BM1_TPs <- lapply(seq_along(datasets), \(i)
                  sum(Variables.Selected[[i]] %in% 
                        Structural_Variables[[i]]))
# the number True Negatives for each LASSO
BM1_TNs <- lapply(seq_along(datasets), \(k)
                   sum(Variables.Not.Selected[[k]] %in% 
                         Nonstructural_Variables[[k]]))

# the number of False Positives
BM1_FPs <- lapply(seq_along(datasets), \(i)
                  sum(Variables.Selected[[i]] %in% 
                        Nonstructural_Variables[[i]]))
# the number of False Negatives Selected by each LASSO
BM1_FNs <- lapply(seq_along(datasets), \(i)
                  sum(Variables.Not.Selected[[i]] %in% 
                        Structural_Variables[[i]]))


# the True Positive Rate
BM1_TPR = lapply(seq_along(datasets), \(j)
                  j <- (BM1_TPs[[j]]/BM1_NPs[[j]]) )

# the False Positive Rate = FP/(FP + TN)
BM1_FPR = lapply(seq_along(datasets), \(j)
                  j <- (BM1_FPs[[j]])/(BM1_FPs[[j]] + BM1_TNs[[j]]))
# or, another way to calculate it would be
BM1_FPR2 = lapply(seq_along(datasets), \(i)
                   i <- (BM1_FPs[[i]])/(BM1_NNs[[i]]))

# the True Negative Rate = TN/(FP + TN)
BM1_TNR <- lapply(seq_along(datasets), \(j) 
                   j <- (BM1_TNs[[j]]/BM1_NNs[[j]]))
BM1_TNR2 <- lapply(seq_along(datasets), \(w)
                    w <- (BM1_TNs[[w]]/(BM1_FPs[[w]] + BM1_TNs[[w]])))
# or, because TNR = 1 - FPR, we can also use
BM1_TNR3 <- lapply(BM1_FPR, \(i) 
                    i <- (1 - i))


## Write one or more lines of code which determine whether each selected 
## model is "Underspecified", "Correctly Specified", or "Overspecified".
# True Positive Rates as a vector rather than a list
TPR <- unlist(BM1_TPR)
mean_TPR <- round(mean(TPR), 3)
# number of selected regressions with at least one omitted variable
num_OMVs <- sum(TPR < 1, na.rm = TRUE)

# False Positive Rate as a vector rather than a list
FPR <- unlist(BM1_FPR)
mean_FPR <- round(mean(FPR), 3)
num_null_FPR <- sum(FPR == 0, na.rm = TRUE)
# number of models with at least one extraneous variable selected
num_Extraneous <- sum(FPR > 0)

# True Negative Rates as a vector rather than a list
TNR <- unlist(BM1_TNR)
TNR2 <- unlist(BM1_TNR2)
mean_TNR <- round(mean(TNR), 3)
mean_TNR2 <- round(mean(TNR2), 3)

# Number of Underspecified Regression Specifications Selected by LASSO
N_Under = sum( (TPR < 1) & (FPR == 0) )

# Number of Correctly Specified Regressions Selected by LASSO
N_Correct <- sum( (TPR == 1) & (TNR == 1) )

# Overspecified Regression Specifications Selected by LASSO
N_Over = sum( (TPR == 1) & (FPR > 0) )

# sum of all the 3 specification categories
Un_Corr_Ov = N_Under + N_Correct + N_Over


Headers <- c("True Positive Rate", "True Negative Rate", 
             "False Positive Rate")
PMs1 <- data.frame(mean_TPR, mean_TNR, mean_FPR)
colnames(PMs1) <- Headers
rm(mean_TPR, mean_TNR, mean_FPR)

Headers <- c("Underspecified Models Selected", 
             "Correctly Specified Models Selected",
             "Overspecified Models Selected")
PMs2 <- data.frame(N_Under, N_Correct, N_Over)
colnames(PMs2) <- Headers
rm(N_Under, N_Correct, N_Over)

Headers <- c("All Correct, Over, and Underspecified Models", 
             "Models with at least one Omitted Variable",
             "Models with at least one Extra Variable")
PMs3 <- data.frame(Un_Corr_Ov, num_OMVs, num_Extraneous)
colnames(PMs3) <- Headers

# Or, just print out this instead of having to print out 3 different things
performance_metrics <- data.frame(PMs1, PMs2, PMs3)
performance_metrics

write.csv(performance_metrics, 
          file = "glmnet's Performance on the datasets from '0-5-1-1 to 0-6-10-500'.csv", 
          row.names = FALSE)


length(datasets)
head(DS_names_list)
tail(DS_names_list)

#rm(CL, Structural_Variables)
#rm(Variables.Selected, LASSO_fits, LASSO_Coeffs)
#rm(BM1_FPR, BM1_FPs, BM1_NNs, BM1_NPs, BM1_TNR, BM1_TPR, BM1_TPs)
#rm(FPR, TPR, TNR, Headers, num_null_FPR)

#rm(performance_metrics, PMs1, PMs2, PMs3)

