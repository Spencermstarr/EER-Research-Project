
getwd()
setwd("~/DAEN_698/MCS_BM1")
setwd("~/GMU folders (local)/DAEN_698/MCS_BM1")
getwd()


BM1_models <- read.csv("IVs_Selected_by_LASSO.csv", header = FALSE)
head(BM1_models, n = 4)
tail(BM1_models, n = 4)
str(BM1_models)


T_IVs <- do.call(
  rbind.data.frame,
  lapply( True_Regressors,
    function(x) {s <- strsplit(x, " ")} ))

Selected_IVs <- do.call(
  rbind.data.frame,
  lapply( IVs_Selected_by_LASSO$coefficients,
          function(x) {s <- strsplit(x, " ")} ))


### Count up how many T_IVs match Selected_IVs in order
### to measure LASSO's performance.
True_Positives1 <- sum(names(True_IVs) %in% IVs_Selected_by_LASSO$coefficients)
Total_Positives1 <- length(True_Regressors1)


True_Positives2 <- length(intersect(IVs_Selected_by_LASSO$coefficients, 
                                   True_Regressors))
Total_Positives2 <- length(True_Regressors)

TPR = True_Positives/Total_Positives
round(TPR, 4)




