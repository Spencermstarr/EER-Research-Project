getwd()
setwd("~/DAEN_698/MCS_BMs 2 & 3/Forward Selection")
getwd()


BM3_models <- read.csv("(sorted) IVs_Selected_by_FS (1 column, no header).csv", header = FALSE)
head(BM3_models, n = 4)
tail(BM3_models, n = 4)
str(BM3_models)

n_df <- do.call(
  rbind.data.frame,
  lapply(
    strsplit(BM3_models$V1, ";"),
    function(x) {
      s <- strsplit(x, "-")
      c(s[[1]], s[[2]])
    } 
  )
) |> setNames(
  c("n1", "n2", "n3", "n4", "IV")
)





setwd("~/DAEN_698/MCS_BMs 2 & 3/txt files (from Notepad) & csv files")
getwd()



sub_3_df <- subset(n_df, n2 == "3")
write.csv(sub_3_df, file = "Selections_on_3_IV_datasets.csv", row.names = FALSE)

sub_4_df <- subset(n_df, n2 == "4")
write.csv(sub_4_df, file = "Selections_on_4_IV_datasets.csv", row.names = FALSE)

sub_5_df <- subset(n_df, n2 == "5")
write.csv(sub_5_df, file = "Selections_on_5_IV_datasets.csv", row.names = FALSE)

sub_6_df <- subset(n_df, n2 == "6")
write.csv(sub_6_df, file = "Selections_on_6_IV_datasets.csv", row.names = FALSE)

sub_7_df <- subset(n_df, n2 == "7")
write.csv(sub_7_df, file = "Selections_on_7_IV_datasets.csv", row.names = FALSE)

sub_8_df <- subset(n_df, n2 == "8")
write.csv(sub_8_df, file = "Selections_on_8_IV_datasets.csv", row.names = FALSE)

sub_9_df <- subset(n_df, n2 == "9")
write.csv(sub_9_df, file = "Selections_on_9_IV_datasets.csv", row.names = FALSE)

sub_10_df <- subset(n_df, n2 == "10")
write.csv(sub_10_df, file = "Selections_on_10_IV_datasets.csv", row.names = FALSE)

sub_11_df <- subset(n_df, n2 == "11")
write.csv(sub_11_df, file = "Selections_on_11_IV_datasets.csv", row.names = FALSE)

sub_12_df <- subset(n_df, n2 == "12")
write.csv(sub_12_df, file = "Selections_on_12_IV_datasets.csv", row.names = FALSE)

sub_13_df <- subset(n_df, n2 == "13")
write.csv(sub_13_df, file = "Selections_on_13_IV_datasets.csv", row.names = FALSE)

sub_14_df <- subset(n_df, n2 == "14")
write.csv(sub_14_df, file = "Selections_on_14_IV_datasets.csv", row.names = FALSE)

sub_15_df <- subset(n_df, n2 == "15")
write.csv(sub_15_df, file = "Selections_on_15_IV_datasets.csv", row.names = FALSE)



# count, then sum up all of the correctly specified regression 
# equations selected by FS for the 4,500 datasets with 3-Factor models
Three_Factor_Model <- " X1, X2, X3"    # the definition of our 3-Factor model
Three_IV <- sub_3_df$IV
Three_Factor_Model <- Three_IV$    # the definition of our 3-Factor model
lapply(2:length(Three_Factor_Model), combn, x = names(Three_Factor_Model))
CSM3_Option1 <- sum(sub_3_df$IV == "  X1, X2, X3")




CSM4_Option1 <- sum(sub_4_df$IV == "  X1, X2, X3, X4")




CSM5 <- sum(sub_5_df$IV == "  X1, X2, X3, X4, X5")


Total_correctly_specified_models_for_the_5_factor_case



CSM6 <- sum(sub_6_df$IV == "  X1, X2, X3, X4, X5, X6")


Total_correctly_specified_models_for_the_6_factor_case



CSM7 <- sum(sub_7_df$IV == "  X1, X2, X3, X4, X5, X6, X7")

Total_correctly_specified_models_for_the_7_factor_case




CSM8 <- sum(sub_8_df$IV == "  X1, X2, X3, X4, X5, X6, X7, X8")

Total_correctly_specified_models_for_the_8_factor_case



CSM9 <- sum(sub_9_df$IV == "  X1, X2, X3, X4, X5, X6, X7, X8, X9")

Total_correctly_specified_models_for_the_9_factor_case




CSM10 <- sum(sub_10_df$IV == "  X1, X2, X3, X4, X5, X6, X7, X8, X9, X10")

Total_correctly_specified_models_for_the_10_factor_case




CSM11 <- sum(sub_11_df$IV == "  X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, 
    X11")

Total_correctly_specified_models_for_the_11_factor_case




CSM12 <- sum(sub_12_df$IV == "  X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, 
    X11, X12")

Total_correctly_specified_models_for_the_12_factor_case




CSM13 <- sum(sub_13_df$IV == "  X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, 
    X11, X12, X13")

Total_correctly_specified_models_for_the_13_factor_case




CSM14 <- sum(sub_14_df$IV == "  X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, 
    X11, X12, X13, X14")


Total_correctly_specified_models_for_the_14_factor_case




CSM15 <- sum(sub_15_df$IV == "  X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, 
    X11, X12, X13, X14, X15")


Total_correctly_specified_models_for_the_15_factor_case



Correct_Models_Selected_by_BM3 <- sum(CSM3, CSM4, CSM4, CSM5, CSM6, CSM7, 
                                      CSM8, CSM9, CSM10, CSM11, CSM12, 
                                      CSM13, CSM14, CSM15)
Correct_Models_Selected_by_BM3
