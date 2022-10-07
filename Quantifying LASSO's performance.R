


getwd()
setwd("~/DAEN_698/MCS_BM1")
getwd()


BM1_models <- read.csv("IVs_Selected_by_LASSO.csv", header = FALSE)
head(BM1_models, n = 4)
tail(BM1_models, n = 4)
str(BM1_models)


n_df2 <- do.call(
  rbind.data.frame,
  lapply(
    strsplit(BM1_models$V1, ";"),
    function(x) {
      s <- strsplit(x, "-")
      c(s[[1]], s[[2]])
    } 
  )
) |> setNames(
  c("n1", "n2", "n3", "n4", "IV")
)

CSM3 <- sum(sub_3_df$IV == "  X1, X2, X3")

CSM4 <- sum(sub_4_df$IV == "  X1, X2, X3, X4")
CSM5 <- sum(sub_5_df$IV == "  X1, X2, X3, X4, X5")
CSM6 <- sum(sub_6_df$IV == "  X1, X2, X3, X4, X5, X6")
CSM7 <- sum(sub_7_df$IV == "  X1, X2, X3, X4, X5, X6, X7")
CSM8 <- sum(sub_8_df$IV == "  X1, X2, X3, X4, X5, X6, X7, X8")
CSM9 <- sum(sub_9_df$IV == "  X1, X2, X3, X4, X5, X6, X7, X8, X9")
CSM10 <- sum(sub_10_df$IV == "  X1, X2, X3, X4, X5, X6, X7, X8, X9, X10")
CSM11 <- sum(sub_11_df$IV == "  X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, 
    X11")

CSM12 <- sum(sub_12_df$IV == "  X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, 
    X11, X12")

CSM13 <- sum(sub_13_df$IV == "  X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, 
    X11, X12, X13")

CSM14 <- sum(sub_14_df$IV == "  X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, 
    X11, X12, X13, X14")

CSM15 <- sum(sub_15_df$IV == "  X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, 
    X11, X12, X13, X14, X15")


Correct_Models_Selected_by_BM1 <- sum(CSM3, CSM4, CSM4, CSM5, CSM6, CSM7, 
                                      CSM8, CSM9, CSM10, CSM11, CSM12, 
                                      CSM13, CSM14, CSM15)
Correct_Models_Selected_by_BM1



setwd("~/DAEN_698/MCS_BM1/txt & csv files")
sub_3_df <- subset(n_df, n2 == "3")
head(sub_3_df)
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







correct_models <- sapply(BM1_models, count(BM1_models, ))





