


getwd()
setwd("~/DAEN_698/MCS_BMs 2 & 3")
getwd()


BM2_models1 <- read.csv("IVs_Selected_by_BE (no headers).csv", header = FALSE)
head(BM2_models1, n = 4)
tail(BM2_models1, n = 4)
str(BM2_models1)

n_df <- do.call(
  rbind.data.frame,
  lapply(
    strsplit(BM2_models1$V1, ";"),
    function(x) {
      s <- strsplit(x, "-")
      c(s[[1]], s[[2]])
    } 
  )
) |> setNames(
  c("n1", "n2", "n3", "n4", "IV")
)

BM2_models2 <- read.csv("IVs_Selected_by_BE (with headers).csv", header = TRUE)
head(BM2_models2, n = 4)
tail(BM2_models2, n = 4)
str(BM2_models2)

n_df <- do.call(
  rbind.data.frame,
  lapply(
    strsplit(BM2_models2$DS_name, ";"),
    function(x) {
      s <- strsplit(x, "-")
      c(s[[1]], s[[2]])
    } 
  )
) |> setNames(
  c("n1", "n2", "n3", "n4", "IV")
)


sub_3_df <- subset(n_df, n2 == "3")
head(sub_3_df)
write.csv(sub_3_df, file = "Selections_on_3_IV_datasets.csv", row.names = FALSE)


setwd("~/DAEN_698/MCS_BMs 2 & 3/txt files (from Notepad) & csv files")
getwd()


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



