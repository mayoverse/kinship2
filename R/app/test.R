setwd("R/app")
library(devtools)
load_all()
library(shiny)
library(dplyr)
source("data_management.R")
source("data_import.R")
source("data_col_sel.R")
source("data_download.R")
source("utils.R")

runApp()

library(kinship2)

data(sample.ped)
df <- sample.ped
df$avail
df[, c("indId", "fatherId", "motherId",
    "gender", "available")] <- df[, c("id", "father", "mother",
        "sex", "avail")]
df$available
df <- norm_data(df)[[1]]
df$avail
summary(df)
df$aff <- df$avail
df[1:4, "gender"] <- NA
df[10:13, "avail"] <- NA
fam_df <- generate_aff_inds(df, "gender", threshold = 1.5, sup_thres_aff = TRUE)
fam_df$mods_aff


data(testped1)

df <- testped1
colnames(df)
cols_ren <- list(indId = "id", fatherId = "father",
    motherId = "mother", gender = "sex")
data.table::setnames(df,
                old = as.vector(unlist(cols_ren)),
                new = names(cols_ren))
colnames(df)

df_norm <- check_data(df)

col_classes <- "character"
quote = "'"
header = TRUE
sep = "/t"
read.csv("C:/Users/llenezet/Documents/EnCours/kinship2/data/testped1.tab", quote = quote,
                header = header, sep = sep, fill = TRUE,
                colClasses = col_classes, row.names = 1)
utils::read.table("C:/Users/llenezet/Documents/EnCours/kinship2/data/testped1.tab", quote = "",
                header = header)