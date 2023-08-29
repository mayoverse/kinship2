setwd("R/")
library(devtools)
load_all()
library(shiny)
library(dplyr)
library(ggplot2)
source("app/data_management.R")
source("app/data_import.R")
source("app/data_col_sel.R")
source("app/data_download.R")
source("app/plot_ped.R")
source("app/plot_download.R")
source("app/utils.R")
source("class/pedigree.R")
source("class/validity.R")
source("class/Pedigree_class.R")


df <- read.csv("C:/Users/llenezet/Documents/EnCours/pedigreecreation/PedigreeApp/data/TestPedigree2.csv", sep = ";")
summary(df)

ped_df <- df
object <- pedigree(ped_df)
class(object)
object$ped
object[["ped"]]
object[["ped"]][1, "id"] <- 9

object$ped[1, "id"] <- "12"

summary(object)
df <- norm_ped(df)

data(sampleped)
df <- sampleped
df$avail
df[, c("indId", "fatherId", "motherId",
    "gender", "available")] <- df[, c("id", "father", "mother",
        "sex", "avail")]
df$available
df <- norm_ped(df)[[1]]
df$avail
summary(df)
df$aff <- df$avail
df[1:4, "gender"] <- NA
df[10:13, "avail"] <- NA
fam_df <- generate_aff_inds(df, "gender", threshold = 1.5, sup_thres_aff = TRUE)

fam_df

ped <- with(fam_df, pedigree(id, dadid, momid, sex, fam_df[affected]))
ped$affected
legendPlot(ped, affected.label = "gender")
ncol(ped$affected)
ped$affected
data(testped1)
summary(testped1)
cols_ren <- c("indId" = "id", "fatherId" = "father",
    "motherId" = "mother", "gender" = "sex")
data.table::setnames(testped1,
                    old = as.vector(unlist(cols_ren)),
                    new = names(cols_ren))
df <- check_ped(testped1)
df <- generate_aff_inds(df$norm,
    col_aff = "sex",
    mods_aff = "male")
aff <- generate_colors(df, "affected")
df <- aff$df
leg <- create_legend(aff$scales)
ggarrange(leg$A, leg$B)
df <- select_from_inf(df, c(1, 2), 3)
df <- df[df$family == 1, ]
ped <- with(df, pedigree(id, dadid, momid, sex, affected))

legendPlot(ped, affected = as.data.frame(ped$affected))
nb_ind_gen <- align(ped)$n
plot_ped <- ped_plot(df, cex_plot = 0.5, mar = c(0.5, 0.5, 0.5, 0.5),
    psize = c(2, length(nb_ind_gen)),
    to_plotly = FALSE)

data(minnbreast)
df <- minnbreast
df[, c("indId", "fatherId", "motherId",
    "gender")] <- df[, c("id", "fatherid", "motherid",
        "sex")]
df$bcp <- as.numeric(df$bcpc)
df <- check_ped(df)
df <- generate_aff_inds(df$norm,
    col_aff = "cancer", threshold = 0, sup_thres_aff = TRUE)
df <- generate_colors(df, "affected")$df
df <- select_from_inf(df, c(1, 2), 3)
df <- df[df$family == 1, ]

summary(df)
summary(df$affected)
ped <- pedigree(df$id, df$dadid, df$momid, df$sex, as.matrix(df[c("affected", "bcpc")]))
nb_ind_gen <- align(ped)$n
a <- plot(ped, ggplot_gen=T)
a$ggplot
plot_ped <- ped_plot(df, cex_plot = 0.5, mar = c(0.5, 0.5, 0.5, 0.5),
    psize = c(2, length(nb_ind_gen)),
    to_plotly = TRUE, title = "Test it is")