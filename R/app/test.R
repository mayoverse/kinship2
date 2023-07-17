setwd("R/app")
library(devtools)
load_all()
library(shiny)
library(dplyr)
library(ggplot2)
source("data_management.R")
source("data_import.R")
source("data_col_sel.R")
source("data_download.R")
source("plot_ped.R")
source("plot_download.R")
source("utils.R")

runApp()

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

ped <- with(fam_df, pedigree(id, dadid, momid, sex, affected))
legendPlot(ped, affected.label = "Affected")
ncol(ped$affected)
ped$affected
data(testped1)
summary(testped1)
cols_ren <- c("indId" = "id", "fatherId" = "father",
    "motherId" = "mother", "gender" = "sex")
data.table::setnames(testped1,
                    old = as.vector(unlist(cols_ren)),
                    new = names(cols_ren))
df <- check_data(testped1)
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
nb_ind_gen <- align.pedigree(ped)$n
plot_ped <- ped_plot(df, cex_plot = 0.5, mar = c(0.5, 0.5, 0.5, 0.5),
    psize = c(2, length(nb_ind_gen)),
    to_plotly = FALSE)

data(minnbreast)
df <- minnbreast
summary(df)
df[, c("indId", "fatherId", "motherId",
    "gender")] <- df[, c("id", "fatherid", "motherid",
        "sex")]
df <- check_data(df)
df <- generate_aff_inds(df$norm,
    col_aff = "cancer", threshold = 0, sup_thres_aff = TRUE)
df <- generate_colors(df, "affected")$df
df <- select_from_inf(df, c(1, 2), 3)
df <- df[df$family == 1, ]
ped <- with(df, pedigree(id, dadid, momid, sex, affected))
nb_ind_gen <- align.pedigree(ped)$n
plot_ped <- ped_plot(df, cex_plot = 0.5, mar = c(0.5, 0.5, 0.5, 0.5),
    psize = c(2, length(nb_ind_gen)),
    to_plotly = TRUE, title = "Test it is")
plot_ped
plot(ped, title = "Test it is", ggplot_gen = TRUE)
library(shiny)
library(DT)
shinyApp(
  ui = fluidPage(DTOutput('tbl', height = "100%")),
  server = function(input, output) {
    output$tbl = renderDT(
      iris[1:3,], options = list(lengthChange = TRUE,
      paging = FALSE, scrollX = TRUE,
        scrollY = "200px",
        scrollCollapse = TRUE), selection = 'none'
    )
  }
)
