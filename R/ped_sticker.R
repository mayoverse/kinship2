#### Libraries needed ####
library(magick)
library(dplyr)
library(hexSticker)
library(kinship2)

# Charge necessary data
data(sample.ped)

# Create pedigree object
ped_all <- with(sample.ped, pedigree(id, father, mother, sex,
   affected = cbind(affected, avail), famid = ped
 ))

# Select first family
ped1 <- ped_all["1"]

# Shrink pedigree
ped1trim <- pedigree.shrink(ped1, maxBits = 12,
    avail = sample.ped[sample.ped$ped == 1, "avail"])

# Export plot to png
output_dir <- "inst/figures/graph.png"
png(output_dir,
    width = 500, height = 300, units = "px", bg = "#FFFFFF00")
op <- par(mar = rep(0.5, 4))
plot.pedigree.shrink(ped1trim, mar = rep(0, 4), xlegend = 4)
legend(
    x = 3.5, y = 2.7, legend = c("DNA Available", "UnAvailable"),
    pch = c(1, 1), col = c(2, 1), bty = "n", cex = 1
  )
dev.off()
dev.off()
par(op)

# Create sticker
s <- sticker(output_dir,
          package = "kinship2", p_size = 60, p_color = "#000000",
          p_x = 1, p_y = 1.5,
          s_x = 1, s_y = 0.8, s_width = 0.8, s_height = 0.5,
          h_fill = "#FFFFFF00", h_color = "#3576ac",
          filename = "inst/figures/icon_kinship2.png", dpi = 800)

plot(s)
