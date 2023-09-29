#### Libraries needed ####
usethis::use_package("magick")
usethis::use_package("dplyr")
usethis::use_package("hexSticker")

library(Pedigree)

# Charge necessary data
data(sampleped)

# Create pedigree object
ped1 <- pedigree(sampleped[sampleped$family == "1", ])

# Shrink pedigree
ped1trim <- shrink(ped1, max_bits = 12)

# Export plot to png
output_dir <- "inst/figures/graph.png"
png(output_dir,
    width = 500, height = 300, units = "px", bg = "#FFFFFF00")
op <- par(mar = rep(0.5, 4))

plot(ped1trim$pedObj, mar = rep(0, 4))
legend(
    x = 3.5, y = 2.7, legend = c("DNA Available", "UnAvailable"),
    pch = c(1, 1), col = c(2, 1), bty = "n", cex = 1
)
dev.off()
dev.off()
par(op)

# Create sticker
s <- sticker(
    output_dir,
    package = "Pedigree", p_size = 60, p_color = "#000000",
    p_x = 1, p_y = 1.5,
    s_x = 1, s_y = 0.8, s_width = 0.8, s_height = 0.5,
    h_fill = "#FFFFFF00", h_color = "#3576ac",
    filename = "inst/figures/icon_Pedigree.png", dpi = 800
)

plot(s)
