#### Libraries needed ####
usethis::use_package("magick")
usethis::use_package("dplyr")
usethis::use_package("hexSticker")

library(Pedixplorer)

# Charge necessary data
data(sampleped)

# Create Pedigree object
ped1 <- Pedigree(sampleped[sampleped$family == "1", -1])

# Shrink Pedigree
ped1trim <- shrink(ped1, max_bits = 12)

# Export plot to png
output_dir <- "inst/figures/graph.png"
png(output_dir,
    width = 1300, height = 900,
    units = "px", bg = "#FFFFFF00", pointsize = 36)
op <- par(mar = c(5.5, 0.5, 0.5, 0.5), lwd = 3,
    oma = c(0,0,0,0))

lst <- ped_to_plotdf(ped1trim$pedObj, cex = 1.2, symbolsize = 1, aff_mark = FALSE
)
lst$par_usr$usr[3] <- 3.6
p <- plot_fromdf(lst$df, usr = lst$par_usr$usr,
    boxw = lst$par_usr$boxw, boxh = lst$par_usr$boxh
)
leg_symbolsize <- 0.6
leg <- ped_to_legdf(ped1trim$pedObj, cex = 0.8,
    boxw = lst$par_usr$boxw * leg_symbolsize,
    boxh = lst$par_usr$boxh * leg_symbolsize,
    adjx = 1, adjy = 0.15
)
leg_loc <- c(0.5, 5, 3.8, 4.5)
leg$leg_df$x0 <- scales::rescale(leg$leg_df$x0,
    c(leg_loc[1], leg_loc[2])
)
leg$leg_df$y0 <- scales::rescale(leg$leg_df$y0,
    c(leg_loc[3], leg_loc[4])
)
clip(leg_loc[1] - 1, leg_loc[2] + 1, leg_loc[3] - 1, leg_loc[4] + 1)
plot_fromdf(leg$leg_df, add_to_existing = TRUE,
    boxw = lst$par_usr$boxw * leg_symbolsize,
    boxh = lst$par_usr$boxh * leg_symbolsize
)
#box("figure", col = "#000000", lwd = 2)
#box("plot", col = "#ff0000", lwd = 2)
#box("inner", col = "#001aff", lwd = 2)
#box("outer", col = "#00ff00", lwd = 2)
dev.off()
dev.off()
par(op)

# Create sticker
s <- hexSticker::sticker(
    output_dir,
    package = "Pedixplorer", p_size = 50, p_color = "#000000",
    p_x = 1, p_y = 1.5,
    s_x = 1, s_y = 0.8, s_width = 0.7, s_height = 0.4,
    h_fill = "#FFFFFF00", h_color = "#3576ac",
    filename = "inst/figures/icon_Pedixplorer.png", dpi = 1000
)

plot(s)
