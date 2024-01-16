new_par <- list(
    bg = "transparent",
    usr = c(0, 1, 0, 1),
    fig = c(0, 1, 0, 1),
    mai = c(0, 0, 0, 0),
    mar = c(0, 0, 0, 0),
    pin = c(7, 7),
    plt = c(0, 1, 0, 1),
    xaxp = c(0, 4, 4),
    xpd = FALSE,
    yaxp = c(2, 2, 1),
    fin = c(7, 7),
    pty = "m"
)

test_that("Pedigree legend works", {
    data("sampleped")
    sampleped$val_num <- as.numeric(sampleped$id)
    ped <- Pedigree(sampleped)
    ped <- ped[ped(ped, "famid") == "1"]
    ped <- generate_colors(ped, add_to_scale = TRUE, "avail", mods_aff = TRUE)
    ped <- generate_colors(ped,
        add_to_scale = TRUE, "val_num", threshold = 115,
        colors_aff = c("pink", "purple"), keep_full_scale = TRUE
    )
    lst <- ped_to_legdf(ped, boxh = 1, boxw = 1, cex = 0.8)
    expect_snapshot(lst)

    vdiffr::expect_doppelganger("Legend alone",
        function() {
            plot_fromdf(lst$df,
                usr = c(-1, max(lst$df$x0) + 1, -1, max(lst$df$y0) + 1),
                add_to_existing = FALSE
            )
        }
    )

    vdiffr::expect_doppelganger("Plot with legend",
        function() {
            plot(ped, cex = 0.8, symbolsize = 1.5, aff_mark = FALSE,
                legend = TRUE, leg_cex = 0.5, leg_symbolsize = 0.3,
                leg_loc = c(4, 18, 4.5, 4.9)
            )
        }
    )
})
