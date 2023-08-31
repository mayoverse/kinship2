test_that("Pedigree legend works", {
    data("sampleped")
    ped <- pedigree(sampleped)
    ped <- ped[ped$ped$family == "1", ]
    ped <- generate_colors(ped, add_to_scale = TRUE, "avail")
    ped <- generate_colors(ped,
        add_to_scale = TRUE, "indId", threshold = 115,
        colors_aff = c("pink", "purple"), keep_full_scale = TRUE
    )

    lst <- ped_to_legdf(ped, cex = 0.8)
    vdiffr::expect_doppelganger("Legend alone",
        function() {
            plot_fromdf(lst$leg_df,
                usr = c(-1, max(lst$leg_df$x0) + 1, -1, max(lst$leg_df$y0) + 1),
                add_to_existing = FALSE
            )
        }
    )

    vdiffr::expect_doppelganger("Plot with legend",
        function() {
            plot(ped, cex = 0.8, symbolsize = 1.5, mark = FALSE,
                legend = TRUE, leg_cex = 0.5, leg_symbolsize = 0.3,
                leg_loc = c(4, 18, 4.5, 4.9)
            )
        }
    )
})
