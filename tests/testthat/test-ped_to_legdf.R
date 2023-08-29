test_that("Pedigree legend works", {
    data("sampleped")
    ped <- pedigree(sampleped)
    ped1 <- ped[ped$ped$family == "1", ]
    ped1$ped
    ped1 <- generate_colors(ped1, add_to_scale = TRUE, "avail")
    plot(ped1)
    par_usr <- ped_to_plotdf(ped1)$par_usr
    lst <- ped_to_legdf(ped1, par = par_usr, cex = 1)
    plot_from_df(lst$df, usr = lst$par_usr$usr, add_to_existing = FALSE)
})