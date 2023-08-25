test_that("pedigree other test", {
    ped2mat <- matrix(
        c(
            1, 1, 0, 0, 1,
            1, 2, 0, 0, 2,
            1, 3, 1, 2, 1,
            1, 4, 1, 2, 2,
            1, 5, 0, 0, 2,
            1, 6, 0, 0, 1,
            1, 7, 3, 5, 2,
            1, 8, 6, 4, 1,
            1, 9, 6, 4, 1,
            1, 10, 8, 7, 2
        ), ncol = 5, byrow = TRUE
    )

    ped2df <- as.data.frame(ped2mat)
    names(ped2df) <- c("family", "indId", "fatherId", "motherId", "gender")
    ## 1 2  3 4 5 6 7 8 9 10,11,12,13,14,15,16
    ped2df$disease <- c(NA, NA, 1, 0, 0, 0, 0, 1, 1, 1)
    ped2df$smoker <- c(0, NA, 0, 0, 1, 1, 1, 0, 0, 0)
    ped2df$availability <- c(0, 0, 1, 1, 0, 1, 1, 1, 1, 1)
    ped2df$status <- c(1, 1, 1, 0, 1, 0, 0, 0, 0, 0)

    rel_df <- data.frame(indId1 = 8, indId2 = 9, code = 1, family = 1)
    ped <- pedigree(ped2df, rel_df = rel_df)
    plot(ped, label = ped$ped$smoker, mark = ped$ped$disease)

    p <- plot(ped, label = ped$ped$smoker, mark = ped$ped$disease, ggplot_gen = TRUE)
    p$ggplot

    ped_to_plotdf(ped)
    vdiffr::expect_doppelganger("OtherPed with twin", ped)
})