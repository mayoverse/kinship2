test_that("Pedigree plotting test", {
    ped2mat <- matrix(
        c(
            1, 1, 0, 0, 1, 1, 0,
            1, 2, 0, 0, 2, 0, 1,
            1, 3, 1, 2, 1, 0, 1,
            1, 4, 1, 2, 2, 1, 0,
            1, 5, 0, 0, 2, 1, 1,
            1, 6, 0, 0, 1, 0, 1,
            1, 7, 3, 5, 2, 0, 0,
            1, 8, 6, 4, 1, 1, 1,
            1, 9, 6, 4, 1, 0, 1,
            1, 10, 8, 7, 2, 0, 0
        ), ncol = 7, byrow = TRUE
    )
    ped2df <- as.data.frame(ped2mat)
    names(ped2df) <- c("family", "indId", "fatherId", "motherId",
        "gender", "affection", "available"
    )

    ped2df$disease <- c(NA, NA, 1, 0, 0, 0, 0, 1, 1, 1)
    ped2df$smoker <- c(0, NA, 0, 0, 1, 1, 1, 0, 0, 0)
    ped2df$availability <- c(0, 0, 1, 1, 0, 1, 1, 1, 1, 1)
    ped2df$vitalStatus <- c(1, 1, 1, 0, 1, 0, 0, 8, 0, 0)

    rel_df <- data.frame(indId1 = 8, indId2 = 9, code = 3, family = 1)
    ped <- Pedigree(ped2df, rel_df, missid = "0")
    ped
    vdiffr::expect_doppelganger("Ped simple affection",
        function() plot(ped)
    )

    lst <- plot(ped, label = "smoker", aff_mark = FALSE, ggplot_gen = TRUE)
    vdiffr::expect_doppelganger("Ped simple affection ggplot",
        function() plot(lst$ggplot)
    )

    ped <- generate_colors(ped, add_to_scale = TRUE,
        col_aff = "smoker", colors_aff = c("#00e6ee", "#c300ff")
    )

    lst <- ped_to_plotdf(ped)
    expect_equal(length(lst), 2)
    expect_equal(dim(lst$df), c(82, 15))
    expect_snapshot(lst)
    p <- plot(ped, title = "Pedigree", ggplot_gen = TRUE)
    vdiffr::expect_doppelganger("Ped 2 affections ggplot",
        function() plot(p$ggplot)
    )
})

test_that("Pedigree fails to line up", {
    # Here is a case where the levels fail to line up properly
    data(sampleped)
    df1 <- sampleped[sampleped$famid == "1", ]
    ped1 <- Pedigree(df1)
    vdiffr::expect_doppelganger("ped1",
        function() plot(ped1)
    )
    # With reordering it's better
    df1reord <- df1[c(35:41, 1:34), ]
    ped1reord <- Pedigree(df1reord)
    vdiffr::expect_doppelganger("ped1reorder",
        function() plot(ped1reord)
    )
})
