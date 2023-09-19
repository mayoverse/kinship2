test_that("fix_parents_df works with sex errors and with family", {
    data("sampleped")
    datped2 <- sampleped[sampleped$family %in% 2, ]
    ## this gets an error
    ped <- pedigree(datped2)

    expect_equal(
        kindepth(ped, align = TRUE),
        c(0, 0, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2)
    )
    expect_equal(
        kindepth(ped, align = FALSE),
        c(0, 0, 0, 1, 1, 1, 1, 1, 0, 2, 2, 2, 2, 2)
    )

    data(minnbreast)
    ped <- pedigree(minnbreast, cols_ren_ped = list(
        "indId" = "id", "fatherId" = "fatherid",
        "motherId" = "motherid", "gender" = "sex", "family" = "famid"
    ))
    expect_equal(sum(kindepth(ped)), 33147)
    expect_equal(sum(kindepth(ped, align = TRUE)), 39087)
})