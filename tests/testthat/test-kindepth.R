test_that("Kindepth works", {
    data("sampleped")
    datped2 <- sampleped[sampleped$famid %in% 2, ]
    ## this gets an error
    ped <- Pedigree(datped2)

    expect_equal(
        kindepth(ped, align = TRUE),
        c(0, 0, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2)
    )
    expect_equal(
        kindepth(ped, align = FALSE),
        c(0, 0, 0, 1, 1, 1, 1, 1, 0, 2, 2, 2, 2, 2)
    )

    data(minnbreast)
    ped <- Pedigree(minnbreast, cols_ren_ped = list(
        "indId" = "id", "fatherId" = "fatherid",
        "motherId" = "motherid", "gender" = "sex", "family" = "famid"
    ), missid = "0")
    expect_equal(sum(kindepth(ped)), 33147)
    expect_equal(sum(kindepth(ped, align = TRUE)), 39087)
})
