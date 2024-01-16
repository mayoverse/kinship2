test_that("Kindepth works", {
    data("sampleped")
    datped2 <- sampleped[sampleped$famid %in% 2, ]
    ## this gets an error
    ped <- Pedigree(datped2)

    expect_equal(
        kindepth(ped, align_parents = TRUE),
        c(0, 0, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2)
    )
    expect_equal(
        kindepth(ped, align_parents = FALSE),
        c(0, 0, 0, 1, 1, 1, 1, 1, 0, 2, 2, 2, 2, 2)
    )

    data(minnbreast)
    ped <- Pedigree(minnbreast, cols_ren_ped = list(
        "indId" = "id", "fatherId" = "fatherid",
        "motherId" = "motherid", "gender" = "sex", "family" = "famid"
    ), missid = "0")
    expect_equal(sum(kindepth(ped)), 33147)
    expect_equal(sum(kindepth(ped, align_parents = TRUE)), 39091)

    df <- data.frame(
        id = 1:7,
        dadid = c(0, 0, 0, 1, 3, 0, 3),
        momid = c(0, 0, 0, 2, 4, 0, 6),
        sex = c(1, 2, 1, 2, 1, 2, 1)
    )
    pedi <- Pedigree(df, missid = "0")
    expect_equal(kindepth(pedi, align_parents = TRUE), c(0, 0, 1, 1, 2, 1, 2))
    expect_equal(kindepth(pedi), c(0, 0, 0, 1, 2, 0, 1))

    ## Uncle / Niece spouse
    df <- data.frame(
        id = 1:7,
        dadid = c(0, 0, 0, 1, 1, 3, 5),
        momid = c(0, 0, 0, 2, 2, 4, 6),
        sex = c(1, 2, 1, 2, 1, 2, 1)
    )
    pedi <- Pedigree(df, missid = "0")
    expect_equal(kindepth(pedi, align_parents = TRUE), c(0, 0, 1, 1, 1, 2, 3))
    vdiffr::expect_doppelganger("Niece Uncle spouse",
        function() plot(pedi)
    )

    ## Double marriage
    ## reported on github in 2023
    ## version 1.9.6 failed to plot subject 3 second marriage and kids
    ## fix in 9/2023 to revert to some version 1.8.5 version of kindepth
    df <- data.frame(
        id = 1:12,
        dadid = c(0, 0, 1, 0, 0, 0, 3, 3, 5, 5, 7, 10),
        momid = c(0, 0, 2, 0, 0, 0, 4, 4, 6, 6, 9, 8),
        sex = c(1, 2, 1, 2, 1, 2, 1, 2, 2, 1, 1, 2)
    )
    pedi <- Pedigree(df, missid = "0")
    expect_equal(
        kindepth(pedi, align_parents = TRUE),
        c(0, 0, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3)
    )
    vdiffr::expect_doppelganger("Double marriage",
        function() plot(pedi)
    )
})
