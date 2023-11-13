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


    df <- data.frame(
        id = 1:9,
        dadid = c(0, 0, 0, 1, 1, 1, 3, 7, 5),
        momid = c(0, 0, 0, 2, 2, 2, 2, 6, 8),
        sex = c(1, 2, 1, 2, 1, 2, 1, 2, 1)
    )
    pedi <- Pedigree(df, missid = "0")
    kindepth(pedi, align_parents = TRUE)
    plot(pedi)
})
