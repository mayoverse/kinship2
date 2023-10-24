test_that("align works", {
    data("sampleped")
    ped <- Pedigree(sampleped)
    ped1 <- ped[ped(ped, "family") == 1]
    plist1 <- align(ped1)
    expect_equal(plist1$n, c(6, 12, 17, 8))

    ped2 <- ped[ped(ped, "family") == 2]
    withr::local_options(width = 50)
    plist2 <- align(ped2)
    expect_equal(plist2$n, c(2, 7, 5))

    plist <- align(ped)
    expect_equal(plist[["1"]]$n, plist1$n)
    expect_equal(plist[["2"]]$n, plist2$n)

})

test_that("test auto_hint works", {
    data("sampleped")
    ped <- Pedigree(sampleped)
    expect_equal(sum(kindepth(ped)), 73)
    expect_error(auto_hint(ped))  #this fixes up marriages and such

    ped <- Pedigree(sampleped[-1])
    newhint <- auto_hint(ped)
    plist <- align(ped, packed = TRUE,
        align = TRUE, width = 8, hints = newhint
    )
    expect_snapshot(plist)
})

test_that("test alignment with inbreeding and relationship matrix", {
    data("sampleped")
    rel_df <- data.frame(
        id1 = c(112, 113, 133, 209),
        id2 = c(110, 114, 132, 109),
        code = c(1, 4, 4, 4)
    )
    ped <- Pedigree(sampleped[-1], rel_df)
    plist <- align(ped)

    ped_sr <- Pedigree(sampleped[-1])
    plist_sr <- align(ped_sr)

    expect_equal(plist$nid[1, ],
        c(35, 36, 5, 6,  7,  8, 42, 43, rep(0, 16))
    )
    expect_equal(plist_sr$nid[1, ],
        c(5, 6, 7, 8, 35, 36, 42, 43, rep(0, 14))
    )
})

test_that("besthint works", {
    data("sampleped")
    ped <- Pedigree(sampleped)
    expect_error(best_hint(ped))  #this fixes up marriages and such

    ped1 <- Pedigree(sampleped[-1])
    newhint1 <- best_hint(ped1)
    plist <- align(ped1, packed = TRUE,
        align = TRUE, width = 8, hints = newhint1
    )
    expect_snapshot(plist)
})

test_that("Alignement with spouse", {
    data(sampleped)
    df1 <- sampleped[sampleped$family == 1, ]
    relate1 <- data.frame(
        indId1 = 113,
        indId2 = 114,
        code = 4,
        family = 1
    )
    ped1 <- Pedigree(df1, relate1)
    hints <- auto_hint(ped1)
    expect_equal(as.vector(hints$spouse), c(9, 10, 2))
    expect_equal(hints$order,
        c(
            1, 2, 3, 4, 1, 2, 3, 4, 1, 1,
            2, 3, 5, 4, 5, 6, 7, 8, 9, 10,
            1, 2, 3, 4, 5, 6, 7, 8, 6, 7,
            8, 9, 10, 11, 6, 7, 11, 12, 12,
            13, 14
        )
    )
    align(ped1)
})

test_that("Double wife", {
    ## reported on github in 2023
    ## version 1.9.6 failed to plot subject 3 second marriage and kids
    ## fix in 9/2023 to revert to some version 1.8.5 version of kindepth
    df <- data.frame(
        id = 1:7, dadid = c(0, 0, 0, 1, 3, 0, 3),
        momid = c(0, 0, 0, 2, 4, 0, 6), sex = c(1, 2, 1, 2, 1, 2, 1)
    )
    ped <- Pedigree(df)
    vdiffr::expect_doppelganger("double_wife",
        function() plot(ped)
    )
})