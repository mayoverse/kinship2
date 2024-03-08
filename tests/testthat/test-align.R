test_that("align works", {
    data("sampleped")
    pedi <- Pedigree(sampleped)
    ped1 <- pedi[famid(pedi) == "1"]
    plist1 <- align(ped1)
    expect_equal(plist1$n, c(2, 10, 16, 14))

    ped2 <- pedi[famid(pedi) == 2]
    plist2 <- align(ped2)
    expect_equal(plist2$n, c(2, 7, 5))

    plist <- align(pedi)
    expect_equal(plist[["1"]]$n, plist1$n)
    expect_equal(plist[["2"]]$n, plist2$n)
})

test_that("test auto_hint works", {
    data("sampleped")
    pedi <- Pedigree(sampleped)
    expect_equal(sum(kindepth(pedi)), 73)
    expect_error(auto_hint(pedi)) # Works only on 1 family

    pedi <- Pedigree(sampleped[-1])
    newhint <- auto_hint(pedi)
    plist <- align(pedi, packed = TRUE,
        align = TRUE, width = 8, hints = newhint
    )
    expect_snapshot(plist)

    ## With rel matrix
    rel_df <- data.frame(
        id1 = c(112, 113, 133, 209),
        id2 = c(110, 114, 132, 109),
        code = c(1, 4, 4, 4)
    )
    pedi <- Pedigree(sampleped[-1], rel_df)
    newhint <- auto_hint(pedi)
    expect_equal(horder(newhint),
        setNames(c(
            1, 2, 3, 4, 5, 6, 7, 8, 1, 1, 3, 2, 1, 4, 1,
            3, 9, 2, 4, 10, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10,
            11, 12, 13, 14, 2, 3, 10, 11, 11, 12, 13, 4,
            5, 12, 13, 14, 15, 16, 17, 18, 14, 15, 16, 17, 18
        ), id(ped(pedi)))
    )
    expect_equal(as.data.frame(spouse(newhint)),
        data.frame(
            idl = c("112", "114", "109"),
            idr = c("118", "115", "110"),
            anchor = anchor_to_factor(c("right", "right", "left"))
        )
    )
})

test_that("test alignment with inbreeding and relationship matrix", {
    data("sampleped")
    rel_df <- data.frame(
        id1 = c(112, 113, 133, 209),
        id2 = c(110, 114, 132, 109),
        code = c(1, 4, 4, 4)
    )
    ped_withrel <- Pedigree(sampleped[-1], rel_df)
    plist <- align(ped_withrel)

    ped_norel <- Pedigree(sampleped[-1])
    plist_sr <- align(ped_norel)

    expect_equal(plist$nid[1, ],
        c(35, 36, 42, 43, rep(0, 19))
    )
    expect_equal(plist_sr$nid[1, ],
        c(35, 36, 42, 43, rep(0, 17))
    )
    vdiffr::expect_doppelganger("sampleped_withrel",
        function() plot(ped_withrel)
    )
    vdiffr::expect_doppelganger("sampleped_norel",
        function() plot(ped_norel)
    )
})

test_that("besthint works", {
    data("sampleped")
    pedi <- Pedigree(sampleped)
    expect_error(best_hint(pedi))  #this fixes up marriages and such

    ped1 <- Pedigree(sampleped[-1])
    hints(ped1) <- best_hint(ped1)

    vdiffr::expect_doppelganger("Best hint",
        function() plot(ped1)
    )

    plist <- align(ped1, packed = TRUE,
        align = TRUE, width = 8
    )
    expect_snapshot(plist)
})

test_that("Alignment with spouse", {
    data(sampleped)
    df1 <- sampleped[sampleped$famid == 1, ]
    relate1 <- data.frame(
        indId1 = 113,
        indId2 = 114,
        code = 4,
        family = 1
    )
    ped1 <- Pedigree(df1, relate1)
    hints <- auto_hint(ped1)
    expect_equal(spouse(hints),
        data.frame(
            idl = c("1_112", "1_114", "1_109"),
            idr = c("1_118", "1_115", "1_110"),
            anchor = anchor_to_factor(c("right", "right", "left"))
        )
    )
    expect_equal(horder(hints),
        setNames(c(
            1, 2, 3, 4, 5, 6, 7, 8, 1, 1,
            2, 3, 1, 4, 1, 3, 9, 2, 4, 10,
            1, 2, 3, 4, 5, 6, 7, 8, 9, 10,
            11, 12, 13, 14, 2, 3, 10, 11, 11,
            12, 13
        ), id(ped(ped1)))
    )
})
