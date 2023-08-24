test_that("align works", {
    data("sampleped")
    ped <- pedigree(sampleped)
    ped1 <- ped[ped$ped$family == 1]
    plist1 <- align(ped1)
    expect_equal(plist1$n, c(6, 12, 17, 8))

    ped2 <- ped[ped$ped$family == 2]
    withr::local_options(width = 50)
    plist2 <- align(ped2)
    expect_equal(plist2$n, c(2, 7, 5))

    plist <- align(ped)
    expect_equal(plist[[1]]$n, plist1$n)
    expect_equal(plist[[2]]$n, plist2$n)

})

test_that("test auto_hint works", {
    data("sampleped")
    ped <- pedigree(sampleped)
    expect_equal(sum(kindepth(ped)), 73)
    expect_error(auto_hint(ped))  #this fixes up marriages and such

    ped <- pedigree(sampleped[-1])
    newhint <- auto_hint(ped)
    plist <- align(ped, packed = TRUE,
        align = TRUE, width = 8, hints = newhint
    )
    expect_snapshot(plist)
})

test_that("test alignement with inbreeding and relationship matrix", {
    data("sampleped")
    rel_df <- data.frame(
        id1 = c(112, 113, 133, 209),
        id2 = c(110, 114, 132, 109),
        code = c(1, 4, 4, 4)
    )
    ped <- pedigree(sampleped[-1], rel_df = rel_df)
    plist <- align(ped)

    ped_sr <- pedigree(sampleped[-1])
    plist_sr <- align(ped_sr)
    plist

    expect_equal(plist$nid[1, ],
        c(35, 36, 5, 6,  7,  8, 42, 43, rep(0, 14))
    )
    expect_equal(plist_sr$nid[1, ],
        c(5, 6, 7, 8, 35, 36, 42, 43, rep(0, 14))
    )
})

test_that("besthint works", {
    data("sampleped")
    ped <- pedigree(sampleped)
    expect_error(best_hint(ped))  #this fixes up marriages and such

    ped1 <- pedigree(sampleped[-1])
    newhint1 <- best_hint(ped1)
    plist <- align(ped1, packed = TRUE,
        align = TRUE, width = 8, hints = newhint1
    )
    expect_snapshot(plist)
})
TRUE
