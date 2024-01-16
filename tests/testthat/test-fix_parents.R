test_that("fix_parents works with number", {
    materdf <- data.frame(id = 1:5, momid = c(0, 1, 1, 2, 2), sex = "female")
    materdf$dadid <- materdf$momid * 100
    materdf <- as.data.frame(lapply(materdf, as.character))
    expect_snapshot_error(Pedigree(materdf, missid = "0"))
    peddf <- with(materdf, fix_parents(id, dadid, momid, sex, missid = "0"))
    expect_no_error(Pedigree(peddf, missid = "0"))
})

test_that("fix_parents works with character", {
    test1char <- data.frame(id = paste("fam", 101:111, sep = ""),
        sex = c("male", "female")[c(1, 2, 1, 2, 1, 1, 2, 2, 1, 2, 1)],
        dadid = c(0, 0, "fam101", "fam101", "fam101", 0, 0,
            "fam106", "fam106", "fam106", "fam109"
        ),
        momid = c(0, 0, "fam102", "fam102", "fam102", 0, 0,
            "fam107", "fam107", "fam107", "fam112"
        )
    )
    expect_snapshot_error(Pedigree(test1char, missid = "0"))
    test1newmom <- with(test1char,
        fix_parents(id, dadid, momid, sex, missid = "0")
    )
    expect_no_error(Pedigree(test1newmom, missid = "0"))
})

test_that("fix_parents works with sex errors", {
    data("sampleped")
    datped2 <- sampleped[sampleped$famid %in% 2, ]
    datped2[datped2$id %in% 203, "sex"] <- 2
    datped2 <- datped2[-which(datped2$id %in% 209), ]

    ## this gets an error
    expect_snapshot_error(Pedigree(datped2))

    ## This fix the error and keep the dataframe dimensions
    fixped2 <- with(datped2,
        fix_parents(id, dadid, momid, sex, missid = NA_character_)
    )
    expect_equal(fixped2$sex[fixped2$id == 203], 1)
    expect_contains(fixped2$id, "209")
    expect_no_error(Pedigree(fixped2))
})

test_that("fix_parents works with famid", {
    data("sampleped")
    datped <- sampleped[-which(sampleped$id %in% 209), ]

    ## this gets an error
    expect_snapshot_error(Pedigree(datped))
    fixped <- fix_parents(datped)

    expect_contains(fixped$id, "2_209")
    expect_equal(fixped$sex[fixped$id == "2_209"], 1)
    expect_equal(fixped$famid[fixped$id == "2_209"], "2")
    expect_no_error(Pedigree(fixped))
})
