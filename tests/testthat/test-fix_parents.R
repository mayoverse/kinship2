test_that("fix_parents works with number", {
    materdf <- data.frame(id = 1:5, momid = c(0, 1, 1, 2, 2), sex = "female")
    materdf$dadid <- materdf$momid * 100
    materdf <- as.data.frame(lapply(materdf, as.character))
    expect_error(Pedigree(materdf))
    peddf <- with(materdf, fix_parents(id, dadid, momid, sex, missid = "0"))
    expect_no_error(Pedigree(peddf))
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
    expect_error(Pedigree(test1char))
    test1newmom <- with(test1char,
        fix_parents(id, dadid, momid, sex, missid = "0")
    )
    expect_no_error(Pedigree(test1newmom))
})

test_that("fix_parents works with sex errors", {
    data("sampleped")
    datped2 <- sampleped[sampleped$famid %in% 2, ]
    datped2[datped2$id %in% 203, "sex"] <- 2
    datped2 <- datped2[-which(datped2$id %in% 209), ]

    ## this gets an error
    expect_warning(Pedigree(datped2))

    ## This fix the error
    datped2[, c("id", "momid", "dadid")] <- as.data.frame(lapply(
        datped2[, c("id", "momid", "dadid")], as.character
    ))
    fixped2 <- with(datped2, fix_parents(id, dadid, momid, sex, missid = "0"))
    expect_no_error(Pedigree(fixped2))
})


test_that("fix_parents_df works with sex errors and with family", {
    data("sampleped")
    datped2 <- sampleped[sampleped$famid %in% 2, ]
    # Set individual 203 as female
    datped2[datped2$id %in% 203, "sex"] <- 2
    # Delete individual 209 from id
    datped2 <- datped2[-which(datped2$id %in% 209), ]

    ## this gets an error
    expect_warning(Pedigree(datped2))

    ## This fix the error and keep the dataframe dimensions
    datped2[, c("id", "momid", "dadid")] <- as.data.frame(lapply(
        datped2[, c("id", "momid", "dadid")], as.character
    ))
    fixped2 <- fix_parents(datped2, delete = TRUE)
    expect_no_error(Pedigree(fixped2))
    expect_equal(dim(fixped2), c(13, 7))

    fixped2 <- fix_parents(datped2, delete = FALSE)
    expect_no_error(Pedigree(fixped2))
    expect_equal(dim(fixped2), c(14, 7))
})