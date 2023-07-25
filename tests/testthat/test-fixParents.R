test_that("fixParents works with number", {
    materdf <- data.frame(id = 1:5, momid = c(0, 1, 1, 2, 2), sex = 2)

    materdf$dadid <- materdf$momid * 100
    expect_error(with(materdf, pedigree(id, dadid, momid, sex)))
    peddf <- with(materdf, fixParents(id, dadid, momid, sex))
    expect_no_error(with(peddf, pedigree(id, dadid, momid, sex)))
})

test_that("fixParrents works with character", {
    test1char <- data.frame(id = paste("fam", 101:111, sep = ""), sex = c("male",
        "female")[c(1, 2, 1, 2, 1, 1, 2, 2, 1, 2, 1)], father = c(0, 0, "fam101",
        "fam101", "fam101", 0, 0, "fam106", "fam106", "fam106", "fam109"), mother = c(0,
        0, "fam102", "fam102", "fam102", 0, 0, "fam107", "fam107", "fam107", "fam112"))
    expect_error(with(test1char, pedigree(id, father, mother, sex, missid = "0")))
    test1newmom <- with(test1char, fixParents(id, father, mother, sex, missid = "0"))
    expect_no_error(with(test1newmom, pedigree(id, dadid, momid, sex, missid = "0")))
})

test_that("fixParrents works with sex errors", {
    data(sample.ped)
    datped2 <- sample.ped[sample.ped$ped %in% 2, ]
    datped2[datped2$id %in% 203, "sex"] <- 2
    datped2 <- datped2[-which(datped2$id %in% 209), ]

    ## this gets an error
    expect_error(with(datped2, pedigree(id, father, mother, sex)))

    ## This fix the error
    fixped2 <- with(datped2, fixParents(id, father, mother, sex))
    expect_no_error(with(fixped2, pedigree(id, dadid, momid, sex)))
})


test_that("fixParents_df works with sex errors", {
    data(sample.ped)
    datped2 <- sample.ped[sample.ped$ped %in% 2, ]
    # Set individual 203 as female
    datped2[datped2$id %in% 203, "sex"] <- 2
    # Delete individual 209 from id
    datped2 <- datped2[-which(datped2$id %in% 209), ]

    ## this gets an error
    expect_error(with(datped2, pedigree(id, father, mother, sex)))

    ## This fix the error and keep the dataframe dimensions
    fixped2 <- fixParents.data.frame(datped2, momid = "mother", dadid = "father",
        delete = TRUE)
    expect_no_error(with(fixped2, pedigree(id, dadid, momid, sex)))
    expect_equal(dim(fixped2), c(13, 7))

    fixped2 <- fixParents.data.frame(datped2, momid = "mother", dadid = "father",
        delete = FALSE)
    expect_no_error(with(fixped2, pedigree(id, dadid, momid, sex)))
    expect_equal(dim(fixped2), c(14, 7))
})
TRUE
