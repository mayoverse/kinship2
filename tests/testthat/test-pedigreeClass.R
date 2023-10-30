test_that("Pedigree works", {
    ped <- Pedigree(data.frame(
        id = character(),
        dadid = character(),
        momid = character(),
        sex = numeric(),
        family = character(),
        avail = numeric(),
        affection = numeric()
    ))
    expect_s4_class(ped, "Pedigree")
    expect_equal(nrow(ped@ped), 0)
    expect_equal(nrow(ped@rel), 0)
    expect_equal(nrow(ped@meta), 0)
    expect_equal(nrow(ped@deriv), 0)
    expect_equal(length(ped@scales), 2)
    expect_equal(length(ped@scales$fill), 9)
    expect_equal(length(ped@scales$border), 4)
})

test_that("Pedigree old usage compatibility", {
    data(sampleped)
    ped1 <- with(sampleped,
        Pedigree(id, dadid, momid, sex, family, avail, affection)
    )
    expect_equal(ped1, Pedigree(sampleped))

    ped2mat <- matrix(c(
        1, 1, 0, 0, 1,
        1, 2, 0, 0, 2,
        1, 3, 1, 2, 1,
        1, 4, 1, 2, 2,
        1, 5, 0, 0, 2,
        1, 6, 0, 0, 1,
        1, 7, 3, 5, 2,
        1, 8, 6, 4, 1,
        1, 9, 6, 4, 1,
        1, 10, 8, 7, 2
    ), ncol = 5, byrow = TRUE)

    ped2df <- as.data.frame(ped2mat)
    names(ped2df) <- c("family", "id", "dadid", "momid", "sex")
    ## 1 2  3 4 5 6 7 8 9 10,11,12,13,14,15,16
    ped2df$disease <- c(NA, NA, 1, 0, 0, 0, 0, 1, 1, 1)
    ped2df$smoker <- c(0, NA, 0, 0, 1, 1, 1, 0, 0, 0)
    ped2df$available <- c(0, 0, 1, 1, 0, 1, 1, 1, 1, 1)
    ped2df$status <- c(1, 1, 1, 0, 1, 0, 0, 0, 0, 0)

    ped2 <- with(ped2df, Pedigree(id, dadid, momid, sex, family,
        available, status, affected = cbind(disease, smoker, available),
        relation = matrix(c(8, 9, 1, 1), ncol = 4)
    ))
    rel_df <- data.frame(id1 = 8, id2 = 9, code = 1, family = 1)

    expect_equal(ped2,
        Pedigree(ped2df, col_aff = c("disease", "smoker", "available"),
            rel_df
        )
    )
})

test_that("Pedigree from sampleped and affectation", {
    # Here is a case where the levels fail to line up properly
    data("sampleped")
    df1 <- sampleped[sampleped$family == 1, ]
    colnames(df1)
    ped1 <- Pedigree(df1, cols_ren_ped = list(
        "indId" = "id",
        "fatherId" = "dadid",
        "motherId" = "momid",
        "gender" = "sex",
        "available" = "avail",
        "affection" = "affected"
    ))

    expect_equal(nrow(ped1@ped), 41)
    expect_equal(ncol(ped1@ped), 19)
    expect_equal(nrow(ped1@rel), 0)
    expect_equal(ncol(ped1@rel), 7)

    expect_error(ped1$ped$id <- "1")
    expect_error(ped1$ped$id[1] <- "1")
    expect_error(ped1$ped$id[1] <- "102")
    expect_error(ped1$ped$dadid[1] <- "101")
    expect_no_error(ped1$ped$dadid[3] <- "1_103")
    expect_warning(expect_error(ped1$ped$sex[3] <- "103"))
    expect_error(ped1$ped$sex[3] <- "female")
    expect_error(ped1$ped$sex[3] <- "unknown")
    expect_no_error(ped1$ped$sex[41] <- "unknown")

    expect_equal(as.data.frame(ped1), ped1$ped)
})

test_that("Pedigree subscripting", {
    data(minnbreast)
    minnped <- Pedigree(minnbreast, cols_ren_ped = list(
        "indId" = "id", "fatherId" = "fatherid",
        "motherId" = "motherid", "gender" = "sex", "family" = "famid",
        "affection" = "cancer"
    ))
    expect_equal(nrow(ped(minnped)), 28081)
    expect_equal(ncol(minnped[["ped"]]), 28)

    ped8 <- minnped[ped(minnped)$family == "8",
        c("id", "dadid", "momid", "sex", "affection")
    ]

    expect_equal(nrow(ped8$ped), 40)
    expect_equal(ncol(ped8$ped), 11)

    # Subjects 150, 152, 154, 158 are children,
    # and 143, 162, 149 are parents and a child
    droplist <- paste("8", c(150, 152, 154, 158, 143, 162, 149), sep = "_")

    keep1 <- !(ped8$ped$id %in% droplist)  # logical
    keep2 <- which(keep1)  # numeric
    keep3 <- as.character(ped8$ped$id[keep1])  # character
    keep4 <- factor(keep3)

    test1 <- ped8[keep1, ]
    test2 <- ped8[keep2, ]
    test3 <- ped8[keep3, ]
    test4 <- ped8[keep4, ]

    expect_equal(test1, test2)
    expect_equal(test1, test3)
    expect_equal(test1, test4)

    pedcol <- minnped[, c("id", "dadid", "momid", "sex", "affection")]
    expect_equal(nrow(pedcol$ped), 28081)
    expect_equal(ncol(pedcol$ped), 11)

    pedrow <- minnped[c("8_150", "8_163", "8_145", "8_135", "8_136")]
    expect_equal(nrow(pedrow$ped), 5)
    expect_equal(ncol(pedrow$ped), 28)
})

test_that("Pedigree to dataframe", {
    data("sampleped")
    ped <- Pedigree(sampleped)
    expect_equal(dim(as.data.frame(ped)), c(55, 19))
})

test_that("Pedigree length", {
    data("sampleped")
    ped <- Pedigree(sampleped)
    expect_equal(length(ped), 55)
})

test_that("Pedigree getters", {
    data("sampleped")
    ped <- Pedigree(sampleped)
    expect_equal(ped$ped, ped(ped))
    expect_equal(ped$rel, rel(ped))
    expect_equal(ped$hints, hints(ped))
    expect_equal(ped$hints$horder, order(ped))
    expect_equal(ped$hints$spouse, spouse(ped))
    expect_equal(ped$scales$fill, fill(ped))
    expect_equal(ped$scales$border, border(ped))
})

test_that("Pedigree setters", {
    data("sampleped")
    ped <- Pedigree(sampleped)
    peddf <- ped(ped)
    peddf[1, "family"] <- "2"
    ped(ped) <- peddf
    expect_equal(ped(ped)[1, "family"], "2")
})
