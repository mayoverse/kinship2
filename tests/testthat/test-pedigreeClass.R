test_that("pedigree works", {
    ped <- pedigree()
    expect_s4_class(ped, "Pedigree")
    expect_equal(nrow(ped@ped), 0)
    expect_equal(nrow(ped@rel), 0)
    expect_equal(length(ped@scales), 2)
    expect_equal(length(ped@scales$fill), 7)
    expect_equal(length(ped@scales$border), 4)

    expect_snapshot(summary(ped))
})

test_that("pedigree from sampleped and affectation", {
    # Here is a case where the levels fail to line up properly
    data(sampleped)
    df1 <- sampleped[sampleped$family == 1, ]
    colnames(df1)
    ped1 <- pedigree(df1, cols_ren_ped = list(
        "indId" = "id",
        "fatherId" = "dadid",
        "motherId" = "momid",
        "gender" = "sex",
        "available" = "avail"
    ))
    ped1$ped
    expect_equal(nrow(ped1@ped), 41)
    expect_equal(ncol(ped1@ped), 15)
    expect_equal(nrow(ped1@rel), 0)
    expect_equal(ncol(ped1@rel), 7)
    expect_snapshot(summary(ped1))

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

test_that("pedigree subscripting", {
    data(minnbreast)
    minnped <- pedigree(minnbreast, cols_ren_ped = list(
        "indId" = "id", "fatherId" = "fatherid",
        "motherId" = "motherid", "gender" = "sex", "family" = "famid"
    ))
    expect_equal(nrow(minnped$ped), 28081)
    expect_equal(ncol(minnped[["ped"]]), 24)

    ped8 <- minnped[minnped$ped$family == "8",
        c("id", "dadid", "momid", "sex", "cancer")
    ]

    expect_equal(nrow(ped8$ped), 40)
    expect_equal(ncol(ped8$ped), 9)

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

    pedcol <- minnped[, c("id", "dadid", "momid", "sex", "cancer")]
    expect_equal(nrow(pedcol$ped), 28081)
    expect_equal(ncol(pedcol$ped), 9)

    pedrow <- minnped[c("8_150", "8_163", "8_145", "8_135", "8_136")]
    expect_equal(nrow(pedrow$ped), 5)
    expect_equal(ncol(pedrow$ped), 24)
})

test_that("pedigree to dataframe", {
    data(sampleped)
    ped <- pedigree(sampleped)
    expect_equal(dim(as.data.frame(ped)), c(55, 15))
})
