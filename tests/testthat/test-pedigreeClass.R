test_that("Pedigree works", {
    ped <- Pedigree(data.frame(
        id = character(),
        dadid = character(),
        momid = character(),
        sex = numeric(),
        famid = character(),
        avail = numeric(),
        affection = numeric()
    ))
    expect_s4_class(ped, "Pedigree")
    expect_equal(length(ped@ped), 0)
    expect_equal(length(ped@rel), 0)
    expect_equal(dim(fill(ped)), c(0, 9))
    expect_equal(dim(border(ped)), c(0, 5))
    expect_equal(dim(spouse(ped)), c(0, 3))
    expect_equal(length(horder(ped)), 0)
})

test_that("Pedigree old usage compatibility", {
    data(sampleped)
    ped1 <- with(sampleped,
        Pedigree(id, dadid, momid, sex, famid, avail, affection)
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
    names(ped2df) <- c("famid", "id", "dadid", "momid", "sex")
    ped2df$id <- as.integer(ped2df$id)
    ## 1 2  3 4 5 6 7 8 9 10,11,12,13,14,15,16
    ped2df$disease <- c(NA, NA, 1, 0, 0, 0, 0, 1, 1, 1)
    ped2df$smoker <- c(0, NA, 0, 0, 1, 1, 1, 0, 0, 0)
    ped2df$available <- c(0, 0, 1, 1, 0, 1, 1, 1, 1, 1)
    ped2df$status <- c(1, 1, 1, 0, 1, 0, 0, 0, 0, 0)

    ## With vectors
    ped2 <- with(ped2df, Pedigree(id, dadid, momid, sex, famid,
        available, status, affected = cbind(disease, smoker, available),
        rel_df = matrix(c(8, 9, 1, 1), ncol = 4), missid = "0"
    ))

    ## With dataframes
    rel_df <- data.frame(id1 = 8, id2 = 9, code = 1, famid = 1)
    expect_equal(ped2,
        Pedigree(ped2df, col_aff = c("disease", "smoker", "available"),
            rel_df, missid = "0"
        )
    )
})

test_that("Pedigree from sampleped and affectation", {
    # Here is a case where the levels fail to line up properly
    data("sampleped")
    df1 <- sampleped[sampleped$famid == 1, ]
    ped1 <- Pedigree(df1, cols_ren_ped = list(
        "indId" = "id",
        "fatherId" = "dadid",
        "motherId" = "momid",
        "gender" = "sex",
        "available" = "avail",
        "affection" = "affected",
        "family" = "famid"
    ))

    expect_equal(dim(as.data.frame(ped(ped1))), c(41, 27))
    expect_equal(dim(as.data.frame(rel(ped1))), c(0, 4))

    expect_error(id(ped(ped1)) <- "1")
    expect_error(id(ped(ped1))[1] <- "1")
    expect_error(id(ped(ped1))[1] <- "102")
    expect_no_error(id(ped(ped1))[41] <- "142")
    expect_equal(id(ped(ped1))[41], "142")
    expect_no_error(dadid(ped(ped1))[3] <- "1_103")
    expect_warning(expect_error(sex(ped(ped1))[3] <- "103"))
    expect_error(sex(ped(ped1))[3] <- "female")
    expect_error(sex(ped(ped1))[3] <- "unknown")
    expect_no_error(sex(ped(ped1))[41] <- "male")
})

test_that("Pedigree subscripting", {
    data(minnbreast)
    minnped <- Pedigree(minnbreast, cols_ren_ped = list(
        "indId" = "id", "fatherId" = "fatherid",
        "motherId" = "motherid", "gender" = "sex", "family" = "famid",
        "affection" = "cancer"
    ), missid = "0")
    expect_equal(length(minnped), 28081)
    expect_equal(dim(as.data.frame(ped(minnped))), c(28081, 36))

    ped8 <- minnped[famid(ped(minnped)) == "8"]

    expect_equal(dim(as.data.frame(ped(ped8))), c(40, 36))

    # Subjects 150, 152, 154, 158 are children,
    # and 143, 162, 149 are parents and a child
    droplist <- paste("8", c(150, 152, 154, 158, 143, 162, 149), sep = "_")

    keep1 <- !(id(ped(ped8)) %in% droplist)  # logical
    keep2 <- which(keep1)  # numeric
    keep3 <- as.character(id(ped(ped8))[keep1])  # character
    keep4 <- factor(keep3)

    test1 <- ped8[keep1]
    test2 <- ped8[keep2]
    test3 <- ped8[keep3]
    test4 <- ped8[keep4]

    expect_equal(test1, test2)
    expect_equal(test1, test3)
    expect_equal(test1, test4)


    pedrow <- minnped[c("8_150", "8_163", "8_145", "8_135", "8_136")]
    expect_equal(length(pedrow), 5)
})

test_that("Pedigree generic", {
    data("sampleped")
    pedi <- Pedigree(sampleped)
    expect_equal(dim(as.data.frame(ped(pedi))), c(55, 27))
    expect_equal(names(as.list(pedi)), c("ped", "rel", "scales", "hints"))
    expect_equal(length(pedi), 55)
})

test_that("Pedigree accessors", {
    data("sampleped")
    pedi <- Pedigree(sampleped)
    expect_equal(pedi@ped, ped(pedi))
    expect_equal(pedi@rel, rel(pedi))
    expect_equal(pedi@hints, hints(pedi))
    expect_equal(pedi@hints@horder, horder(pedi))
    expect_equal(pedi@hints@spouse, spouse(pedi))
    expect_equal(pedi@scales@fill, fill(pedi))
    expect_equal(pedi@scales@border, border(pedi))
})
