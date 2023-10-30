test_that("Class ped work", {
    ped0 <- new("Ped")
    expect_s4_class(ped0, "Ped")
    expect_equal(length(ped0), 0)
    expect_equal(length(as.list(ped0)), 12)
    expect_equal(dim(as.data.frame(ped0)), c(0, 12))

    ped2 <- Ped(
        obj = c("ID5", "ID4"),
        momid = c(NA, NA),
        dadid = c(NA, NA),
        sex = factor(c("female", "male"))
    )
    expect_s4_class(ped2, "Ped")
    expect_equal(length(ped2), 2)
    expect_equal(length(as.list(ped2)), 12)
    expect_equal(dim(as.data.frame(ped2)), c(2, 12))
    expect_snapshot(ped2)

    expect_error(Ped(
        obj = c("ID1", "ID2", "ID3"),
        momid = c(NA, NA),
        dadid = c(NA, NA)
    ))
    expect_error(Ped(
        obj = c("ID1", "ID2", "ID3"),
        momid = c(NA, NA, NA),
        dadid = c(NA, NA, NA),
        sex = c(1, 2)
    ))

    expect_error(Ped(
        obj = c("ID1", "ID2", "ID3"),
        momid = c("ID3", NA, NA),
        dadid = c(NA, NA, NA),
        sex = c("male", "female", "male")
    ))
    expect_no_error(Ped(
        obj = c("ID1", "ID2", "ID3"),
        momid = c("ID2", NA, NA),
        dadid = c("ID3", NA, NA),
        sex = c("male", "female", "male")
    ))
    expect_no_error(Ped(
        obj = c("ID1", "ID2", "ID3"),
        momid = c("ID2", NA, NA),
        dadid = c("ID3", NA, NA),
        sex = c("male", "female", "male"),
        famid = c("F1", "F1", "F2")
    ))
    expect_error(Ped(
        obj = c("ID3", "ID2", ""),
        momid = c("ID2", NA, NA),
        dadid = c("ID3", NA, NA),
        sex = c("male", "female", "male"),
        famid = c("F1", "F1", "F2")
    ))

    #### Metadata ####
    ped3 <- Ped(
        obj = c("ID1", "ID2", "ID3"),
        momid = c("ID2", NA, NA),
        dadid = c("ID3", NA, NA),
        sex = c("male", "female", "male"),
        famid = c("F1", "F1", "F2")
    )

    mcols(ped3) <- list(A = c("test", 1, 3), B = c("test3", 6, 8))
    mcols(ped3)$Test <- c("test2", 3, 4)
    expect_equal(dim(mcols(ped3)), c(3, 3))

    expect_error(mcols(ped3) <- list(
        A = c("test", 1, 3, 6), B = c("test3", 6, 8, 9)
    ))
    expect_equal(length(as.list(ped3)), 15)
    expect_equal(dim(as.data.frame(ped3)), c(3, 15))

    df <- data.frame(
        id = c("ID1", "ID2", "ID3"),
        momid = c("ID2", NA, NA),
        dadid = c("ID3", NA, NA),
        sex = c("male", "female", "male"),
        famid = c("F1", "F1", "F2"),
        test = c("test", 1, 3),
        test2 = c("test2", 3, 4)
    )
    ped3 <- Ped(df)

    expect_equal(ped3[1]@id, "ID1")
    expect_equal(ped3[1:2]@id, c("ID1", "ID2"))
    famid(ped3)

    expect_equal(dim(as.data.frame(ped3)), c(3, 14))
    expect_equal(dim(mcols(ped3)), c(3, 2))

    expect_error(c(ped3, ped3))
    ped5 <- suppressWarnings(c(ped3, ped2))

    expect_equal(dim(as.data.frame(ped5)), c(5, 14))
})

test_that("Rel class works", {
    rel0 <- new("Rel")
    expect_s4_class(rel0, "Rel")
    expect_equal(length(rel0), 0)
    expect_equal(length(as.list(rel0)), 4)
    expect_equal(dim(as.data.frame(rel0)), c(0, 4))
    rel2 <- Rel(
        obj = c("ID5", "ID4"),
        id2 = c("ID3", "ID2"),
        code = c(1, 4),
    )
    mcols(rel2) <- list("A" = c(1, 2))
    expect_s4_class(rel2, "Rel")
    expect_equal(length(rel2), 2)
    expect_equal(length(as.list(rel2)), 5)
    expect_equal(dim(as.data.frame(rel2)), c(2, 5))
    expect_snapshot(rel2)

    expect_error(rel4 <- c(rel2, rel2))

    expect_error(rel3 <- Rel(
        obj = c("ID5", "ID2", "ID4"),
        id2 = c("ID3", "ID3", "ID2"),
        code = c(1, 2),
    ))

    rel3 <- Rel(
        obj = c("ID6", "ID2", "ID4"),
        id2 = c("ID3", "ID3", "ID1"),
        code = c(1, 2, 3),
    )
    expect_equal(dim(as.data.frame(c(rel3, rel2))), c(5, 5))

})
