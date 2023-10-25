test_that("Class ped work", {
    ped0 <- new("Ped")
    expect_s4_class(ped0, "Ped")
    expect_s4_class(ped0, "Vector")
    expect_equal(length(ped0), 0)
    expect_equal(length(as.list(ped0)), 14)
    expect_equal(dim(as.data.frame(ped0)), c(0, 14))

    ped2 <- Ped(
        id = c("ID1", "ID2"),
        momid = c(NA, NA),
        dadid = c(NA, NA),
        sex = factor(c("female", "male"))
    )
    expect_s4_class(ped2, "Ped")
    expect_s4_class(ped2, "Vector")
    expect_equal(length(ped2), 2)
    expect_equal(length(as.list(ped2)), 14)
    expect_equal(dim(as.data.frame(ped2)), c(2, 14))
    expect_snapshot(ped2)

    expect_error(Ped(
        id = c("ID1", "ID2", "ID3"),
        momid = c(NA, NA),
        dadid = c(NA, NA)
    ))
    expect_error(Ped(
        id = c("ID1", "ID2"),
        momid = c(NA, NA, NA),
        dadid = c(NA, NA, NA)
    ))
    expect_error(Ped(
        id = c("ID1", "ID2", "ID3"),
        momid = c("ID3", NA, NA),
        dadid = c(NA, NA, NA),
        sex = c("male", "female", "male")
    ))
    expect_no_error(Ped(
        id = c("ID1", "ID2", "ID3"),
        momid = c("ID2", NA, NA),
        dadid = c("ID3", NA, NA),
        sex = c("male", "female", "male")
    ))
    expect_no_error(Ped(
        id = c("ID1", "ID2", "ID3"),
        momid = c("ID2", NA, NA),
        dadid = c("ID3", NA, NA),
        sex = c("male", "female", "male"),
        family = c("F1", "F1", "F2")
    ))
    expect_error(Ped(
        id = c("ID3", "ID2", ""),
        momid = c("ID2", NA, NA),
        dadid = c("ID3", NA, NA),
        sex = c("male", "female", "male"),
        family = c("F1", "F1", "F2")
    ))
    ped3 <- Ped(
        id = c("ID1", "ID2", "ID3"),
        momid = c("ID2", NA, NA),
        dadid = c("ID3", NA, NA),
        sex = c("male", "female", "male"),
        family = c("F1", "F1", "F2")
    )
    mcols(ped3) <- list(A = c("test", 1, 3))
    mcols(ped3)$Test <- c("test", 1, 3)
    expect_equal(dim(mcols(ped3)), c(3, 2))
    as.list(ped3)
    ped3
})