test_that("Class ped work", {
    ped0 <- new("Ped")
    expect_s4_class(ped0, "Ped")
    expect_equal(length(ped0), 0)
    expect_equal(length(as.list(ped0)), 15)
    expect_equal(dim(as.data.frame(ped0)), c(0, 15))

    ped2 <- Ped(
        obj = c("ID5", "ID4"),
        momid = c(NA, NA),
        dadid = c(NA, NA),
        sex = factor(c("female", "male"))
    )
    expect_s4_class(ped2, "Ped")
    expect_equal(length(ped2), 2)
    expect_equal(length(as.list(ped2)), 15)
    expect_equal(dim(as.data.frame(ped2)), c(2, 15))
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
    expect_equal(length(as.list(ped3)), 18)
    expect_equal(dim(as.data.frame(ped3)), c(3, 18))

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

    expect_equal(dim(as.data.frame(ped3)), c(3, 17))
    expect_equal(dim(mcols(ped3)), c(3, 2))

    expect_error(c(ped3, ped3))
    ped5 <- suppressWarnings(c(ped3, ped2))

    expect_equal(dim(as.data.frame(ped5)), c(5, 17))

    ## Subsetting
    expect_error(subset(ped3, "ID1"))
    ped1_char <- subset(ped3, "ID1", del_parents = TRUE)
    ped1_num <- subset(ped3, 1, del_parents = TRUE)
    ped1_log <- subset(ped3, c(TRUE, FALSE, FALSE), del_parents = TRUE)

    expect_equal(ped1_char, ped1_num)
    expect_equal(ped1_char, ped1_log)
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

    expect_equal(length(subset(rel3, "ID6")), 0)
    expect_equal(length(subset(rel3, c("ID2", "ID3"))), 1)
    expect_equal(length(subset(rel3, c("ID2", "ID3", "ID6"))), 2)

})

test_that("Hints class works", {
    ## From scratch
    hts0 <- Hints()
    expect_equal(horder(hts0), numeric())
    expect_error(horder(hts0) <- c(1, 2))
    horder(hts0) <- c("ID1" = 1, "ID2" = 2)
    expect_equal(horder(hts0), c("ID1" = 1, "ID2" = 2))
    expect_equal(dim(spouse(hts0)), c(0, 3))
    expect_error(spouse(hts0) <- data.frame(
        idl = c("ID1", "ID2"),
        idr = c("ID3", "ID4"),
        anchor = factor(c("left", "right"))
    ))
    horder(hts0) <- c("ID1" = 1, "ID2" = 2, "ID3" = 3, "ID4" = 4)
    spouse(hts0) <- data.frame(
        idl = c("ID1", "ID2"),
        idr = c("ID3", "ID4"),
        anchor = factor(c("left", "right"))
    )
    expect_equal(dim(spouse(hts0)), c(2, 3))
    expect_snapshot(hts0)

    ## With constructor
    hts2 <- Hints(
        horder = c("ID1" = 1, "ID2" = 2, "ID3" = 3, "ID4" = 4),
        spouse = data.frame(
            idl = c("ID1", "ID2"),
            idr = c("ID3", "ID4"),
            anchor = factor(c("left", "right"))
        )
    )
    expect_equal(hts0, hts2)

    ## With missing values
    expect_error(Hints(horder = c("ID1" = 1, "ID2" = NA, "ID3" = 3, "ID4" = 4)))

    expect_error(Hints(
        horder = c("ID1" = 1, "ID2" = 2, "ID3" = 3, "ID4" = 4),
        spouse = data.frame(
            idl = c("ID1", "ID2"),
            idr = c("ID3", NA),
            anchor = factor(c("left", "right"))
        )
    ))

    hts1 <- subset(hts2, "ID1")
    expect_equal(horder(hts1), c("ID1" = 1))
    expect_equal(dim(spouse(hts1)), c(0, 3))

    hts13 <- subset(hts2, c("ID1", "ID3"))
    expect_equal(horder(hts13), c("ID1" = 1, "ID3" = 3))
    expect_equal(dim(spouse(hts13)), c(1, 3))
})

test_that("Scales class works", {
    ## From scratch
    scl0 <- Scales()
    expect_equal(dim(fill(scl0)), c(0, 9))
    expect_equal(dim(border(scl0)), c(0, 5))

    expect_error(fill(scl0) <- c("ID1", "ID2"))
    expect_error(border(scl0) <- c("ID1", "ID2"))

    expect_error(fill(scl0)$column_values <- c("ID1", "ID2"))

    expect_snapshot_error(fill(scl0) <- data.frame(
        order = c("A", 3),
        column_values = c("ID1", "ID2"),
        column_mods = c(1, 2),
        mods = c("ID1", "ID2"),
        labels = c("ID1", "ID2"),
        affected = c("A", FALSE),
        fill = c("ID1", "ID2"),
        density = c(1, 2),
        angle = c("A", 60)
    ))
    expect_snapshot_error(border(scl0) <- data.frame(
        column_values = c("ID1", "ID2"),
        column_mods = c("ID1", "ID2"),
        mods = c("ID1", "ID2"),
        labels = c(1, 2),
        border = c("ID1", "ID2")
    ))


    fill(scl0) <- data.frame(
        order = c(2, 3),
        column_values = c("ID1", "ID2"),
        column_mods = c("ID1", "ID2"),
        mods = c(1, 2),
        labels = c("ID1", "ID2"),
        affected = c(TRUE, FALSE),
        fill = c("ID1", "ID2"),
        density = c(1, 2),
        angle = c(90, 60)
    )
    expect_equal(dim(fill(scl0)), c(2, 9))
    fill(scl0)$fill[1] <- "ID3"
    expect_equal(fill(scl0)$fill[1], "ID3")

    border(scl0) <- data.frame(
        column_values = c("ID1", "ID2"),
        column_mods = c("ID1", "ID2"),
        mods = c(1, 2),
        labels = c("Lab1", "Lab2"),
        border = c("ID1", "ID2")
    )

    expect_equal(dim(border(scl0)), c(2, 5))
    expect_snapshot(scl0)

    ## With constructor
    scl2 <- Scales(
        fill = data.frame(
            order = c(2, 3),
            column_values = c("ID1", "ID2"),
            column_mods = c("ID1", "ID2"),
            mods = c(1, 2),
            labels = c("ID1", "ID2"),
            affected = c(TRUE, FALSE),
            fill = c("ID3", "ID2"),
            density = c(1, 2),
            angle = c(90, 60)
        ),
        border = data.frame(
            column_values = c("ID1", "ID2"),
            column_mods = c("ID1", "ID2"),
            mods = c(1, 2),
            labels = c("Lab1", "Lab2"),
            border = c("ID1", "ID2")
        )
    )
    expect_equal(scl2, scl0)
})

test_that("Pedigree class works", {
    pedi <- Pedigree()
    expect_equal(length(pedi), 0)
    expect_equal(length(as.list(pedi)), 4)
    expect_s4_class(scales(pedi), "Scales")
    expect_s4_class(hints(pedi), "Hints")
    expect_s4_class(ped(pedi), "Ped")
    expect_s4_class(rel(pedi), "Rel")
    expect_equal(horder(pedi), numeric())
    expect_equal(dim(spouse(pedi)), c(0, 3))
    expect_equal(dim(fill(pedi)), c(0, 9))
    expect_equal(dim(border(pedi)), c(0, 5))
    expect_equal(length(ped(pedi)), 0)
    expect_equal(length(rel(pedi)), 0)
})
