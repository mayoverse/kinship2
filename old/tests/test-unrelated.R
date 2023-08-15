test_that("pedigree.unrelated works", {
    data(sample.ped)
    fam1 <- sample.ped[sample.ped$ped == 1, ]
    ped1 <- pedigree(fam1$id, fam1$father, fam1$mother, fam1$sex, fam1$affected,
        fam1$avail)

    set.seed(10)
    id1 <- pedigree.unrelated(ped1, avail = fam1$avail)
    expect_equal(id1, c("109", "113", "133", "141"))

    fam2 <- sample.ped[sample.ped$ped == 2, ]
    ped2 <- pedigree(fam2$id, fam2$father, fam2$mother, fam2$sex, fam2$affected,
        fam2$avail)

    set.seed(10)
    id2 <- pedigree.unrelated(ped2, avail = fam2$avail)
    expect_equal(id2, c("203", "206"))
})
TRUE
