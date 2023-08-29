test_that("Pedigree unrelated", {
    data(sampleped)

    pedAll <- pedigree(sampleped$id, sampleped$father, sampleped$mother, sampleped$sex,
        famid = sampleped$ped, affected = cbind(sampleped$affected, sampleped$avail))

    ped1 <- pedAll["1"]
    ped2 <- pedAll["2"]

    ## to see plot:
    expect_doppelganger("Pedigree unrelated 1", plot.pedigree(ped1, align = FALSE))
    set.seed(10)
    expect_equal(pedigree.unrelated(ped1, avail = ped1$affected[, 2]), c("109", "113",
        "133", "141"))

    set.seed(10)
    expect_equal(pedigree.unrelated(ped2, avail = ped2$affected[, 2]), c("203", "206"))
})
TRUE
