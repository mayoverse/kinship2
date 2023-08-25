test_that("pedigree fails to line up", {
    # Here is a case where the levels fail to line up properly
    data(sample.ped)
    df1 <- sample.ped[sample.ped$ped == 1, ]
    ped1 <- with(df1, pedigree(id, father, mother, sex, affected))
    expect_doppelganger("ped1", plot(ped1))

    # With reordering it's better
    df1reord <- df1[c(35:41, 1:34), ]
    ped1reord <- with(df1reord, pedigree(id, father, mother, sex, affected = affected))
    expect_doppelganger("ped1reorder", plot(ped1reord))
})

TRUE
