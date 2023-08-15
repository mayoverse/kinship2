test_that("makefamid works", {
    id <- 1:20
    mom <- c(0, 0, 0, 2, 2, 2, 0, 2, 0, 0, 2, 2, 0, 2, 0, 2, 7, 7, 11, 14)
    dad <- c(0, 0, 0, 1, 1, 1, 0, 1, 0, 0, 3, 3, 0, 3, 0, 3, 8, 8, 10, 13)
    famid <- c(1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1)
    temp <- makefamid(id, mom, dad)
    expect_equal(temp, famid)
})

test_that("Family check works", {
    data(sample.ped)
    pedAll <- with(sample.ped, pedigree(id, father, mother, sex, affected = cbind(affected,
        avail), famid = ped))

    ## check them giving separate ped ids
    fcheck.sep <- with(sample.ped, familycheck(ped, id, father, mother))
    as.numeric(as.vector(fcheck.sep[1, ]))
    expect_equal(as.numeric(as.vector(fcheck.sep[1, ])), c(1, 41, 1, 1, 0))

    ## check assigning them same ped id
    fcheck.combined <- with(sample.ped, familycheck(rep(1, nrow(sample.ped)), id,
        father, mother))
    expect_equal(as.numeric(as.vector(fcheck.combined[1, ])), c(1, 55, 1, 2, 0))

})
TRUE
