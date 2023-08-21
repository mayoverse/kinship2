test_that("makefamid works", {
    id <- as.character(1:20)
    mom <- as.character(c(0, 0, 0, 2, 2, 2, 0, 2, 0, 0, 2, 2, 0, 2, 0, 2, 7, 7, 11, 14))
    dad <- as.character(c(0, 0, 0, 1, 1, 1, 0, 1, 0, 0, 3, 3, 0, 3, 0, 3, 8, 8, 10, 13))
    famid <- c(1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1)
    temp <- makefamid(id, mom, dad)
    expect_equal(temp, famid)
})

rel_df <- c(
        213, 214, 1, 3,
        210, 211, 2, 3,
        140, 141, 3, 1,
        133, 134, 4, 1
    )

rel_df <- matrix(rel_df, ncol = 4, byrow = TRUE)
dimnames(rel_df) <- list(NULL, c("id1", "id2", "code", "family"))
rel_df <- data.frame(rel_df)

test_that("makefamid works with pedigree", {
    ## Simple case with no family id
    data(sampleped)
    ped <- pedigree(sampleped[-1], rel_df = rel_df[c(1:3)])
    ped <- makefamid(ped)

    ## Expected values
    fam <- sampleped$family
    fam[sampleped$id == "113"] <- 0 # singleton
    id <- paste(fam, c(101:141, 201:214), sep = "_")
    expect_equal(ped$ped$id, id)
    expect_equal(ped$rel$id1, c("2_213", "2_210", "1_140", "1_133"))

    ## Updating already present family id
    data(sampleped)
    sampleped$family[sampleped$family == "2"] <- 3
    ped <- pedigree(sampleped, rel_df = rel_df)
    ped <- makefamid(ped)
    expect_equal(ped$ped$id, id)
    expect_equal(ped$rel$id1, c("2_213", "2_210", "1_140", "1_133"))
})

test_that("Family check works", {
    data(sampleped)
    ped <- pedigree(sampleped)

    ## check them giving separate ped ids
    fcheck_df_sep <- with(sampleped,
        familycheck(family, as.character(id), dadid, momid)
    )
    fcheck_ped_sep <- familycheck(ped)
    expect_equal(as.numeric(as.vector(fcheck_df_sep[1, ])), c(1, 41, 1, 1, 0))
    expect_equal(as.numeric(as.vector(fcheck_ped_sep[1, ])), c(1, 41, 1, 1, 0))

    ## check assigning them same ped id
    fcheck_df_combined <- with(sampleped, familycheck(
        rep(1, nrow(sampleped)), as.character(id), dadid, momid
    ))
    sampleped$family[sampleped$family == "2"] <- 1
    ped <- pedigree(sampleped)
    fcheck_ped_combined <- familycheck(ped)
    expect_equal(as.numeric(as.vector(fcheck_df_combined[1, ])),
        c(1, 55, 1, 2, 0)
    )
    expect_equal(as.numeric(as.vector(fcheck_ped_combined[1, ])),
        c(1, 55, 1, 2, 0)
    )

    ## Correct the family id with makefamid
    ped <- makefamid(ped)
    fcheck_ped_corrected <- familycheck(ped)
    expect_equal(as.numeric(as.vector(fcheck_ped_corrected[1, ])),
        c(0, 1, 1, 0, 0)
    )
    expect_equal(as.numeric(as.vector(fcheck_ped_corrected[2, ])),
        c(1, 40, 0, 1, 0)
    )
    expect_equal(as.numeric(as.vector(fcheck_ped_corrected[3, ])),
        c(2, 14, 0, 1, 0)
    )

})
