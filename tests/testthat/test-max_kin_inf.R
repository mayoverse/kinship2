test_that("min_dist_inf works", {
    data("sampleped")
    colnames(sampleped) <- c("ped", "id", "dadid", "momid",
        "sex", "affected", "avail"
    )
    sampleped[c("id", "dadid", "momid")] <- as.data.frame(lapply(
        sampleped[c("id", "dadid", "momid")], as.character
    ), stringsAsFactors = FALSE)
    summary(sampleped)

    res <- with(sampleped, min_dist_inf(id, informative = "AvAf",
        dadid, momid, sex, avail, affected
    ))
    expect_equal(sum(res, na.rm = TRUE), 97)
    mxkin <- with(sampleped, min_dist_inf(id, informative = "Av",
        dadid, momid, sex, avail, affected
    ))
    expect_equal(sum(mxkin, na.rm = TRUE), 90)
    mxkin <- with(sampleped, min_dist_inf(id, informative = "AvOrAf",
        dadid, momid, sex, avail, affected
    ))
    expect_equal(sum(mxkin, na.rm = TRUE), 76)
})

test_that("min_dist_inf works with Pedigree", {
    data("sampleped")
    ped <- Pedigree(sampleped)
    ped <- generate_colors(ped, col_aff = "affection",
        threshold = 0.5, sup_thres_aff = TRUE
    )

    mxkin <- min_dist_inf(ped, col_aff = "affection_aff", informative = "Av")
    expect_s4_class(mxkin, "Pedigree")
    expect_equal(sum(mxkin$ped$kin, na.rm = TRUE), 90)
})
TRUE
