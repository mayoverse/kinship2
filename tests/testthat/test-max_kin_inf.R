test_that("max_kin_inf works", {
    data("sampleped")
    colnames(sampleped) <- c("ped", "id", "dadid", "momid",
        "sex", "affected", "avail"
    )
    sampleped[c("id", "dadid", "momid")] <- as.data.frame(lapply(
        sampleped[c("id", "dadid", "momid")], as.character
    ), stringsAsFactors = FALSE)
    summary(sampleped)

    res <- with(sampleped, max_kin_inf(id, informative = "AvAf",
        dadid, momid, sex, avail, affected
    ))
    expect_equal(sum(res, na.rm = TRUE), 97)
    expect_equal(max_kin_inf(sampleped), res)

    mxkin <- max_kin_inf(sampleped, informative = "Av")
    expect_equal(sum(mxkin, na.rm = TRUE), 90)
    mxkin <- max_kin_inf(sampleped, informative = "AvOrAf")
    expect_equal(sum(mxkin, na.rm = TRUE), 76)
})

test_that("max_kin_inf works with pedigree", {
    data("sampleped")
    ped <- pedigree(sampleped)
    ped <- generate_colors(ped, col_aff = "affected",
        threshold = 0.5, sup_thres_aff = TRUE
    )
    mxkin <- max_kin_inf(ped, column = "affected_aff", informative = "Av")
    expect_s4_class(mxkin, "Pedigree")
    expect_equal(sum(mxkin$ped$kin, na.rm = TRUE), 90)
})
TRUE
