test_that("min_dist_inf works", {
    data("sampleped")
    colnames(sampleped) <- c("ped", "id", "dadid", "momid",
        "sex", "affected", "avail"
    )
    sampleped[c("id", "dadid", "momid")] <- as.data.frame(lapply(
        sampleped[c("id", "dadid", "momid")], as.character
    ), stringsAsFactors = FALSE)

    id_inf <- with(sampleped, is_informative(
        id, avail, affected, informative = "AvAf"
    ))

    res <- with(sampleped,
        min_dist_inf(id, dadid, momid, sex, id_inf
        )
    )
    expect_equal(sum(res, na.rm = TRUE), 97)

    id_inf <- with(sampleped, is_informative(
        id, avail, affected, informative = "Av"
    ))
    mxkin <- with(sampleped,
        min_dist_inf(id, dadid, momid, sex, id_inf)
    )
    expect_equal(sum(mxkin, na.rm = TRUE), 90)

    id_inf <- with(sampleped, is_informative(
        id, avail, affected, informative = "AvOrAf"
    ))
    mxkin <- with(sampleped,
        min_dist_inf(id, dadid, momid, sex, id_inf)
    )
    expect_equal(sum(mxkin, na.rm = TRUE), 76)
})

test_that("min_dist_inf works with Pedigree", {
    data("sampleped")
    ped <- Pedigree(sampleped)
    ped <- generate_colors(ped, col_aff = "affection",
        threshold = 0.5, sup_thres_aff = TRUE
    )
    expect_equal(sum(affected(ped(ped)), na.rm = TRUE), 23)
    mxkin <- min_dist_inf(ped, col_aff = "affection_mods", informative = "Av")
    expect_s4_class(mxkin, "Pedigree")
    expect_equal(sum(kin(ped(mxkin)), na.rm = TRUE), 90)
})
