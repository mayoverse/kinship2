ped_df_r <- c(
    1, 3, 4, 2, TRUE, 0, "1", "None",
    2, 0, 0, 1, TRUE, 1, 2, "A",
    3, 8, 7, "man", FALSE, NA, "2", "E",
    4, 6, 5, "woman", FALSE, "A", 3, "A",
    5, 0, 0, "f", FALSE, NA, 7, "E",
    6, "None", 0, "m", TRUE, 0, "NA", "D",
    7, 0, "0", 1, FALSE, "NA", 6, "A",
    8, 0, 0, 1, FALSE, "None", "3", "D",
    8, 2, 0, 2, FALSE, "0", "3", "A",
    9, 9, 8, 3, FALSE, "Ab", "5", "B"
)
ped_df_r <- matrix(ped_df_r, ncol = 8, byrow = TRUE)
dimnames(ped_df_r) <- list(NULL, c(
    "indId", "fatherId", "motherId", "gender",
    "steril", "available", "NumOther", "AffMod"
))

ped_df_r <- data.frame(ped_df_r)
ped_df <- suppressWarnings(norm_ped(ped_df_r, na_strings = c("None", "NA")))
ped_df$NumOther <- as.numeric(ped_df$NumOther)
values <- ped_df$NumOther
aff_num <- generate_aff_inds(ped_df$NumOther,
    threshold = 4, sup_thres_aff = TRUE
)
aff_fact <- generate_aff_inds(ped_df$AffMod, mods_aff = c("D", "E"))

test_that("generate aff inds works", {
    expect_equal(aff_num$mods, c(0, 0, 0, 0, 1, NA, 1, 0, 0, 1))
    expect_equal(levels(as.factor(aff_num$labels)),
        c("Affected > to 4", "Healthy <= to 4")
    )
    expect_equal(aff_fact$mods, c(NA, 0, 1, 0, 1, 1, 0, 1, 0, 0))
    expect_equal(levels(as.factor(aff_fact$labels)),
        c("Affected are D/E", "Healthy are A/B")
    )
})

test_that("generate border works", {
    border <- generate_border(ped_df$avail)
    expect_equal(border$mods, c(NA, 1, 0))
    expect_equal(border$border, c("grey", "green", "black"))
    expect_equal(border$labels, c("NA", "Available", "Non Available"))
})

test_that("generate fill full scale off", {
    list_num <- generate_fill(ped_df$NumOther, aff_num$affected,
        aff_num$labels, keep_full_scale = FALSE
    )
    list_fact <- generate_fill(ped_df$AffMod, aff_fact$affected,
        aff_fact$labels, keep_full_scale = FALSE
    )
    expect_equal(list_num$mods, aff_num$mods)
    expect_equal(list_num$fill_scale$fill, c("white", "red", "grey"))
    expect_equal(list_fact$mods, aff_fact$mods)
    expect_equal(list_fact$fill_scale$fill, c("grey", "white", "red"))
})

test_that("generate fill full scale on", {
    list_num <- generate_fill(ped_df$NumOther, aff_num$affected,
        aff_num$labels, keep_full_scale = TRUE
    )
    list_fact <- generate_fill(ped_df$AffMod, aff_fact$affected,
        aff_fact$labels, keep_full_scale = TRUE
    )

    expect_equal(
        list_num$fill_scale$fill, c("#FFFFFF", "#9AB1C4", "#36648B",
            "#FF0000", "grey", "#F67700", "#EEEE00"
        )
    )
    expect_equal(
        list_fact$fill_scale$fill,
        c("grey", "#FFFFFF", "#FF0000", "#EEEE00", "#36648B")
    )

    aff_num_notsup <- generate_aff_inds(ped_df$NumOther,
        threshold = 4, sup_thres_aff = FALSE
    )
    list_num_rev <- generate_fill(ped_df$NumOther, aff_num_notsup$affected,
        aff_num_notsup$labels, keep_full_scale = TRUE
    )
    expect_equal(list_num_rev$fill_scale$fill, c(
        "#FF0000", "#F67700", "#EEEE00", "#FFFFFF",
        "grey", "#9AB1C4", "#36648B"
    ))
})

test_that("generate colors works on df", {
    list_num <- generate_colors(ped_df, "NumOther",
        threshold = 4, sup_thres_aff = TRUE
    )
    expect_equal(list_num$df$NumOther_aff, aff_num$mods)
    expect_equal(list_num$scale$fill$fill, c("white", "red", "grey"))
    expect_equal(list_num$scale$fill$labels,
        c("Healthy <= to 4", "Affected > to 4", NA)
    )
    expect_equal(list_num$scale$fill$mods,  c(0, 1, NA))
    expect_equal(list_num$scale$fill$affected,  c("FALSE", "TRUE", NA))
})

test_that("generate colors works on pedigree object", {
    data("sampleped")
    ped <- pedigree(sampleped[sampleped$family == "1", -1])
    ped$ped$id <- as.numeric(ped$ped$id)
    ped_aff <- generate_colors(ped, col_aff = "id",
        threshold = 120, sup_thres_aff = TRUE, add_to_scale = FALSE
    )
    expect_equal(ped_aff$ped$id_aff, c(rep(0, 20), rep(1, 21)))
    expect_equal(ped_aff$scales$fill$fill, c("white", "red"))
    expect_equal(ped_aff$scales$fill$labels,
        c("Healthy <= to 120", "Affected > to 120")
    )
    expect_equal(ped_aff$scales$fill$mods,  c(0, 1))
    expect_equal(ped_aff$scales$fill$affected,  c("FALSE", "TRUE"))
})
TRUE