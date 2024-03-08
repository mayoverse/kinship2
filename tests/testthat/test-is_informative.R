test_that("is_informative works", {
    data("sampleped")

    # Test for character
    id <- as.character(sampleped$id)
    avail <- sampleped$avail
    affected <- sampleped$affection

    expect_equal(is_informative(id, avail, affected),
        c(
            "110", "116", "118", "119", "124", "127",
            "128", "201", "203", "206", "207", "214"
        )
    )
    expect_equal(
        length(is_informative(id, avail, affected, informative = "AvOrAf")),
        35
    )
    expect_equal(
        length(is_informative(id, avail, affected, informative = "Av")),
        24
    )
    expect_equal(
        length(is_informative(id, avail, affected, informative = "Af")),
        23
    )
    expect_equal(
        is_informative(
            id, avail, affected,
            informative = c("1", "110", "150", "214")
        ),
        c("110", "214")
    )
    length(id)
    expect_equal(
        is_informative(
            id, avail, affected,
            informative = c(TRUE, FALSE, TRUE, rep(FALSE, 52))
        ),
        c("101", "103")
    )
    expect_equal(
        length(is_informative(id, avail, affected, informative = "All")),
        55
    )
})

test_that("is_informative works with Pedigree", {
    data("sampleped")

    ped <- Pedigree(sampleped[1:7])
    ped <- generate_colors(ped, col_aff = "affection",
        threshold = 0.5, sup_thres_aff = TRUE,
        add_to_scale = FALSE
    )


    ped_upd <- is_informative(ped, col_aff = "affection_mods",
        informative = "AvAf"
    )

    expect_equal(
        id(ped(ped_upd))[isinf(ped(ped_upd)) == TRUE],
        c(
            "1_110", "1_116", "1_118", "1_119", "1_124", "1_127",
            "1_128", "2_201", "2_203", "2_206", "2_207", "2_214"
        )
    )
    ped <- Pedigree(sampleped[c(2:5, 7)])
    expect_snapshot_error(is_informative(
        ped, col_aff = "test", informative = "AvAf"
    ))


    ped <- generate_colors(ped,
        col_aff = "sex", mods_aff = "male", add_to_scale = FALSE
    )
    expect_equal(
        sum(isinf(ped(is_informative(
            ped, col_aff = "sex_mods", informative = "Af"
        )))),
        length(ped(ped, "id")[ped(ped, "sex") == "male"])
    )

    data(minnbreast)
    ped <- Pedigree(minnbreast, cols_ren_ped = list(
        "indId" = "id",
        "fatherId" = "fatherid",
        "motherId" = "motherid",
        "gender" = "sex",
        "family" = "famid"
    ), missid = "0")
    ped <- generate_colors(ped, col_aff = "education",
        threshold = 3, sup_thres_aff = TRUE, keep_full_scale = TRUE,
        add_to_scale = FALSE
    )
    expect_equal(
        sum(isinf(ped(is_informative(
            ped, col_aff = "education_mods", informative = "Af"
        )))),
        sum(minnbreast$education > 3, na.rm = TRUE)
    )
})
