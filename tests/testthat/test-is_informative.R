test_that("is_informative works", {
    data("sampleped")

    # Test for character
    id <- as.character(sampleped$id)
    avail <- sampleped$available
    affected <- sampleped$affected

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
        is_informative(id, avail, affected, informative = c(1, 110, 150, 214)),
        c("110", "214")
    )
    expect_equal(
        is_informative(id, avail, affected, informative = c(TRUE, FALSE, TRUE)),
        c("101", "103")
    )
    expect_equal(
        length(is_informative(id, avail, affected, informative = "All")),
        55
    )

    sampleped$avail <- sampleped$available
    sampleped$id <- as.character(sampleped$id)
    expect_equal(with(sampleped,
            is_informative(id, avail, affected, informative = "AvAf")
        ),
        c(
            "110", "116", "118", "119", "124", "127",
            "128", "201", "203", "206", "207", "214"
        )
    )
})

test_that("is_informative works with Pedigree", {
    data("sampleped")

    ped <- Pedigree(sampleped[1:7])
    ped <- generate_colors(ped, col_aff = "affection",
        threshold = 0.5, sup_thres_aff = TRUE,
        add_to_scale = FALSE
    )


    df <- is_informative(ped, col_aff = "affection_aff",
        informative = "AvAf"
    )$ped
    expect_equal(
        df$id[df$id_inf == 1],
        c(
            "1_110", "1_116", "1_118", "1_119", "1_124", "1_127",
            "1_128", "2_201", "2_203", "2_206", "2_207", "2_214"
        )
    )
    ped <- Pedigree(sampleped[c(2:5, 7)])
    expect_error(is_informative(ped, informative = "AvAf"))
    expect_error(is_informative(ped, column = "test", informative = "AvAf"))

    ped <- generate_colors(ped,
        col_aff = "sex", mods_aff = "male", add_to_scale = FALSE
    )
    expect_equal(
        sum(is_informative(ped, col_aff = "sex_aff",
            informative = "Af"
        )$ped$id_inf),
        length(ped(ped)[ped(ped)$sex == "male", "id"])
    )

    data(minnbreast)
    summary(minnbreast)
    ped <- Pedigree(minnbreast, cols_ren_ped = list(
        "indId" = "id",
        "fatherId" = "fatherid",
        "motherId" = "motherid",
        "gender" = "sex"
    ))
    ped <- generate_colors(ped, col_aff = "education",
        threshold = 3, sup_thres_aff = TRUE, keep_full_scale = TRUE,
        add_to_scale = FALSE
    )
    expect_equal(
        sum(is_informative(ped,
                col_aff = "education_aff", informative = "Af"
            )$ped$id_inf
        ),
        sum(minnbreast$education > 3, na.rm = TRUE)
    )
})
