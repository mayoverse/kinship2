test_that("bit_size works", {
    data(minnbreast)
    minnped <- Pedigree(minnbreast, cols_ren_ped = list(
        "indId" = "id", "fatherId" = "fatherid",
        "motherId" = "motherid", "gender" = "sex", "family" = "famid"
    ))
    bs_pedi <- bit_size(minnped, missid = NA_character_)
    bs_char <- bit_size(as.character(minnbreast$fatherid),
        as.character(minnbreast$motherid), missid = NA_character_
    )

    ped <- with(minnbreast, Ped(id, sex, fatherid, motherid, missid = NA_character_))
    bs_ped <- bit_size(ped)

    expect_equal(bs_ped, bs_char)
    expect_equal(bs_pedi, bs_char)
})
