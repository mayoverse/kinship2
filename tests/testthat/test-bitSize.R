test_that("bit_size works", {
    data(minnbreast)
    minnped <- pedigree(minnbreast, cols_ren_ped = list(
        "indId" = "id", "fatherId" = "fatherid",
        "motherId" = "motherid", "gender" = "sex", "family" = "famid"
    ))
    bs_ped <- bit_size(minnped, missid = "0")
    bs_char <- bit_size(as.character(minnbreast$fatherid),
        as.character(minnbreast$motherid), missid = "0"
    )
    expect_equal(bs_ped, bs_char)
})
