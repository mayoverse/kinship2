test_that("descendants works", {
    data("sampleped")
    idlist <- c("1_101", "2_208")

    desc_char <- with(sampleped, descendants(c("101", "208"), id, dadid, momid))

    pedi <- Pedigree(sampleped)
    desc <- descendants(idlist, pedi)
    expect_equal(desc, c(
        "1_109", "2_212", "2_213", "2_214",
        "1_121", "1_122", "1_123", "1_124"
    ))
    ped <- with(sampleped, Ped(id, sex, dadid, momid, famid, missid = "0"))
    idlist <- c("101", "208")
    expect_equal(descendants(idlist, ped), desc_char)
})
