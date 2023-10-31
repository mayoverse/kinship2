test_that("descendants works", {
    data("sampleped")
    idlist <- c("1_101", "2_208")
    pedi <- Pedigree(sampleped)
    desc <- descendants(idlist, pedi)
    expect_equal(desc, c(
        "1_109", "2_212", "2_213", "2_214",
        "1_121", "1_122", "1_123", "1_124"
    ))
    ped <- with(sampleped, Ped(id, sex, dadid, momid, family, missid = "0"))
    expect_equal(descendants(idlist, ped), c(
        "109", "212", "213", "214",
        "121", "122", "123", "124"
    ))
})
