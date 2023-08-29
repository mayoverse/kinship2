test_that("descendants works", {
    data("sampleped")
    idlist <- c("1_101", "2_208")
    ped <- pedigree(sampleped)
    desc <- descendants(idlist, ped)
    expect_equal(desc, c(
        "1_109", "2_212", "2_213", "2_214",
        "1_121", "1_122", "1_123", "1_124"
    ))
})
