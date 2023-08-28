test_that("unavailable detection works", {
    data("sampleped")
    id <- sampleped$id
    dadid <- sampleped$dadid
    momid <- sampleped$momid
    avail <- sampleped$avail
    ped <- pedigree(sampleped)
    expect_equal(find_unavailable(ped),
        c("1_101", "1_102", "1_107", "1_108", "1_113")
    )

})