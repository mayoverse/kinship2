test_that("unavailable detection works", {
    data("sampleped")
    id <- sampleped$id
    dadid <- sampleped$dadid
    momid <- sampleped$momid
    avail <- sampleped$avail
    ped <- pedigree(sampleped)
    plot(ped)
    expect_equal(find_unavailable(ped),
        c("1_101", "1_102", "1_107", "1_108", "1_113")
    )
    find_avail_affected(ped)
    ped$ped$affected[25] <- NA
    expect_equal(as.vector(find_avail_affected(ped)$id_trimmed), "1_125")
    expect_equal(find_avail_noninform(ped),
        c("1_101", "1_102", "1_107", "1_108")
    )
})