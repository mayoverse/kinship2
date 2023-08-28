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

test_that("Unrelated detection works", {
    data(sampleped)

    ped <- pedigree(sampleped)

    ped1 <- ped[ped$ped$family == 1, ]
    ped2 <- ped[ped$ped$family == 2, ]

    set.seed(10)
    expect_equal(unrelated(ped1),
        c("1_109", "1_113", "1_133", "1_141")
    )
    set.seed(10)
    expect_equal(unrelated(ped2), c("2_203", "2_206"))
})
