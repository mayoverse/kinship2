test_that("unavailable detection works", {
    data("sampleped")
    ped <- Pedigree(sampleped)
    expect_equal(find_unavailable(ped),
        c(paste("1", c(
            101, 102, 107, 108, 111, 113, 121, 122, 123, 131, 132, 134, 139
        ), sep = "_"), paste("2", c(205, 210, 213), sep = "_"))
    )
    affected(ped(ped))[25] <- NA
    expect_equal(as.vector(find_avail_affected(ped)$id_trimmed), "1_125")
    expect_equal(find_avail_noninform(ped),
        c(paste("1", c(
            101, 102, 107, 108, 111, 113, 121, 122, 123, 131, 132, 134, 139
        ), sep = "_"), paste("2", c(205, 210, 213), sep = "_"))
    )
})

test_that("Unrelated detection works", {
    data("sampleped")

    ped <- Pedigree(sampleped)

    ped1 <- ped[famid(ped) == 1]
    ped2 <- ped[famid(ped) == 2]

    set.seed(10)
    expect_equal(unrelated(ped1),
        c("1_109", "1_113", "1_133", "1_141")
    )
    set.seed(10)
    expect_equal(unrelated(ped2), c("2_203", "2_206"))
})
