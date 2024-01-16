## example data and test steps from Pedigree.shrink
## Jason Sinnwell

test_that("Pedigree shrink works", {
    data(minnbreast)
    ped_mb <- Pedigree(minnbreast,
        cols_ren_ped = list(fatherId = "fatherid", motherId = "motherid",
            indId = "id", gender = "sex", family = "famid"
        ), missid = "0"
    )
    ped_mb <- generate_colors(ped_mb, col_aff = "cancer", add_to_scale = FALSE)
    mn2 <- ped_mb[famid(ped_mb) == "5"]


    ## this Pedigree as one person with cancer. The Pedigree is not informative
    ## if they are the only available, so Pedigree.shrink trims all.
    ## This caused an error in Pedigree.shrink before kinship2. v1.2.8.
    ## Now fixed

    vdiffr::expect_doppelganger("Pedigree shrink 1",
        function() plot(mn2)
    )

    ## breaks in trim
    avail(ped(mn2)) <- ifelse(is.na(mcols(mn2)$cancer), 0, mcols(mn2)$cancer)

    find_unavailable(ped(mn2))
    mn2_s <- shrink(mn2)

    expect_equal(mn2_s$id_lst$unavail,
        paste("5", c(
            44, 45, 46, 47, 48, 49, 50, 51, 52,
            53, 55, 56, 57, 58, 59, 60, 61,
            62, 63, 64, 65, 66, 67, 68, 69, 70, 71,
            72, 73, 74, 75, 76, 77, 78, 79, 26050, 26051
        ), sep = "_")
    )

    mn8 <- ped_mb[famid(ped_mb) == "8"]
    vdiffr::expect_doppelganger("Pedigree shrink 2",
        function() plot(mn8)
    )

    avail <- ifelse(is.na(mcols(mn8)$cancer), 0, mcols(mn8)$cancer)

    mn8_s <- shrink(mn8, avail)

    expect_equal(mn8_s$id_lst$unavail,
        paste("8", c(
            137, 138, 139, 140, 144, 145, 146,
            147, 148, 150, 151, 152, 153, 154,
            155, 156, 157, 158, 159, 160, 163,
            164, 165, 166, 167, 168, 169, 170,
            171, 172, 173, 174
        ), sep = "_")
    )
})

test_that("Pedigree shrink error if missing info", {
    ## use sampleped from the package
    data("sampleped")
    ped <- Pedigree(sampleped)
    ped2 <- ped[famid(ped) == "2"]
    sex(ped(ped2))[c(13, 12)] <- c("unknown", "terminated")

    ## set 2nd col of affected to NA
    expect_no_error(shrink(ped2, max_bits = 32))
    avail(ped(ped2))[c(7, 9)] <- NA
    expect_error(shrink(ped2, max_bits = 32))
})

test_that("Pedigree shrink avail test", {
    ## use sampleped from the package
    data("sampleped")
    ped <- Pedigree(sampleped)
    ped1 <- ped[famid(ped) == "1"]

    set.seed(10)
    ped1_s_av_32 <- shrink(ped1, max_bits = 32)

    set.seed(10)
    ped1_s_av_25 <- shrink(ped1, max_bits = 25)

    expect_equal(ped1_s_av_32$id_trim,
        paste("1", c(
            101, 102, 107, 108, 111, 113, 121,
            122, 123, 131, 132, 134, 139
        ), sep = "_")
    )

    expect_equal(ped1_s_av_25$id_trim,
        paste("1", c(
            101, 102, 107, 108, 111, 113, 121,
            122, 123, 131, 132, 134, 139, 140, 141
        ), sep = "_")
    )
})

test_that("Pedigree shrink with character", {
    ## use sampleped from the package
    data("sampleped")
    sampleped$famid[sampleped$famid == 1] <- "A"
    ped <- Pedigree(sampleped)
    ped1 <- ped[famid(ped) == "A"]

    set.seed(100)
    ped1_s_av_32 <- shrink(ped1, max_bits = 32)
    expect_equal(ped1_s_av_32$id_trim, c(
        "A_101", "A_102", "A_107", "A_108",
        "A_111", "A_113", "A_121", "A_122",
        "A_123", "A_131", "A_132", "A_134", "A_139"
    ))

    set.seed(100)
    ped1_s_av_25 <- shrink(ped1, max_bits = 25)
    expect_equal(ped1_s_av_25$id_trim, c(
        "A_101", "A_102", "A_107", "A_108",
        "A_111", "A_113", "A_121", "A_122",
        "A_123", "A_131", "A_132", "A_134",
        "A_139", "A_133", "A_141"
    ))
})

test_that("Shrink works", {
    data("sampleped")
    ped <- Pedigree(sampleped)
    ped2 <- ped[famid(ped) == "2"]
    ped2_s <- shrink(ped2)

    vdiffr::expect_doppelganger("Whole ped",
        function() plot(ped2, title = "Whole ped")
    )
    vdiffr::expect_doppelganger("Shrinked ped",
        function() plot(ped2_s$pedObj, title = "Shrinked ped")
    )
})
