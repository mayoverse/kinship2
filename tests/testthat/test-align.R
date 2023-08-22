test_that("align.pedigree works", {
    data("sampleped")
    ped <- pedigree(sampleped)
    withr::local_options(width = 50)
    expect_snapshot(align.pedigree(ped))
    align <- align.pedigree(ped)
    expect_equal(align$n, c(8, 19, 22, 8))
})

test_that("test autohint works", {
    data("sampleped")
    ped <- pedigree(sampleped)
    newhint <- autohint(ped)  #this fixes up marriages and such
    check_hints(newhint, ped$ped$sex)
    plist <- align.pedigree(ped, packed = TRUE,
        align = TRUE, width = 8, hints = newhint
    )
    expect_snapshot(plist)
})
TRUE
