test_that("Class ped work", {
    ped <- new("Ped")
    expect_is(ped, "Ped")
    expect_is(ped, "Vector")
    expect_equal(length(ped), 0)
    expect_error(ped@id <- "test")
})