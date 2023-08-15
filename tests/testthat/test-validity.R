setClass("testClass", representation(x = "data.frame", y = "numeric"))
obj <- new("testClass", x = data.frame(A = 1:10, B = LETTERS[1:10]), y = 11:20)

test_that("checkColnamesSlot works", {
    expect_equal(checkColnamesSlot(obj, "y", "A"), "Missing columns in y slot. See pedigree documentation.")
    expect_equal(checkColnamesSlot(obj, "x", "A"), NULL)
    expect_equal(checkColnamesSlot(obj, "x", c("B", "C", "D")), "C, D column(s) is not present in slot x.")
})

test_that("checkValuesSlot works", {
    expect_equal(checkValuesSlot(obj, "y", "A", 1:15), "Missing columns in y slot. See pedigree documentation.")
    expect_equal(checkValuesSlot(obj, "x", "A", 1:15), NULL)
    expect_error(checkValuesSlot(obj, "x", c("B", "A"), 1:15))
    expect_equal(checkValuesSlot(obj, "x", "B", 1:2), "Values A, B, C, D, E ... in column B of slot x should be in 1, 2.")
    expect_equal(checkValuesSlot(obj, "x", "A", 1:5), "Values 6, 7, 8, 9, 10 in column A of slot x should be in 1, 2, 3, 4, 5.")
    expect_equal(checkValuesSlot(obj, "x", "A", 1:15), NULL)

    ## test present = FALSE
    expect_equal(checkValuesSlot(obj, "x", "A", 1:15, present = FALSE), "Values 1, 2, 3, 4, 5 ... in column A of slot x should not be in 1, 2, 3, 4, 5 ....")
})

test_that("paste0max works", {
    expect_equal(paste0max(1:10), "1, 2, 3, 4, 5 ...")
    expect_equal(paste0max(1:3), "1, 2, 3")
})