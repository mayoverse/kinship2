setClass("testClass", representation(x = "data.frame", y = "list"))
obj <- new("testClass",
    x = data.frame(A = 1:10, B = LETTERS[1:10]),
    y = list(B = 11:20)
)
setMethod("as.list", c(x = "testClass"), function(x, ...) {
    list(x = x@x, y = x@y)
})
lst <- list(X = data.frame(a = 1:10), Y = 11:20)

test_that("check_slot_fd works", {
    expect_error(check_slot_fd(obj, "C", "A"))
    expect_equal(
        check_slot_fd(obj, "x", c("B", "C", "D")),
        "'C', 'D' column(s) is not present in slot x."
    )
    expect_equal(check_slot_fd(lst, "X", "a"), NULL)
})

test_that("check_values works", {
    expect_equal(
        check_values(obj@y$B, 1:15),
        paste0("Values '16', '17', '18', '19', '20' ",
            "should be in '1', '2', '3', '4', '5'..."
        )
    )
    expect_equal(check_values(obj@x$A, 1:15), NULL)
    expect_error(check_values(obj@x[c("B", "A")], 1:15))
    expect_equal(
        check_values(obj@x$B, 1:2),
        "Values 'A', 'B', 'C', 'D', 'E'... should be in '1', '2'"
    )
    expect_equal(
        check_values(obj@x$A, 1:5),
        "Values '6', '7', '8', '9', '10' should be in '1', '2', '3', '4', '5'"
    )

    ## test present = FALSE
    expect_equal(
        check_values(obj@x$A, 1:15, present = FALSE),
        paste0("Values '1', '2', '3', '4', '5'... ",
            "should not be in '1', '2', '3', '4', '5'..."
        )
    )
})

test_that("paste0max works", {
    expect_equal(paste0max(1:10), "'1', '2', '3', '4', '5'...")
    expect_equal(paste0max(1:3), "'1', '2', '3'")
})
