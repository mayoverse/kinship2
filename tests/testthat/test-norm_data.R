testthat("Norm dfa", {
    df <- c(
    1, 3, 4, 2, FALSE, NA, "1",
    2, 0, 0, 1, TRUE, 1, 1,
    3, 8, 7, "man", FALSE, NA, "2",
    4, 6, 5, "woman", TRUE, "A", 1,
    5, 0, 0, "f", FALSE, NA, 1,
    6, "None", 0, "m", FALSE, NA, "1",
    7, 0, "0", 1, FALSE, "NA", 1,
    8, 0, 0, 1, TRUE, "None", "3",
    8, 0, 0, 2, TRUE, "None", "3",
    9, 9, 1, 3, FALSE, "Ab", "A"
    )
    df <- matrix(df, ncol = 7, byrow = TRUE)
    dimnames(df) <- list(NULL, c("IndID", "FatherID", "MotherID",
        "Gender", "Sterilisation", "Availability", "NumOther"))
    df <- data.frame(df)
    summary(df)
    list_get <- norm_data(df, na_strings = c("None", "0", "NA"))
    expect_equal(list_get$norm$IndID, c("1", "2", "5", "6"))
    expect_equal(list_get$errors$IndID, c("3", "4", "7", "8", "8", "9"))
})
