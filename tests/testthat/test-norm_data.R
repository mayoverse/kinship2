test_that("Norm ped", {
    ped_df <- c(
        1, 3, 4, 2, TRUE, NA, "1", "None",
        2, 0, 0, 1, TRUE, 1, 2, "A",
        3, 8, 7, "man", FALSE, 0, "2", "E",
        4, 6, 5, "woman", FALSE, "A", 3, "A",
        5, 0, 0, "f", FALSE, NA, 7, "E",
        6, "None", 0, "m", TRUE, 0, "NA", "D",
        7, 0, "0", 1, FALSE, "NA", 6, "A",
        8, 0, 0, 1, FALSE, "0", "3", "D",
        8, 2, 0, 2, FALSE, "None", "3", "A",
        9, 9, 8, 3, FALSE, "Ab", "5", "B"
    )
    ped_df <- matrix(ped_df, ncol = 8, byrow = TRUE)
    dimnames(ped_df) <- list(NULL, c(
        "indId", "fatherId", "motherId", "gender",
        "sterilisation", "available", "NumOther", "AffMod"
    ))
    ped_df <- data.frame(ped_df)
    ped_df <- suppressWarnings(norm_ped(
        ped_df, na_strings = c("None", "NA")
    ))
    expect_equal(dim(ped_df), c(10, 21))
    expect_snapshot(ped_df)
    expect_equal(sum(is.na(ped_df$error)), 4)
})

test_that("Norm rel", {
    rel_df <- c(
        1, 2, 1, 1,
        1, 3, 2, 1,
        2, 3, 3, 1,
        1, 2, 4, 2,
        3, 4, "MZ twin", 2,
        6, 7, "Other", 2,
        8, "8", "spo Use", 2,
        9, "0", "4", 1,
        NA, "B", NA, 1
    )

    rel_df <- matrix(rel_df, ncol = 4, byrow = TRUE)
    dimnames(rel_df) <- list(NULL, c("id1", "id2", "code", "family"))
    rel_df <- data.frame(rel_df)

    rel_df <- norm_rel(rel_df)
    expect_equal(dim(rel_df), c(9, 5))
    expect_snapshot(rel_df)
    expect_equal(sum(is.na(rel_df$error)), 6)

    rel_df <- c(
        1, 2, 1,
        1, 3, 2,
        2, 3, 3,
        1, 2, 4,
        3, 4, "MZ twin",
        6, 7, "Other",
        8, "8", "spo Use",
        9, "0", "4"
    )
    rel_df <- matrix(rel_df, ncol = 4, byrow = TRUE)
    dimnames(rel_df) <- list(NULL, c("id1", "id2", "code", "family"))
    rel_df <- data.frame(rel_df)
    expect_snapshot(norm_rel(rel_df, missid = "0"))
})
