test_that("check_columns", {
    df <- data.frame(
        ColN1 = c(1, 2), ColN2 = 4,
        ColU1 = "B", ColU2 = "1",
        ColTU1 = "A", ColTU2 = 3,
        ColNR1 = 4, ColNR2 = 5
    )
    # Test with cols_used_del = FALSE
    expect_error(suppressWarnings(check_columns(
        df, c("ColN1", "ColN2"), c("ColU1", "ColU2"),
        c("ColTU1", "ColTU2", "ColTU3")
    )))
    df_result <- data.frame(
        ColN1 = c(1, 2), ColN2 = 4, ColTU1 = "A", ColTU2 = 3
    )
    df_get <- suppressWarnings(check_columns(
        df, c("ColN1", "ColN2"), c("ColU1", "ColU2"),
        c("ColTU1", "ColTU2"), others_cols = FALSE, cols_used_del = TRUE
    ))
    expect_equal(df_get, df_result)

    df <- data.frame(
        ColN1 = c(1, 2), ColN2 = 4, ColTU1 = "A",
        ColTU2 = 3, ColNR1 = 4, ColNR2 = 5
    )
    # Test with others_cols = TRUE
    df_get <- suppressWarnings(check_columns(
        df, c("ColN1", "ColN2"), c("ColU1", "ColU2"),
        c("ColTU1", "ColTU2"), others_cols = TRUE
    ))
    df_result <- data.frame(
        ColN1 = c(1, 2), ColN2 = 4,
        ColTU1 = "A", ColTU2 = 3, ColNR1 = 4, ColNR2 = 5
    )
    expect_equal(df_get, df_result)

    # Test with cols_used_init = TRUE
    df_get <- suppressWarnings(check_columns(
        df, c("ColN1", "ColN2"), c("ColU1", "ColU2"),
        c("ColTU1", "ColTU2"), cols_used_init = TRUE
    ))
    df_result <- data.frame(
        ColN1 = c(1, 2), ColN2 = 4,
        ColTU1 = "A", ColTU2 = 3, ColU1 = NA, ColU2 = NA
    )
    expect_equal(df_get, df_result)

    # Test with cols_to_use_init = TRUE
    df_get <- suppressWarnings(check_columns(
        df, c("ColN1", "ColN2"), c("ColU1", "ColU2"),
        c("ColTU1", "ColTU2", "ColTU3"), cols_to_use_init = TRUE
    ))
    df_result <- data.frame(
        ColN1 = c(1, 2), ColN2 = 4,
        ColTU1 = "A", ColTU2 = 3, ColTU3 = NA
    )
    expect_equal(df_get, df_result)
})

test_that("check_num_na", {
    var <- c(45, "NA", "Test", "46.2", -2, "-46", "2NA")
    get_b_na <- check_num_na(var)
    expect_equal(get_b_na, c(TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, FALSE))
    get_b <- check_num_na(var, na_as_num = FALSE)
    expect_equal(get_b, c(TRUE, FALSE, FALSE, TRUE, TRUE, TRUE, FALSE))
})
