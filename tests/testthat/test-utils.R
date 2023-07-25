test_that("var_to_factor works", {
    set.seed(10)
    var <- runif(10)
    var_fact <- var_to_factor(var, threshold = 0.5)
    expect_snapshot(var_fact)
})

test_that("df_cont_table", {
    set.seed(10)
    var1 <- runif(10)
    var2 <- runif(10)
    df <- data.frame(var1, var2)
    df_ct2 <- df_cont_table(df, "var1", 0.5, "var2", c(0.25, 0.5, 0.75))
    df_ct1 <- df_cont_table(df, "var1", 0.5)
    expect_snapshot(df_ct1)
    expect_snapshot(df_ct2)
})

test_that("check_columns", {
    df <- data.frame(ColN1 = c(1, 2), ColN2 = 4, ColU1 = "B", ColU2 = "1", ColTU1 = "A",
        ColTU2 = 3, ColNR1 = 4, ColNR2 = 5)
    df_result <- data.frame(ColN1 = c(1, 2), ColN2 = 4, ColTU1 = "A", ColTU2 = 3)
    cols_needed <- c("ColN1", "ColN2")
    cols_used <- c("ColU1", "ColU2")
    cols_to_use <- c("ColTU1", "ColTU2")
    others_cols <- FALSE
    df_get <- suppressWarnings(check_columns(df, c("ColN1", "ColN2"), c("ColU1",
        "ColU2"), c("ColTU1", "ColTU2"), others_cols = FALSE))
    expect_equal(df_get, df_result)
})

test_that("check_num_na", {
    var <- c(45, "NA", "Test", "46.2", -2, "-46", "2NA")
    get_b_na <- check_num_na(var)
    expect_equal(get_b_na, c(T, T, F, T, T, T, F))
    get_b <- check_num_na(var, na_as_num = FALSE)
    expect_equal(get_b, c(T, F, F, T, T, T, F))
})
TRUE
