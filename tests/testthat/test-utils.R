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