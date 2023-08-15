test_that("max_kin_inf works", {
    data("sample.ped")
    colnames(sample.ped) <- c("ped", "id", "dadid", "momid", "sex", "affected", "avail")
    df <- max_kin_inf(sample.ped)
    expect_equal(sum(df$kin, na.rm = TRUE), 97)
    df <- max_kin_inf(sample.ped, informative = "Av")
    expect_equal(sum(df$kin, na.rm = TRUE), 90)
    df <- max_kin_inf(sample.ped, informative = "AvOrAf")
    expect_equal(sum(df$kin, na.rm = TRUE), 76)
})
TRUE
