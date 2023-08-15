test_that("useful_inds works", {
    data("sample.ped")
    colnames(sample.ped) <- c("ped", "id", "dadid", "momid", "sex", "affected", "avail")
    df <- num_child(sample.ped)
    df <- useful_inds(df)
    expect_equal(df$id[!df$useful], c(101, 102, 107, 108, 113, 117))
    df <- num_child(sample.ped)
    df <- useful_inds(df, informative = "Av")
    expect_equal(df$id[!df$useful], c(101, 102, 107, 108, 117))
    df <- num_child(sample.ped)
    df <- useful_inds(df, informative = "AvOrAf")
    expect_equal(df$id[!df$useful], c(101, 108))
})
TRUE
