test_that("useful_inds works", {
    data("sampleped")
    colnames(sampleped) <- c(
        "ped", "id", "dadid", "momid", "sex", "affected", "avail"
    )
    sampleped[c("id", "dadid", "momid")] <- as.data.frame(
        lapply(sampleped[c("id", "dadid", "momid")], as.character)
    )
    df <- merge(sampleped, num_child(sampleped)[c("id", "num_child_tot")])
    df$useful <- useful_inds(df)
    expect_equal(df$id[!df$useful], c("101", "102", "107", "108", "113", "117"))

    data("sampleped")
    ped <- pedigree(sampleped)
    ped <- num_child(ped)
    ped <- useful_inds(ped, informative = "Av")
    expect_equal(ped$ped$id[!ped$ped$useful], c("1_101", "1_102", "1_107", "1_108", "1_117"))

    expect_error(useful_inds(ped, informative = "AvOrAf"))

    ped <- useful_inds(ped, informative = "AvOrAf", reset = TRUE)
    expect_equal(ped$ped$id[!ped$ped$useful], c("1_101", "1_108"))
})
TRUE
