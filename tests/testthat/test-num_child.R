test_that("Num child", {
    df <- c(
        1, 3, 4, 2,
        2, 0, 0, 1,
        3, 0, 0, 1,
        4, 0, 0, 2,
        5, 107, 1, 3,
        100, 3, 4, 2,
        102, 3, 4, 2,
        106, 3, 4, 2,
        107, 0, 0, 1,
        201, 2, 1, 1,
        202, 2, 1, 1,
        203, 2, 1, 1,
        204, 2, 1, 1,
        205, 107, 102, 1
    )
    df <- matrix(df, ncol = 4, byrow = TRUE)
    dimnames(df) <- list(NULL, c("id", "dadid", "momid", "sex"))
    df <- data.frame(df)
    df <- as.data.frame(lapply(df, as.character))

    relation <- data.frame(
        id1 = c(2, 4, 102), id2 = c(106, 3, 100),
        code = c("Spouse", "Spouse", "MZ twin")
    )

    df_num <- with(df, num_child(id, dadid, momid, relation, missid = "0"))

    expect_equal(df_num$num_child_dir,
        c(5, 4, 4, 4, 0, 0, 1, 0, 2, 0, 0, 0, 0, 0)
    )
    expect_equal(df_num$num_child_tot,
        c(6, 5, 4, 4, 0, 0, 2, 4, 6, 0, 0, 0, 0, 0)
    )
    expect_equal(df_num$num_child_ind,
        c(1, 1, 0, 0, 0, 0, 1, 4, 4, 0, 0, 0, 0, 0)
    )

    ped <- Pedigree(df, relation, missid = "0")
    ped <- num_child(ped, reset = TRUE)
    cols <- c("num_child_dir", "num_child_tot", "num_child_ind")
    rownames(df_num) <- df_num$id
    expect_equal(as.data.frame(ped(ped))[cols], df_num[cols])
})
