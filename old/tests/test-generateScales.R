ped_df <- c(
    1, 3, 4, 2, TRUE, NA, "1", "None",
    2, 0, 0, 1, TRUE, 1, 2, "A",
    3, 8, 7, "man", FALSE, NA, "2", "E",
    4, 6, 5, "woman", FALSE, "A", 3, "A",
    5, 0, 0, "f", FALSE, NA, 7, "E",
    6, "None", 0, "m", TRUE, NA, "NA", "D",
    7, 0, "0", 1, FALSE, "NA", 6, "A",
    8, 0, 0, 1, FALSE, "None", "3", "D",
    8, 2, 0, 2, FALSE, "None", "3", "A",
    9, 9, 8, 3, FALSE, "Ab", "5", "B")
ped_df <- matrix(ped_df, ncol = 8, byrow = TRUE)
dimnames(ped_df) <- list(NULL, c("indId", "fatherId", "motherId", "gender", "steril",
    "available", "NumOther", "AffMod"))
ped_df <- data.frame(ped_df)
ped_df <- normPed(ped_df, na_strings = c("None", "0", "NA"))

ped_df_aff_num <- generate_aff_inds(ped_df, "NumOther", threshold = 4, sup_thres_aff = TRUE)
ped_df_aff_fact <- generate_aff_inds(ped_df, "AffMod", mods_aff = c("D", "E"))

test_that("generate colors full scale off", {
    list_num <- generate_colors(ped_df_aff_num, "NumOther", keep_full_scale = FALSE)
    list_fact <- generate_colors(ped_df_aff_fact, "AffMod", keep_full_scale = FALSE)
    expect_equal(list_num$ped_df$border, c("black", "green", "green", "black"))
    expect_equal(list_num$ped_df$fill, c("white", "white", "white", "red"))
    expect_equal(list_fact$ped_df$border, c("black", "green", "green", "black"))
    expect_equal(list_fact$ped_df$fill, c("grey", "white", "white", "red"))
})

test_that("generate colors full scale on", {
    list_num <- generate_colors(ped_df_aff_num, "NumOther", keep_full_scale = TRUE)
    list_fact <- generate_colors(ped_df_aff_fact, "AffMod", keep_full_scale = TRUE)
    expect_equal(list_num$ped_df$border, c("black", "green", "green", "black"))
    expect_equal(list_num$ped_df$fill, c("#FFFFFF", "#9AB1C4", "#36648B", "#F67700"))
    expect_equal(list_fact$ped_df$border, c("black", "green", "green", "black"))
    expect_equal(list_fact$ped_df$fill, c("grey", "#FFFFFF", "#FFFFFF", "#EEEE00"))
})
TRUE