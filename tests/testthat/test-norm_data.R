df <- c(
    1, 3, 4, 2, FALSE, NA, "1", "None",
    2, 0, 0, 1, TRUE, 1, 2, "A",
    3, 8, 7, "man", FALSE, NA, "2", "E",
    4, 6, 5, "woman", FALSE, "A", 3, "A",
    5, 0, 0, "f", FALSE, NA, 7, "E",
    6, "None", 0, "m", TRUE, NA, "NA", "D",
    7, 0, "0", 1, FALSE, "NA", 6, "A",
    8, 0, 0, 1, TRUE, "None", "3", "D",
    8, 0, 0, 2, TRUE, "None", "3", "A",
    9, 9, 1, 3, FALSE, "Ab", "5", "B"
)
df <- matrix(df, ncol = 8, byrow = TRUE)
dimnames(df) <- list(NULL, c("IndID", "FatherID", "MotherID",
    "Gender", "Sterilisation", "Availability", "NumOther", "AffMod"))
df <- data.frame(df)
list_get <- norm_data(df, na_strings = c("None", "0", "NA"))

df_aff_num <- generate_aff_inds(list_get$norm, "NumOther",
    threshold = 4, sup_thres_aff = TRUE)
df_aff_fact <- generate_aff_inds(list_get$norm, "AffMod",
    mods_aff = c("D", "E"))

test_that("Norm data", {
    expect_equal(dim(list_get$norm), c(4, 15))
    expect_equal(dim(list_get$errors), c(6, 15))
    expect_equal(list_get$norm$IndID, c("1", "2", "4", "5"))
    expect_equal(list_get$errors$IndID, c("3", "6", "7", "8", "8", "9"))
})

test_that("generate aff inds", {
    expect_equal(as.character(df_aff_num$affected), c("0", "0", "0", "1"))
    expect_equal(as.character(df_aff_fact$affected), c(NA, "0", "0", "1"))
})

test_that("generate colors full scale off", {
    list_num <- generate_colors(df_aff_num, "NumOther", keep_full_scale = FALSE)
    list_fact <- generate_colors(df_aff_fact, "AffMod", keep_full_scale = FALSE)
    expect_equal(list_num$df$border, c("black", "green", "green", "black"))
    expect_equal(list_num$df$fill, c("white", "white", "white", "red"))
    expect_equal(list_fact$df$border, c("black", "green", "green", "black"))
    expect_equal(list_fact$df$fill, c("grey", "white", "white", "red"))
})

test_that("generate colors full scale on", {
    list_num <- generate_colors(df_aff_num, "NumOther", keep_full_scale = TRUE)
    list_fact <- generate_colors(df_aff_fact, "AffMod", keep_full_scale = TRUE)
    expect_equal(list_num$df$border, c("black", "green", "green", "black"))
    expect_equal(list_num$df$fill, c("#FFFFFF", "#9AB1C4", "#36648B", "#F67700"))
    expect_equal(list_fact$df$border, c("black", "green", "green", "black"))
    expect_equal(list_fact$df$fill, c("grey", "#FFFFFF", "#FFFFFF", "#EEEE00"))
})
