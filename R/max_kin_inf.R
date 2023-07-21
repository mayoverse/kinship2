usethis::use_package("dplyr")
#' @title Kinship computation
#'
#' @description Compute the kinship between the informative individuals and
#' all the others.
#'
#' @param df Dataframe with individuals to select
#' @param informative Informative individuals selection can take 3 values:
#' 'AvAf' (available and affected),
#' 'AvOrAf' (available or affected),
#' 'Av' (available only),
#' 'Af' (affected only),
#' 'All' (all individuals)
#' or a numeric vector of individuals id
#' or a boolean
#'
#' @return Dataframe with selected individuals
#'
#' @examples
#' data(sample.ped)
#' df <- max_kin_inf(sample.ped, informative = 'Av/Af')
#' summary(df$kin)
#'
#' @export max_kin_inf
max_kin_inf <- function(df, informative = "AvAf") {
    print("Bal: max_kin_inf")

    cols_needed <- c("id", "dadid", "momid", "sex", "avail", "affected")
    cols_used <- c("kin", "inf")

    df <- check_columns(df, cols_needed, cols_used, "", others_cols = TRUE)

    # Selection of all informative individuals depending of the informative
    # parameter
    id_inf <- is_informative(df, informative)
    if (any(is.na(id_inf)) || length(id_inf) == 0) {
        stop("No informative individuals detected")
    }
    # For all individuals, compute kinship degree
    mat <- kinship(with(df, pedigree(id, dadid, momid, sex)))
    sub <- mat[, colnames(mat) %in% id_inf] %>%
        as.data.frame()

    df$kin <- log2(1 / apply(sub, 1, max))
    df$kin[is.infinite(df$kin)] <- NA

    checked <- length(df$kin[!is.na(df$kin)])
    message(paste(checked,
        "individuals linked to informative individuals detected"))
    df
}
TRUE
