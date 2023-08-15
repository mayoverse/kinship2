#' Compute the usefulness of individuals
#'
#' @description Check for usefulness of individuals
#'
#' @details Check for the informativeness of the individuals based on the
#' informative parameter given, the number of children and the usefulness
#' of their parents. A `useful` column is added to the dataframe with the
#' usefulness of the individual. This boolean is hereditary.
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
#' @export useful_inds
useful_inds <- function(df, informative = "AvAf", keep_infos = FALSE) {

    cols_needed <- c("id", "dadid", "momid", "sex",
        "avail", "affected", "num_child_tot")

    df <- check_columns(df, cols_needed, "", "", others_cols = TRUE)

    # Get informative individuals
    id_inf <- is_informative(df, informative = informative)
    is_inf <- df$id %in% id_inf

    # Keep individual affected or available
    if (keep_infos) {
        is_inf <- is_inf |
            (!is.na(df$affected) & df$affected == 1) |
            (!is.na(df$avail) & df$avail == 1)
    }

    # Check if parents participate to the pedigree structure
    ped_part <- df$num_child_tot > 1
    to_kept <- is_inf | ped_part

    num_ind_old <- 0
    num_ind_new <- length(df$id[to_kept])
    # Until no more individuals are added
    while (num_ind_old != num_ind_new) {
        for (it1 in seq_along(to_kept)) {
            if (!to_kept[it1]) {
                # If not already kept Check if parents to be kept
                to_kept[it1] <- any(to_kept[df$id %in%
                    c(df$dadid[it1], df$momid[it1])])
            }
        }
        num_ind_old <- num_ind_new
        num_ind_new <- length(df$id[to_kept])
    }
    to_kept
}
TRUE
