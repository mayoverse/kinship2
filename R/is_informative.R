#' @title Is informative
#'
#' @description Select the ids of the informative individuals.
#'
#' @details Depending on the informative parameter, the function will extract
#' the ids of the informative individuals. In the case of a numeric vector,
#' the function will return the same vector. In the case of a boolean, the
#' function will return the ids of the individuals if TRUE, NA otherwise.
#' In the case of a string, the function will return the ids of the
#' corresponding informative individuals based on the avail and affected
#' columns.
#'
#' @param df Dataframe with individuals to select
#' @param informative Informative individuals selection can take 3 values:
#' "AvAf" (available and affected),
#' "AvOrAf" (available or affected),
#' "Av" (available only),
#' "Af" (affected only),
#' "All" (all individuals)
#' or a numeric vector of individuals id
#' or a boolean
#'
#' @export is_informative
is_informative <- function(df, informative = "AvAf") {

  cols_needed <- c("id", "avail", "affected")

  df <- check_columns(df, cols_needed, "", "", others_cols = TRUE)
  # Selection of all informative individuals
  # depending of the informative parameter
  if (is.numeric(informative)) {
    id_inf <- informative
  } else if (is.logical(informative)) {
    id_inf <- ifelse(informative, df$id, NA)
  } else {
    if (informative == "AvOrAf") {
      id_inf <- df[(df$avail == 1 & !is.na(df$avail)) |
        (df$affected == 1 & !is.na(df$affected)), "id"]
    } else if (informative == "Av") {
      id_inf <- df[df$avail == 1 & !is.na(df$avail), "id"]
    } else if (informative == "Af") {
      id_inf <- df[df$affected == 1 & !is.na(df$affected), "id"]
    } else if (informative == "AvAf") {
      id_inf <- df[(df$avail == 1 & !is.na(df$avail)) &
        (df$affected == 1 & !is.na(df$affected)), "id"]
    } else if (informative == "All") {
      id_inf <- df$id
    } else {
      stop("informative parameter not recognized")
    }
  }
  unique(id_inf)
}
