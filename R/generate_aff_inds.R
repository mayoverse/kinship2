usethis::use_package("plyr")
#' Process the information for affection
#'
#' @details Perform transformation uppon a column given as the one
#' containing affection status to get an `affected` column usable for
#' the rest of the script
#'
#' @param df The dataframe to process
#' @param col_aff The name of the column containing the values to process as
#' affection
#' @param mods_aff Vector of modality to consider as affected in the case
#' where the `col_aff` column is a factor.
#' @param threshold Numeric value separating the affected and healthy subject
#' in the case where the `col_aff` column is numeric.
#' @param sup_thres_aff Boolean defining if the affected individual are above
#' the threshold or not. If TRUE, the individuals will be considered affected
#' if the value of `col_aff` is stricly above the `threshold`. If FALSE, the
#' individuals will be considered affected if the value of `col_aff` is
#' stricly under the `threshold`.
#'
#' @return The dataframe with the `affected` column processed accordingly
#'
#' @export generate_aff_inds
generate_aff_inds <- function(df, col_aff,
    mods_aff = NULL, threshold = NULL, sup_thres_aff = NULL) {

  cols_needed <- col_aff
  cols_used <- c("affected", "mods_aff")

  df <- check_columns(df, cols_needed, cols_used, "", others_cols = TRUE)

  if (is.numeric(df[[col_aff]])) {
    if (is.null(threshold) || is.na(threshold)) {
      stop("Variable is numeric but threshold not correctly defined")
    }
    if (is.na(sup_thres_aff) || is.null(sup_thres_aff)) {
      stop("Variable is numeric but sup_thres_aff not a boolean")
    }
    if (sup_thres_aff) { # Aff are > to threshold
      levels_to_use <- c(
        "1" = paste("Affected > to", threshold),
        "0" = paste("Healthy <= to", threshold)
        )
      df$affected[df[[col_aff]] <= threshold &
        !is.na(df[[col_aff]])] <- 0
      df$affected[df[[col_aff]] > threshold &
        !is.na(df[[col_aff]])] <- 1
    } else { # Aff are < to threshold
      levels_to_use <- c(
        "1" = paste("Affected < to", threshold),
        "0" = paste("Healthy >= to", threshold)
        )
      df$affected[df[[col_aff]] >= threshold &
        !is.na(df[[col_aff]])] <- 0
      df$affected[df[[col_aff]] < threshold &
        !is.na(df[[col_aff]])] <- 1
    }
  } else {
    # Separate for factors by levels
    mods_non_aff <- levels(
        droplevels(
          as.factor(df[[col_aff]][!df[[col_aff]] %in% mods_aff])
          )
        )
    if (length(mods_non_aff) == 0) {
      mods_non_aff <- "None"
    }
    if (length(mods_aff) == 0) {
      mods_aff <- "None"
    }
    levels_to_use <- c(
      "0" = paste("Healthy are", paste(mods_non_aff, collapse = "/")),
      "1" = paste("Affected are", paste(mods_aff, collapse = "/"))
    )
    df$affected[!is.na(df[[col_aff]])] <- 0
    df$affected[df[[col_aff]] %in% mods_aff &
      !is.na(df[[col_aff]])] <- 1
  }

  df$affected <- as.factor(df$affected)
  df$mods_aff <- plyr::revalue(df$affected, levels_to_use)
  df
}
