#' @importFrom plyr revalue
NULL

#' Process the affection informations
#'
#' @description Perform transformation uppon a vector given as the one
#' containing the affection status to obtain an `affected` binary state.
#'
#' @details This function helps to configure a binary state from a character or
#' numeric variable.
#'
#' ## If the variable is a `character` or a `factor`:
#'
#' In this case the affected state will depend on the modality provided as
#' an affected status. All individuals with a value corresponding to one of the
#' element in the vector **mods_aff** will be considered as affected.
#'
#' ## If the variable is `numeric`:
#'
#' In this case the affected state will be `TRUE` if the value of the individual
#' is above the **threshold** if **sup_thres_aff** is `TRUE` and `FALSE`
#' otherwise.
#'
#' @param values Vector containing the values of the column to process.
#' @param mods_aff Vector of modality to consider as affected in the case
#' where the `values` is a factor.
#' @param threshold Numeric value separating the affected and healthy subject
#' in the case where the `values` is numeric.
#' @param sup_thres_aff Boolean defining if the affected individual are above
#' the threshold or not.
#' If `TRUE`, the individuals will be considered affected
#' if the value of `values` is stricly above the `threshold`.
#' If `FALSE`, the individuals will be considered affected if the
#' value is stricly under the `threshold`.
#'
#' @return A dataframe with the `affected` column processed accordingly.
#' The different columns are:
#' - `mods`: The different modalities of the column
#' - `labels`: The labels of the different modalities
#' - `affected`: The column processed to have only TRUE/FALSE values
#'
#' @examples
#' generate_aff_inds(c(1, 2, 3, 4, 5), threshold = 3, sup_thres_aff = TRUE)
#' generate_aff_inds(c("A", "B", "C", "A", "V", "B"), mods_aff = c("A", "B"))
#' @author Louis Le Nézet
#' @keywords generate_scales
#' @export
generate_aff_inds <- function(values, mods_aff = NULL,
    threshold = NULL, sup_thres_aff = NULL
) {
    mods <- rep(NA, length(values))
    if (is.numeric(values)) {
        if (is.null(threshold) || is.na(threshold)) {
            stop("Variable is numeric but threshold not correctly defined")
        }
        if (is.na(sup_thres_aff) || is.null(sup_thres_aff)) {
            stop("Variable is numeric but sup_thres_aff not a boolean")
        }
        if (sup_thres_aff) {
            # Aff are > to threshold
            aff_lab <- paste("Affected > to", threshold)
            healthy_lab <- paste("Healthy <= to", threshold)

            mods[values <= threshold & !is.na(values)] <- 0
            mods[values > threshold & !is.na(values)] <- 1
        } else {
            # Aff are < to threshold
            aff_lab <- paste("Affected < to", threshold)
            healthy_lab <- paste("Healthy >= to", threshold)

            mods[values >= threshold & !is.na(values)] <- 0
            mods[values < threshold & !is.na(values)] <- 1
        }
    } else {
        # Separate for factors by levels
        mods_non_aff <- levels(droplevels(as.factor(
            values[!values %in% as.character(mods_aff)]
        )))
        if (length(mods_non_aff) == 0) {
            mods_non_aff <- "None"
        }
        if (length(mods_aff) == 0) {
            mods_aff <- "None"
        }

        aff_lab <- paste("Affected are", paste(mods_aff, collapse = "/"))
        healthy_lab <- paste("Healthy are", paste(mods_non_aff, collapse = "/"))

        mods[!is.na(values)] <- 0
        mods[values %in% mods_aff & !is.na(values)] <- 1
    }

    labels_to_use <- c(`1` = aff_lab, `0` = healthy_lab)
    aff_to_use <- c(TRUE, FALSE)
    names(aff_to_use) <- c(aff_lab, healthy_lab)

    labels <- revalue(as.character(mods), labels_to_use, warn_missing = FALSE)
    affected <- as.logical(revalue(labels, aff_to_use, warn_missing = FALSE))
    as.data.frame(list(mods = mods, labels = labels, affected = affected))
}
