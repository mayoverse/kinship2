#' Compute the usefulness of individuals
#'
#' @description Check for usefulness of individuals
#'
#' @details Check for the informativeness of the individuals based on the
#' informative parameter given, the number of children and the usefulness
#' of their parents. A `useful` column is added to the dataframe with the
#' usefulness of the individual. This boolean is hereditary.
#'
#' @param obj A dataframe, a Pedigree object or a character vector of ids
#' @param sex A vector with the gender information
#' @param num_child_tot A numeric vector of the number of children of each
#' individuals
#' @param keep_infos Boolean to indicate if individuals with unknown status
#' but available or reverse should be kept
#' @inheritParams is_informative
#' @inheritParams num_child
#' @param ... Other arguments passed to methods.
#'
#' @export
setGeneric("useful_inds", signature = "obj",
    function(obj, ...) standardGeneric("useful_inds")
)

#' @include is_informative.R
#' @export
#' @rdname useful_inds
#' @docType methods
#' @aliases useful_inds,character
setMethod("useful_inds", "character",
    function(obj, dadid, momid, sex, avail, affected, num_child_tot,
        informative = "AvAf", keep_infos = FALSE, missid = "0"
    ) {
        id <- obj

        # Get informative individuals
        id_inf <- is_informative(id, avail, affected,
            informative, missid
        )
        is_inf <- id %in% id_inf

        # Keep individual affected or available
        if (keep_infos) {
            is_inf <- is_inf |
                (!is.na(affected) & affected == 1) |
                (!is.na(avail) & avail == 1)
        }

        # Check if parents participate to the pedigree structure
        ped_part <- num_child_tot > 1
        to_kept <- is_inf | ped_part

        num_ind_old <- 0
        num_ind_new <- length(id[to_kept])
        # Until no more individuals are added
        while (num_ind_old != num_ind_new) {
            for (it1 in seq_along(to_kept)) {
                if (!to_kept[it1]) {
                    # If not already kept Check if parents to be kept
                    to_kept[it1] <- any(to_kept[
                        id %in% c(dadid[it1], momid[it1])
                    ])
                }
            }
            num_ind_old <- num_ind_new
            num_ind_new <- length(id[to_kept])
        }
        to_kept
    }
)

#' @docType methods
#' @aliases useful_inds,data.frame
#' @export
#' @rdname useful_inds
setMethod("useful_inds", "data.frame",
    function(obj, informative = "AvAf", keep_infos = FALSE, missid = "0") {
        df <- obj
        cols_needed <- c(
            "id", "dadid", "momid", "sex",
            "avail", "affected", "num_child_tot"
        )

        check_columns(df, cols_needed, "", "", others_cols = TRUE)
        with(df, useful_inds(
            id, dadid, momid, sex,
            avail, affected, num_child_tot,
            informative, keep_infos, missid
        ))
    }
)

#' @docType methods
#' @aliases useful_inds,Pedigree
#' @export
#' @rdname useful_inds
#' @param reset Boolean to indicate if the `useful` column should be reset
setMethod("useful_inds", "Pedigree",
    function(obj, reset = FALSE, ...) {
        to_kept <- useful_inds(obj$ped, ...)
        if (!reset) {
            check_columns(obj$ped, NULL, "useful", NULL, others_cols = TRUE)
        }
        obj$ped$useful <- to_kept
        obj
    }
)
