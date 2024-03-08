#' Usefulness of individuals
#'
#' @description Compute the usefulness of individuals
#'
#' @details Check for the informativeness of the individuals based on the
#' informative parameter given, the number of children and the usefulness
#' of their parents. A `useful` slot is added to the Ped object with the
#' usefulness of the individual. This boolean is hereditary.
#'
#' @param num_child_tot A numeric vector of the number of children of each
#' individuals
#' @param keep_infos Boolean to indicate if individuals with unknown status
#' but available or reverse should be kept
#' @inheritParams Ped
#' @inheritParams is_informative
#'
#' @return
#' ## When obj is a vector
#' A vector of useful individuals identifiers
#'
#' ## When obj is a Pedigree or Ped object
#' The Pedigree or Ped object with the slot 'useful' containing `TRUE` for
#' useful individuals and `FALSE` otherwise.
#' @keywords shrink
#' @export
#' @usage NULL
setGeneric("useful_inds", signature = "obj",
    function(obj, ...) standardGeneric("useful_inds")
)

#' @include is_informative.R
#' @rdname useful_inds
#' @export
setMethod("useful_inds", "character",
    function(obj, dadid, momid, avail, affected, num_child_tot,
        informative = "AvAf", keep_infos = FALSE
    ) {
        id <- obj

        # Get informative individuals
        id_inf <- is_informative(id, avail, affected,
            informative
        )
        isinf <- id %in% id_inf

        # Keep individual affected or available
        if (keep_infos) {
            isinf <- isinf |
                (!is.na(affected) & affected == 1) |
                (!is.na(avail) & avail == 1)
        }

        # Check if parents participate to the Pedigree structure
        ped_part <- num_child_tot > 1
        to_kept <- isinf | ped_part

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
        id[to_kept]
    }
)

#' @rdname useful_inds
#' @param reset Boolean to indicate if the `useful` column should be reset
#' @examples
#'
#' data(sampleped)
#' ped1 <- Pedigree(sampleped[sampleped$famid == "1",])
#' ped(useful_inds(ped1, informative = "AvAf"))
#' @export
setMethod("useful_inds", "Pedigree", function(obj,
    informative = "AvAf", keep_infos = FALSE, reset = FALSE
) {
    new_ped <- useful_inds(ped(obj),
        informative, keep_infos, reset
    )

    obj@ped <- new_ped
    validObject(obj)
    obj
})

#' @rdname useful_inds
#' @export
setMethod("useful_inds", "Ped", function(obj,
    informative = "AvAf", keep_infos = FALSE, reset = FALSE
) {
    useful <- useful_inds(id(obj), dadid(obj), momid(obj),
        avail(obj), affected(obj), obj@num_child_tot,
        informative, keep_infos
    )

    if (!reset & any(!is.na(useful(obj)))) {
        stop(
            "The useful slot already has values in the Ped object",
            " and reset is set to FALSE"
        )
    }
    obj@useful <- vect_to_binary(
        ifelse(id(obj) %in% useful, 1, 0), logical = TRUE
    )
    validObject(obj)
    obj
})
