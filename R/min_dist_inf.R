#' @importFrom dplyr %>%
NULL

#' @title Minimum distance to the informative individuals
#'
#' @description Compute the minimum distance between the informative
#' individuals and all the others.
#' This distance is a transformation of the maximum kinship degree between
#' the informative individuals and all the others.
#' This transformation is done by taking the log2 of the inverse of the
#' maximum kinship degree.
#'
#' \eqn{min_dist = log2(1/max(kinship))}
#'
#' Therefore, the minimum distance is 0 when the maximum kinship is 1 and
#' is infinite when the maximum kinship is 0. For siblings, the kinship value
#' is 0.5 and the minimum distance is 1. Each time the kinship degree is divided
#' by 2, the minimum distance is increased by 1.
#'
#'
#'
#' @inheritParams sex_to_factor
#' @inheritParams kinship
#' @inheritParams is_informative
#' @inheritParams is_parent
#'
#' @return
#' ## When obj is a vector
#' A vector of the minimum distance between the informative individuals
#' and all the others corresponding to the order of the individuals in the
#' `obj` vector.
#'
#' ## When obj is a Pedigree
#' The Pedigree object with a new column named 'kin' containing the kinship
#' degree.
#'
#' @examples
#' data(sampleped)
#' ped <- pedigree(sampleped)
#' min_dist_inf(ped, col_aff = "affected_aff")$ped
#'
#' @seealso [kinship()]
#' @include is_informative.R
#' @include kinship.R
#' @docType methods
#' @export
setGeneric("min_dist_inf", signature = "obj",
    function(obj, ...) standardGeneric("min_dist_inf")
)

#' @export
#' @rdname min_dist_inf
#' @aliases min_dist_inf,character
#' @docType methods
setMethod("min_dist_inf", "character", function(
    obj, dadid, momid, sex, avail, affected, informative = "AvAf"
) {
    id <- obj
    # Selection of all informative individuals depending of the informative
    # parameter
    id_inf <- is_informative(id, avail, affected, informative)
    if (any(is.na(id_inf)) || length(id_inf) == 0) {
        stop("No informative individuals detected")
    }
    # For all individuals, compute kinship degree
    mat <- as.matrix(kinship(id, dadid, momid, sex))
    sub <- mat[, colnames(mat) %in% id_inf] %>%
        as.data.frame()

    kin <- log2(1 / apply(sub, 1, max))
    kin[is.infinite(kin)] <- NA

    kin
})

#' @export
#' @rdname min_dist_inf
#' @aliases min_dist_inf,Pedigree
#' @docType methods
#' @param reset If TRUE, the `kin` and if `id_inf` columns is reset
setMethod("min_dist_inf", "Pedigree", function(obj,
    col_aff = NULL, informative = "AvAf", missid = "0", reset = FALSE, ...
) {
    ped <- is_informative(obj, col_aff, informative = informative,
        missid, reset
    )

    cols_needed <- c("avail", "affected")
    check_columns(ped$ped, cols_needed, NULL, NULL, others_cols = TRUE)

    kin <- min_dist_inf(
        ped$ped$id, ped$ped$dadid, ped$ped$momid, ped$ped$sex,
        ped$ped$avail, ped$ped$affected, informative
    )

    if (!reset) {
        check_columns(ped$ped, NULL, "kin", NULL)
    }

    ped$ped$kin <- kin
    ped
})
