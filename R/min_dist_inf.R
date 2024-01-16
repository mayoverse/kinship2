#' @importFrom dplyr %>%
NULL

#' Minimum distance to the informative individuals
#'
#' @description Compute the minimum distance between the informative
#' individuals and all the others.
#' This distance is a transformation of the maximum kinship degree between
#' the informative individuals and all the others.
#' This transformation is done by taking the log2 of the inverse of the
#' maximum kinship degree.
#'
#' \eqn{minDist = log2(1 / \max(kinship))}
#'
#' Therefore, the minimum distance is 0 when the maximum kinship is 1 and
#' is infinite when the maximum kinship is 0. For siblings, the kinship value
#' is 0.5 and the minimum distance is 1. Each time the kinship degree is divided
#' by 2, the minimum distance is increased by 1.
#'
#'
#' @param ... Additional arguments
#' @param id_inf An identifiers vector of informative individuals.
#' @inheritParams Ped
#' @inheritParams is_informative
#'
#' @return
#' ## When obj is a vector
#' A vector of the minimum distance between the informative individuals
#' and all the others corresponding to the order of the individuals in the
#' `obj` vector.
#'
#' ## When obj is a Pedigree
#' The Pedigree object with a new slot named 'kin' containing the minimum
#' distance between each individuals and the informative individuals.
#' The `isinf` slot is also updated with the informative individuals.
#'
#' @seealso [kinship()]
#' @include is_informative.R
#' @include kinship.R
#' @export
#' @usage NULL
setGeneric("min_dist_inf", signature = "obj",
    function(obj, ...) standardGeneric("min_dist_inf")
)

#' @rdname min_dist_inf
#' @examples
#'
#' min_dist_inf(
#'      c("A", "B", "C", "D", "E"),
#'      c("C", "D", "0", "0", "0"),
#'      c("E", "E", "0", "0", "0"),
#'      sex = c(1, 2, 1, 2, 1),
#'      id_inf = c("D", "E")
#' )
#' @export
setMethod("min_dist_inf", "character", function(obj,
    dadid, momid, sex, id_inf
) {
    id <- obj
    # Selection of all informative individuals depending of the informative
    # parameter
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

#' @rdname min_dist_inf
#' @examples
#'
#' data(sampleped)
#' ped <- Pedigree(sampleped)
#' kin(ped(min_dist_inf(ped, col_aff = "affection_mods")))
#' @export
setMethod("min_dist_inf", "Pedigree", function(obj,
    col_aff = NULL, informative = "AvAf", reset = FALSE, ...
) {
    obj_aff <- is_informative(obj, col_aff, informative = informative,
        reset = reset
    )

    new_ped <- min_dist_inf(
        ped(obj_aff),
        informative = informative, reset = reset
    )

    ped(obj_aff) <- new_ped
    validObject(obj_aff)
    obj_aff
})

#' @rdname min_dist_inf
#' @param reset If TRUE, the `kin` and if `isinf` columns is reset
#' @export
setMethod("min_dist_inf", "Ped", function(
    obj, informative = "AvAf", reset = FALSE
) {

    if (!reset & any(!is.na(kin(obj)))) {
        stop(
            "The kin slot already has values in the Ped object",
            " and reset is set to FALSE"
        )
    }

    id_inf <- id(obj)[isinf(obj)]
    kin <- min_dist_inf(
        id(obj), dadid(obj), momid(obj), sex(obj), id_inf
    )

    kin(obj) <- kin
    validObject(obj)
    obj
})
