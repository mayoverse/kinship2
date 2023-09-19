#' Get pedigree bit_size
#'
#' @description
#' Calculate pedigree bit_size, defined as :
#'
#' \eqn{
#'   2 \times NbNonFounders - NbFounders
#' }
#'
#' @details
#' This is a utility function used in `shrink()`
#' to calculate the bit_size of a pedigree.
#'
#' @inheritParams kinship
#' @inheritParams is_parent
#' @param obj A pedigree object or a vector of fathers identifierss
#'
#' @return A list with the following components:
#'
#' - bit_size The bit_size of input pedigree
#' - nFounder The number of founders in the pedigree
#' - nNonFounder The number of nonfounders in the pedigree
#'
#' @seealso [shrink()]
#' @include pedigreeClass.R
#' @docType methods
#' @examples
#' data(sampleped)
#' ped <- pedigree(sampleped)
#' bit_size(ped)
#' @export
#' @keywords internal
setGeneric("bit_size", signature = "obj",
    function(obj, ...) standardGeneric("bit_size")
)

#' @docType methods
#' @aliases bit_size,character
#' @rdname bit_size
setMethod("bit_size", "character", function(obj, momid, missid = "0") {
    dadid <- obj
    if (length(dadid) != length(momid)) {
        stop("dadid and momid should have the same length")
    }
    founder <- dadid == missid & momid == missid
    ped_size <- length(dadid)
    n_founder <- sum(founder)
    n_non_founder <- ped_size - n_founder
    bit_size <- 2 * n_non_founder - n_founder
    list(
        bit_size = bit_size, nFounder = n_founder,
        nNonFounder = n_non_founder
    )
})

#' @docType methods
#' @aliases bit_size,Pedigree
#' @rdname bit_size
setMethod("bit_size", "Pedigree",
    function(obj, missid = "0") {
        bit_size(obj$ped$dadid, obj$ped$momid, missid)
    }
)