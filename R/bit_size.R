#' Get Pedigree bit_size
#'
#' @description
#' Calculate Pedigree bit_size, defined as :
#'
#' \eqn{
#'   2 \times NbNonFounders - NbFounders
#' }
#'
#' @details
#' This is a utility function used in `shrink()`
#' to calculate the bit_size of a Pedigree.
#'
#' @inheritParams kinship
#' @inheritParams is_parent
#' @param obj A Pedigree object or a vector of fathers identifierss
#'
#' @return A list with the following components:
#'
#' - bit_size The bit_size of input Pedigree
#' - nFounder The number of founders in the Pedigree
#' - nNonFounder The number of nonfounders in the Pedigree
#'
#' @seealso [shrink()]
#' @include AllClass.R
#' @docType methods
#' @examples
#' data(sampleped)
#' ped <- Pedigree(sampleped)
#' bit_size(ped)
#' @export
#' @keywords internal
setGeneric("bit_size", signature = "obj",
    function(obj, ...) standardGeneric("bit_size")
)

#' @docType methods
#' @aliases bit_size,character
#' @rdname bit_size
setMethod("bit_size", "character_OR_integer", function(obj, momid, missid = "0") {
    dadid <- obj
    if (length(dadid) != length(momid)) {
        stop("dadid and momid should have the same length")
    }
    founder <- dadid %in% missid & momid %in% missid
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
        bit_size(ped(obj), missid)
    }
)

#' @docType methods
#' @aliases bit_size,Ped
#' @rdname bit_size
setMethod("bit_size", "Ped",
    function(obj, missid = NA_character_) {
        print(summary(obj))
        bit_size(dadid(obj), momid(obj), missid)
    }
)