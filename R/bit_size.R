#' Bit size of a Pedigree
#'
#' Utility function used in the `shrink()` function
#' to calculate the bit size of a Pedigree.
#'
#' @details
#' The bit size of a Pedigree is defined as :
#'
#' \deqn{
#'   2 \times NbNonFounders - NbFounders
#' }
#'
#' Where `NbNonFounders` is the number of non founders in the Pedigree
#' (i.e. individuals with identified parents) and
#' `NbFounders` is the number of founders in the Pedigree
#' (i.e. individuals without identified parents).
#'
#' @param obj A Ped or Pedigree object or a vector of fathers identifiers
#' @inheritParams Ped
#'
#' @return A list with the following components:
#'
#' - bit_size The bit size of the Pedigree
#' - nFounder The number of founders in the Pedigree
#' - nNonFounder The number of non founders in the Pedigree
#'
#' @seealso [shrink()]
#' @include AllClass.R
#' @examples
#' data(sampleped)
#' ped <- Pedigree(sampleped)
#' bit_size(ped)
#' @export
#' @keywords internal, shrink
#' @usage NULL
setGeneric("bit_size", signature = "obj",
    function(obj, ...) standardGeneric("bit_size")
)

#' @rdname bit_size
setMethod("bit_size",
    "character_OR_integer",
    function(obj, momid, missid = NA_character_) {
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
    }
)

#' @rdname bit_size
setMethod("bit_size", "Pedigree",
    function(obj) {
        bit_size(ped(obj))
    }
)

#' @rdname bit_size
setMethod("bit_size", "Ped",
    function(obj) {
        bit_size(dadid(obj), momid(obj), NA_character_)
    }
)