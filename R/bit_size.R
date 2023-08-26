#' Get pedigree bit_size
#'
#' @description
#' Calculate pedigree bit_size, defined as 2 * # NonFounders - # Founders
#'
#' @details
#' This is a utility function used in `pedigree.shrink()`
#'
#' @param dadid Vector of fathers ids
#' @param momid Vector of mothers ids
#' @param missid Character defining the missing ids
#'
#' @return A list with the following components:
#' ## bit_size
#' The bit_size of input pedigree
#' ## nFounder
#' The number of founders in the pedigree
#' ## nNonFounder
#' The number of nonfounders in the pedgiree
#'
#' @seealso `pedigree.shrink`
#' @include pedigreeClass.R
#' @export
setGeneric("bit_size", signature = "obj",
    function(obj, ...) standardGeneric("bit_size")
)

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

setMethod("bit_size", "Pedigree",
    function(obj, missid = "0") {
        bit_size(obj$ped$dadid, obj$ped$momid, missid)
    }
)