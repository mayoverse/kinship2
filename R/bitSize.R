#' Get pedigree bitsize
#'
#' @description
#' Calculate pedigree bitsize, defined as 2 * # NonFounders - # Founders
#'
#' @details
#' This is a utility function used in `pedigree.shrink()`
#'
#' @param dadid Vector of fathers ids
#' @param momid Vector of mothers ids
#' @param missid Character defining the missing ids
#'
#' @return A list with the following components:
#' ## bitSize
#' The bitSize of input pedigree
#' ## nFounder
#' The number of founders in the pedigree
#' ## nNonFounder
#' The number of nonfounders in the pedgiree
#'
#' @seealso `pedigree.shrink`
#' @export bitSize
setGeneric("bitSize", function(obj, ...) {
    standardGeneric("bitSize")
})

setMethod("bitSize", "character", function(obj, momid, missid = "0") {
    if (length(obj) != length(momid)) {
        stop("obj and momid should have the same length")
    }
    founder <- obj == missid & momid == missid
    ped_size <- length(obj)
    n_founder <- sum(founder)
    n_non_founder <- ped_size - n_founder
    bit_size <- 2 * n_non_founder - n_founder
    list(
        bitSize = bit_size, nFounder = n_founder,
        nNonFounder = n_non_founder)
})

setMethod("bitSize", "Pedigree",
    function(obj, missid = "0", ...) {
        bitSize(obj$ped$dadid, obj$ped$momid, missid)
    }
)