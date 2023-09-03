#' My function
#'
#' @param obj An object either a character vector or a pedigree
#' @param ... Arguments to be passed to methods
#'
#' @docType methods
#' @export
setGeneric("myfunction", signature = "obj",
    function(obj, ...) standardGeneric("myfunction")
)

#' @docType methods
#' @aliases myfunction,character
#' @rdname myfunction
#' @param dadid A character vector
#' @param momid A character vector
#' @param missid Character defining the missing ids
#' @usage ## S4 method for signature 'character'
#' @usage myfunction(dadid, momid, missid = "0")
#' @return A character vector with the parents ids
setMethod("myfunction", "character", function(obj, momid, missid = "0") {
    dadid <- obj
    paste(dadid, momid, sep = missid)
})

#' @docType methods
#' @aliases myfunction,Pedigree
#' @param ped A pedigree object
#' @param missid Character defining the missing ids
#' @usage ## S4 method for signature 'Pedigree'
#' @usage myfunction(ped, missid = "0")
#' @return A pedigree with the parents ids
#' @include pedigreeClass.R
#' @rdname myfunction
setMethod("myfunction", "Pedigree",
    function(obj, missid = "0") {
        ped <- obj
        ped$par <- myfunction(ped$dadid, ped$momid, missid)
        ped
    }
)