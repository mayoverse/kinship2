# S4 classes ###
#' @include validity.R
#' A class to represent a pedigree.
#'
#' A pedigree is a ensemble of individuals linked to each other into
#' a family tree.
#'
#' They are created from a data.frame containing the individuals informations
#' and a relation ship data.frame for the special links between individuals.
#' A list of scales can be provided to create a legend.
#' @slot ped A data.frame with the individuals informations. The minimum
#' columns required are 'id', 'dadid', 'momid' and 'sex'
#' @slot rel A data.frame for the special relationship between individuals.
#' The minimum columns required are 'id1', 'id2' and 'code'.
#' @slot scales A list of columns to use for the affection status. Each element
#' is named after the column to use and contains information about the
#' generation of the affected status : 'threshold', 'aff_sup_threshold',
#' 'mods_aff' and the plotting informations : 'mods_labels', 'fill', 'border',
#' 'density', 'angle'.
#' @aliases Pedigree
#' @export
#' @seealso \\link{pedigree}
#' @family methods
#' @examples
setClass(
    "Pedigree",
    slots = c(
        ped = "data.frame",
        rel = "data.frame",
        scales = "data.frame"
    )
)

setValidity("Pedigree", isValid)

#### S4 methods ####

#' Pedigree show method.
#' @param object A Pedigree object.
#' @return A character vector with the informations about the object.
setMethod("show", signature(object = "Pedigree"), function(object) {
    cat("Pedigree object with", nrow(object@ped), "individuals and",
        nrow(object@rel), "special relationships.", fill = TRUE)
})

#' Pedigree summary method.
#' @param object A Pedigree object.
#' @return A character vector with the summary of the object.
setMethod("summary", signature(object = "Pedigree"), function(object) {
    cat("Pedigree object with", nrow(object@ped), "individuals", fill = TRUE)
    cols_summary <- c("family", "id", "dadid", "momid", "sex", "avail")
    print(summary(object@ped[cols_summary], maxsum = 5))
    cat("and", nrow(object@rel), "special relationships.", fill = TRUE)
    print(summary(object@rel))
    cat("The scales are:", levels(as.factor(object@scales$column)), fill = TRUE)
})

setMethod("[[", c(x = "Pedigree", i = "ANY", j = "missing"),
    function(x, i, j, ..., drop = TRUE) {
        slot(x, i)
})

setMethod("[[<-", c(x = "Pedigree", i = "ANY", j = "missing", value = "ANY"),
    function(x, i, j, ..., value) {
        slot(x, i) <- value
        x
})

setMethod("$", c(x = "Pedigree"),
    function(x, name) {
        slot(x, name)
})
setMethod("$<-", c(x = "Pedigree"),
    function(x, name, value) {
        slot(x, name) <- value
        x
})

TRUE
