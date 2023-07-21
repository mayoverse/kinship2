#' @include validity.R
NULL

# S4 classes ###
#' A class to represent a pedigree.
#'
#' A pedigree is a ensemble of individuals linked to each other into
#' a family tree.
#'
#' They are created from a data.frame containing the individuals informations
#' and a relation ship data.frame for the special links between individuals.
#' A list of scales can be provided to create a legend.
#' @slot ped A data.frame with the individuals informations. The minimum
#' columns required are "id", "dadid", "momid" and "sex"
#' @slot rel A data.frame for the special relationship between individuals.
#' The minimum columns required are "id1", "id2" and "code".
#' @slot scales A list of columns to use for the affection status. Each element
#' is named after the column to use and contains information about the
#' generation of the affected status : "threshold", "aff_sup_threshold",
#' "mods_aff" and the plotting informations : "mods_labels", "fill", "border",
#' "density", "angle".
#' @aliases Pedigree
#' @export
#' @seealso \link{pedigree}
#' @family methods
#' @examples
setClass(
    "Pedigree",
    slots = c(
        ped = "data.frame",
        rel = "data.frame",
        scales = "data.frame",
        hints = "data.frame"
    )
)

setValidity("Pedigree", isValid)


#### S4 methods ####

#' Pedigree show method.
#' @param object A Pedigree object.
#' @return A character vector with the informations about the object.
setMethod(
    "show",
    signature(object = "Pedigree"),
    function(object) {
        cat("Pedigree object with", nrow(object@ped), "individuals and",
            nrow(object@rel), "special relationships.")
    }
)

#' Pedigree summary method.
#' @param object A Pedigree object.
#' @return A character vector with the summary of the object.
setMethod(
    "summary",
    signature(object = "Pedigree"),
    function(object) {
        cat("Pedigree object with", nrow(object@ped), "individuals")
        cat(summary(object@ped))
        cat("and", nrow(object@rel), "special relationships.")
        cat(summary(object@rel))
        cat("The scales are:", levels(as.factor(object@scales$column)))

    }
)

#' Accessor for the ped slot.
#' @param object A Pedigree object.
#' @return A data frame with the individuals informations.
setMethod(
    "[[",
    signature(x = "Pedigree", i = "ped"),
    function(x, i) {
        x@ped
    }
)

#' Accessor for the rel slot.
#' @param object A Pedigree object.
#' @return A data frame with the special relationships.
setMethod(
    "[[",
    signature(x = "Pedigree", i = "rel"),
    function(x, i) {
        x@rel
    }
)
