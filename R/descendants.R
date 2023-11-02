#' Find all the descendants
#'
#' @description
#' Find all the descendants of a particular list of individuals
#' given a Pedigree
#'
#' @inheritParams duporder
#' @inheritParams kinship
#'
#' @return
#' Vector of all descendants of the individuals in idlist.
#' The list is not ordered.
#'
#' @examples
#' data("sampleped")
#' ped <- Pedigree(sampleped)
#' descendants(c("1_101", "2_208"), ped)
#' @include AllClass.R
#' @export
#' @keywords internal
#' @docType methods
setGeneric("descendants",
    function(idlist, obj, ...) standardGeneric("descendants")
)

#' @rdname descendants
#' @docType methods
#' @aliases descendants,character_OR_integer
setMethod("descendants", signature(idlist = "character_OR_integer", obj = "character_OR_integer"),
    function(idlist, obj, dadid, momid) {
        id <- as.character(obj)
        idlist <- as.character(idlist)
        dadid <- as.character(dadid)
        momid <- as.character(momid)
        child <- id[!(
            is.na(match(dadid, idlist)) &
                is.na(match(momid, idlist))
        )]
        descend <- NULL
        while (length(child > 0)) {
            newchild <- id[!(is.na(match(dadid, child)) &
                        is.na(match(momid, child))
                )
            ]
            descend <- unique(c(descend, child))
            child <- newchild[is.na(match(newchild, c(idlist, descend)))]
        }
        descend
    }
)

#' @rdname descendants
#' @docType methods
#' @aliases descendants,Pedigree
setMethod("descendants", signature(idlist = "character_OR_integer", obj = "Pedigree"),
    function(idlist, obj) {
        descendants(idlist, ped(obj))
    }
)

#' @rdname descendants
#' @docType methods
#' @aliases descendants,Ped
setMethod("descendants", signature(idlist = "character_OR_integer", obj = "Ped"),
    function(idlist, obj) {
        descendants(as.character(idlist), id(obj), dadid(obj), momid(obj))
    }
)
