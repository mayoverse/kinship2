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
#' @aliases descendants,character
setMethod("descendants", signature(idlist = "character", obj = "character"),
    function(idlist, obj, dadid, momid) {
        id <- obj
        child <- id[!(is.na(match(dadid, idlist)) &
                    is.na(match(momid, idlist))
            )
        ]
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
setMethod("descendants", signature(idlist = "character", obj = "Pedigree"),
    function(idlist, obj) {
        descendants(idlist, obj$ped$id, obj$ped$dadid, obj$ped$momid)
    }
)

#' @rdname descendants
#' @docType methods
#' @aliases descendants,Pedigree
setMethod("descendants", signature(idlist = "character", obj = "Ped"),
    function(idlist, obj) {
        descendants(idlist, obj$id, obj$dadid, obj$momid)
    }
)
