#### S4 Accessors ####
#' @include Pedigree.R
#' @include AllClass.R
#' @importFrom S4Vectors mcols
NULL

#### S4 Ped Accessors ####

#' Metadata setters of Ped object from a list
#'
#' @param x A Ped object.
#' @param value A list with the metadata.
#'
#' @return A Ped object with the metadata set.
#'
#' @rdname Ped
setMethod("mcols<-", signature(x = "Ped", value = "list"), function(x, value) {
    mcols(x) <- as(value, "DataFrame")
    x
})

#' Metadata setters of Ped object from a data.frame
#'
#' @param x A Ped object.
#' @param value A data.frame with the metadata.
#'
#' @return A Ped object with the metadata set.
#'
#' @rdname Ped
setMethod("mcols<-", signature(x = "Ped", value = "data.frame"), function(x, value) {
    mcols(x) <- as(value, "DataFrame")
    x
})

#' Famid getter of Ped object
#'
#' @param x A Ped object.
#'
#' @return A character vector with the famid of each individual.
#'
#' @rdname Ped
#' @aliases famid,Ped-method
#' @export
setGeneric("famid", function(x) {
    standardGeneric("famid")
})
setMethod("famid", signature(x = "Ped"), function(x) {
    x@famid
})
setMethod("famid", signature(x = "Rel"), function(x) {
    x@famid
})
setMethod("famid", signature(x = "Pedigree"), function(x) {
    famid(ped(x))
})

#' Id getter of Ped object
#'
#' @param x A Ped object.
#'
#' @return A character vector with the id of each individual.
#'
#' @rdname Ped
#' @aliases id,Ped-method
#' @export
setGeneric("id", function(x) {
    standardGeneric("id")
})
setMethod("id", signature(x = "Ped"), function(x) {
    x@id
})
setGeneric("id<-", function(x, value) {
    standardGeneric("id<-")
})
setMethod("id<-",
    signature(x = "Ped", value = "character_OR_integer"),
    function(x, value) {
        if (! is.character(value) && ! is.integer(value)) {
            stop("id must be a character or integer vector")
        }
        if (length(value) != length(x)) {
            stop(
                "The length of the new values for id should be: ",
                "equal to the length of the Ped object"
            )
        }
        x@id <- as.character(value)
        validObject(x)
        x
    }
)

#' Dadid getter of Ped object
#'
#' @param x A Ped object.
#'
#' @return A character vector with the dadid of each individual.
#'
#' @rdname Ped
#' @aliases dadid,Ped-method
#' @export
setGeneric("dadid", function(x) {
    standardGeneric("dadid")
})
setMethod("dadid", signature(x = "Ped"), function(x) {
    x@dadid
})
setGeneric("dadid<-", function(x, value) {
    standardGeneric("dadid<-")
})
setMethod("dadid<-",
    signature(x = "Ped", value = "character_OR_integer"),
    function(x, value) {
        if (! is.character(value) && ! is.integer(value)) {
            stop("dadid must be a character or integer vector")
        }
        if (length(value) != length(x)) {
            stop(
                "The length of the new values for dadid should be: ",
                "equal to the length of the Ped object"
            )
        }
        x@dadid <- as.character(value)
        validObject(x)
        x
    }
)

#' Momid getter of Ped object
#'
#' @param x A Ped object.
#'
#' @return A character vector with the momid of each individual.
#'
#' @rdname Ped
#' @aliases momid,Ped-method
#' @export
setGeneric("momid", function(x) {
    standardGeneric("momid")
})
setMethod("momid", signature(x = "Ped"), function(x) {
    x@momid
})
setGeneric("momid<-", function(x, value) {
    standardGeneric("momid<-")
})
setMethod("momid<-",
    signature(x = "Ped", value = "character_OR_integer"),
    function(x, value) {
        if (! is.character(value) && ! is.integer(value)) {
            stop("momid must be a character or integer vector")
        }
        if (length(value) != length(x)) {
            stop(
                "The length of the new values for momid should be: ",
                "equal to the length of the Ped object"
            )
        }
        x@momid <- as.character(value)
        validObject(x)
        x
    }
)

#' Sex getter of Ped object
#'
#' @param x A Ped object.
#'
#' @return A character vector with the sex of each individual.
#'
#' @rdname Ped
#' @aliases sex,Ped-method
#' @export
setGeneric("sex", function(x) {
    standardGeneric("sex")
})
setMethod("sex", signature(x = "Ped"), function(x) {
    x@sex
})
setGeneric("sex<-", function(x, value) {
    standardGeneric("sex<-")
})
setMethod("sex<-",
    signature(x = "Ped", value = "character_OR_integer"),
    function(x, value) {
        if (
            ! is.character(value) &&
                ! is.integer(value) &&
                ! is.factor(value)
        ) {
            stop("sex must be a character or integer vector")
        }
        if (length(value) != length(x)) {
            stop(
                "The length of the new values for sex should be: ",
                "equal to the length of the Ped object"
            )
        }
        x@sex <- sex_to_factor(value)
        validObject(x)
        x
    }
)

#### S4 Rel Accessors ####
#' Code accessor of Rel object
#'
#' @param x A Rel object.
#'
#' @return A character vector with the code of each relationship.
#'
#' @rdname Rel
#' @aliases code,Rel-method
#' @export
setGeneric("code", function(x) {
    standardGeneric("code")
})
setMethod("code", signature(x = "Rel"), function(x) {
    x@code
})

#' Id1 accessor of Rel object
#' @param x A Rel object.
#' @return A character vector with the id1 of each relationship.
#' @rdname Rel
#' @aliases id1,Rel-method
#' @export
setGeneric("id1", function(x) {
    standardGeneric("id1")
})
setMethod("id1", signature(x = "Rel"), function(x) {
    x@id1
})

#' Id2 accessor of Rel object
#' @param x A Rel object.
#' @return A character vector with the id2 of each relationship.
#' @rdname Rel
#' @aliases id2,Rel-method
#' @export
setGeneric("id2", function(x) {
    standardGeneric("id2")
})
setMethod("id2", signature(x = "Rel"), function(x) {
    x@id2
})

#### S4 Pedigree Accessors ####
#' @title Pedigree ped accessors
#' @param object A Pedigree object.
#' @param slot A slot in the Ped object of the Pedigree.
#' @return The slot `ped` present in the Pedigree object.
#' or one of its slot.
#' @rdname extract-methods
#' @aliases ped,Pedigree-method
#' @export
setGeneric("ped", function(object, slot) {
    standardGeneric("ped")
})

setMethod(
    "ped",
    signature(object = "Pedigree", slot = "ANY"),
    function(object, slot) {
        slot(object@ped, slot)
    }
)

setMethod(
    "ped",
    signature(object = "Pedigree", slot = "missing"),
    function(object) {
        object@ped
    }
)

setGeneric("ped<-", function(object, slot, value) {
    standardGeneric("ped<-")
})

setMethod(
    "ped<-",
    signature(object = "Pedigree", slot = "ANY", value = "ANY"),
    function(object, slot, value) {
        ped_slots <- c(
            "id", "dadid", "momid", "sex", "famid",
            "steril", "status", "avail", "affected",
            "kin", "useful", "num_child_total",
            "num_child_direct", "num_child_indirect"
        )
        if (! slot %in% ped_slots) {
            stop("slot selected: ", slot, " is not a Ped slot")
        }
        if (length(value) != length(object)) {
            stop(
                "The length of the new value should be: ",
                "equal to the length of the pedigree"
            )
        }
        slot(object@ped, slot) <- value
        validObject(object)
        object
    }
)

setMethod(
    "ped<-",
    signature(object = "Pedigree", slot = "missing", value = "Ped"),
    function(object, slot, value) {
        object@ped <- value
        validObject(object)
        object
    }
)

#### S4 metadata Accessors ####
#' @title Pedigree metadata accessors
#' @param object A Pedigree object.
#' @return The metadata present in the Pedigree object.
#' @rdname extract-methods
#' @aliases mcols,Pedigree-method
#' @export
setMethod(
    "mcols",
    signature(x = "Pedigree"),
    function(x) {
        mcols(x@ped)
    }
)

#' @importFrom S4Vectors 'mcols<-'
setMethod(
    "mcols<-",
    signature(x = "Pedigree", value = "ANY"),
    function(x, value) {
        mcols(x@ped) <- value
        x
    }
)

#### S4 rel Accessors ####

#' @description Pedigree rel accessors
#' @param object A Pedigree object.
#' @return The slot `rel` present in the Pedigree object.
#' @rdname extract-methods
#' @aliases rel,Pedigree-method
#' @export
setGeneric("rel", function(object, slot) {
    standardGeneric("rel")
})

setMethod(
    "rel",
    signature(object = "Pedigree", slot = "ANY"),
    function(object, slot) {
        slot(object@rel, slot)
    }
)

setMethod(
    "rel",
    signature(object = "Pedigree", slot = "missing"),
    function(object, slot) {
        object@rel
    }
)

setGeneric("rel<-", function(object, slot, value) {
    standardGeneric("rel<-")
})

setMethod(
    "rel<-",
    signature(object = "Pedigree", slot = "ANY", value = "ANY"),
    function(object, slot, value) {
        rel_cols <- c("id1", "id2", "code", "famid")
        if (! slot %in% rel_cols) {
            stop("slot selected: ", slot, " is not a relationship column")
        }
        if (length(value) != length(object)) {
            stop(
                "The length of the new value should be: ",
                "equal to the length of the pedigree"
            )
        }
        slot(object@rel, slot) <- value
        validObject(object)
        object
    }
)

#### S4 scales Accessors ####
#' @description Pedigree scales accessors
#' @param object A Pedigree object.
#' @return The slot `scales` present in the Pedigree object.
#' @rdname extract-methods
#' @aliases scales,Pedigree-method
#' @export
setGeneric("scales", function(object) {
    standardGeneric("scales")
})

setMethod("scales", signature(object = "Pedigree"), function(object) {
    object@scales
})

setGeneric("scales<-", function(object, value) {
    standardGeneric("scales<-")
})

setMethod(
    "scales<-", signature(object = "Pedigree", value = "Scales"),
    function(object, value) {
        object@scales <- value
        object
    }
)
#### S4 fill Accessors ####
#' @description Pedigree fill accessors
#' @param object A Pedigree object.
#' @return The slot `fill` present in the Pedigree object.
#' @rdname extract-methods
#' @aliases fill,Pedigree-method
#' @export
setGeneric("fill", function(object) {
    standardGeneric("fill")
})

setMethod("fill",
    signature(object = "Scales"),
    function(object) {
        object@fill
    }
)

setMethod("fill",
    signature(object = "Pedigree"),
    function(object) {
        fill(scales(object))
    }
)

setGeneric("fill<-", function(object, value) {
    standardGeneric("fill<-")
})

setMethod(
    "fill<-",
    signature(object = "Scales", value = "data.frame"),
    function(object, value) {
        object@fill <- value
        validObject(object)
        object
    }
)

setMethod(
    "fill<-",
    signature(object = "Pedigree", value = "data.frame"),
    function(object, value) {
        fill(scales(object)) <- value
        validObject(object)
        object
    }
)

#### S4 border Accessors ####
#' @description Pedigree border accessors
#' @param object A Pedigree object.
#' @return The slot `border` present in the Pedigree object.
#' @rdname extract-methods
#' @aliases border,Pedigree-method
#' @export
setGeneric("border", function(object) {
    standardGeneric("border")
})

setMethod("border",
    signature(object = "Scales"),
    function(object) {
        object@border
    }
)

setMethod("border",
    signature(object = "Pedigree"),
    function(object) {
        border(scales(object))
    }
)

setGeneric("border<-", function(object, value) {
    standardGeneric("border<-")
})

setMethod(
    "border<-",
    signature(object = "Scales", value = "data.frame"),
    function(object, value) {
        object@border <- value
        validObject(object)
        object
    }
)

setMethod(
    "border<-",
    signature(object = "Pedigree", value = "data.frame"),
    function(object, value) {
        border(scales(object)) <- value
        validObject(object)
        object
    }
)

#### S4 hints Accessors ####
#' @description Pedigree hints accessors
#' @param object A Pedigree object.
#' @return The slot `hints` present in the Pedigree object.
#' @rdname extract-methods
#' @aliases hints,Pedigree-method
#' @export
setGeneric("hints", function(object) {
    standardGeneric("hints")
})

setMethod("hints", signature(object = "Pedigree"), function(object) {
    object@hints
})
setGeneric("hints<-", function(object, value) {
    standardGeneric("hints<-")
})

setMethod("hints<-", signature(object = "Pedigree", value = "Hints"), function(
    object, value
) {
    object@hints <- value
    validObject(object)
    object
})

#### S4 horder Accessors ####
#' @title Pedigree horder accessors
#' @description Pedigree horder accessors
#' @param object A Pedigree object.
#' @return The slot `horder` present in the `Hints` slot of
#' a Pedigree object.
#' @rdname Pedigree
#' @aliases horder,Pedigree-method
#' @export
setGeneric("horder", function(object) {
    standardGeneric("horder")
})

setMethod("horder", "Pedigree", function(object) {
    horder(hints(object))
})

setMethod("horder", "Hints", function(object) {
    object@horder
})

setGeneric("horder<-", function(object, value) {
    standardGeneric("horder<-")
})

setMethod(
    "horder<-",
    signature(object = "Pedigree", value = "ANY"),
    function(object, value) {
        if (length(value) != length(object)) {
            stop(
                "The length of the new value should be: ",
                "equal to the length of the pedigree"
            )
        }
        horder(hints(object)) <- value
        validObject(object)
        object
    }
)

setMethod(
    "horder<-",
    signature(object = "Hints", value = "ANY"),
    function(object, value) {
        object@horder <- value
        validObject(object)
        object
    }
)
#### S4 spouse Accessors ####
#' @description Pedigree spouse accessors
#' @param object A Pedigree object.
#' @return The slot `spouse` present in the `Hints` slot of
#' a Pedigree object.
#' @rdname extract-methods
#' @aliases spouse,Pedigree-method
#' @export
setGeneric("spouse", function(object) {
    standardGeneric("spouse")
})

setMethod("spouse", signature(object = "Pedigree"), function(object) {
    spouse(hints(object))
})

setMethod("spouse", signature(object = "Hints"), function(object) {
    object@spouse
})

setGeneric("spouse<-", function(object, value) {
    standardGeneric("spouse<-")
})

setMethod(
    "spouse<-",
    signature(object = "Pedigree", value = "ANY"),
    function(object, value) {
        spouse(hints(object)) <- value
        validObject(object)
        object
    }
)

setMethod(
    "spouse<-",
    signature(object = "Hints", value = "ANY"),
    function(object, value) {
        # TODO: Check that the spouse matrix is valid
        object@spouse <- value
        validObject(object)
        object
    }
)