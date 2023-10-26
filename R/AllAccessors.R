#### S4 Accessors ####
#' @include Pedigree.R
#' @include AllClass.R
#' @importFrom S4Vectors mcols
NULL

#### S4 Ped Accessors ####
## metadata Accessors ##
#' @title Pedigree metadata accessors
#' @param object A Pedigree object.
#' @return The metadata present in the Pedigree object.
#' @rdname extract-methods
#' @aliases mcols,Ped-method
#' @export
setMethod(
    "mcols",
    signature(x = "Ped"),
    function(x) {
        x@elementMetadata
    }
)

#' @title Pedigree metadata accessors
#' @param object A Pedigree object.
#' @return The metadata present in the Pedigree object.
#' @rdname extract-methods
#' @aliases mcols<-,Ped-method
#' @export
#' @importFrom S4Vectors 'mcols<-'
setMethod(
    "mcols<-",
    signature(x = "Ped", value = "ANY"),
    function(x, value) {
        if (is.data.frame(value)) {
            if (nrow(value) != length(x)) {
                stop(
                    "The number of rows of the new value should be: ",
                    "equal to the length of the pedigree"
                )
            }
        } else if (is.list(value)) {
            lglst <- lapply(value, length)
            if (any(lglst != length(x))) {
                stop(
                    "The length of the element(s) '",
                    names(lglst[lglst != length(x)]),
                    "' should be: ",
                    "equal to the length of the pedigree"
                )
            }
            value <- as.data.frame(value)
        }
        x@elementMetadata <- value
        x
    }
)

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
            "id", "dadid", "momid", "sex", "family",
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
        rel_cols <- c("id1", "id2", "code", "family")
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
#### S4 fill Accessors ####
#' @description Pedigree fill accessors
#' @param object A Pedigree object.
#' @return The slot `fill` present in the Pedigree object.
#' @rdname extract-methods
#' @aliases fill,Pedigree-method
#' @export
setGeneric("fill", function(object, slot) {
    standardGeneric("fill")
})

setMethod("fill",
    signature(object = "Pedigree", slot = "ANY"),
    function(object, slot) {
        object@scales$fill[slot]
    }
)

setMethod("fill",
    signature(object = "Pedigree", slot = "missing"),
    function(object) {
        object@scales$fill
    }
)

setGeneric("fill<-", function(object, slot, value) {
    standardGeneric("fill<-")
})

setMethod(
    "fill<-",
    signature(object = "Pedigree", slot = "ANY", value = "ANY"),
    function(object, slot, value) {
        fill_cols <- c("column", "column_values")
        if (! slot %in% fill_cols) {
            stop("slot selected: ", slot, " is not a fill column")
        }
        object@scales$fill[slot] <- value
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
setGeneric("border", function(object, slot) {
    standardGeneric("border")
})

setMethod("border",
    signature(object = "Pedigree", slot = "ANY"),
    function(object, slot) {
        object@scales$border[slot]
    }
)

setGeneric("border<-", function(object, slot, value) {
    standardGeneric("border<-")
})

setMethod(
    "border<-",
    signature(object = "Pedigree", slot = "ANY", value = "ANY"),
    function(object, slot, value) {
        fill_cols <- c("column", "column_values")
        if (! slot %in% fill_cols) {
            stop("slot selected: ", slot, " is not a border column")
        }
        object@scales$border[slot] <- value
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
#### S4 order Accessors ####
#' @description Pedigree order accessors
#' @param object A Pedigree object.
#' @return The slot `order` present in the `Hints` slot of
#' a Pedigree object.
#' @rdname extract-methods
#' @aliases order,Pedigree-method
#' @export
setGeneric("order", function(object) {
    standardGeneric("order")
})

setMethod("order", signature(object = "Pedigree"), function(object) {
    object@hints@order
})

setGeneric("order<-", function(object, value) {
    standardGeneric("order<-")
})

setMethod(
    "order<-",
    signature(object = "Pedigree", value = "ANY"),
    function(object, value) {
        if (length(value) != length(object)) {
            stop(
                "The length of the new value should be: ",
                "equal to the length of the pedigree"
            )
        }
        object@hints@order <- value
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
    object@hints@spouse
})

setGeneric("spouse<-", function(object, value) {
    standardGeneric("spouse<-")
})

setMethod(
    "spouse<-",
    signature(object = "Pedigree", value = "ANY"),
    function(object, value) {
        # TODO: Check that the spouse matrix is valid
        object@hints@spouse <- value
        validObject(object)
        object
    }
)