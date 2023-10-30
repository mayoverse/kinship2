#### S4 Accessors ####
#' @include Pedigree.R
#' @include AllClass.R
NULL

#### S4 ped Accessors ####
#' @title Pedigree ped accessors
#' @param object A Pedigree object.
#' @return The slot `ped` present in the Pedigree object.
#' @rdname extract-methods
#' @aliases ped,Pedigree-method
#' @export
setGeneric("ped", function(object, col) {
    standardGeneric("ped")
})

setMethod(
    "ped",
    signature(object = "Pedigree", col = "ANY"),
    function(object, col) {
        object@ped[col]
    }
)

setMethod(
    "ped",
    signature(object = "Pedigree", col = "missing"),
    function(object) {
        object@ped
    }
)

setGeneric("ped<-", function(object, value) {
    standardGeneric("ped<-")
})

setReplaceMethod("ped", signature(object = "Pedigree", value = "Ped"),
    function(object, value) {
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
    signature(object = "Pedigree"),
    function(object) {
        mcols(object@ped)
    }
)

setMethod(
    "mcols<-",
    signature(object = "Pedigree", value = "ANY"),
    function(object) {
        mcols(object@ped) <- value
    }
)

#### S4 rel Accessors ####

#' @description Pedigree rel accessors
#' @param object A Pedigree object.
#' @return The slot `rel` present in the Pedigree object.
#' @rdname extract-methods
#' @aliases rel,Pedigree-method
#' @export
setGeneric("rel", function(object, col) {
    standardGeneric("rel")
})

setMethod(
    "rel",
    signature(object = "Pedigree", col = "ANY"),
    function(object, col) {
        object@rel[col]
    }
)

setMethod(
    "rel",
    signature(object = "Pedigree", col = "missing"),
    function(object, col) {
        object@rel
    }
)

setGeneric("rel<-", function(object, col, value) {
    standardGeneric("rel<-")
})

setMethod(
    "rel<-",
    signature(object = "Pedigree", col = "ANY", value = "ANY"),
    function(object, col, value) {
        rel_cols <- c("id1", "id2", "code", "family")
        if (! col %in% rel_cols) {
            stop("Col selected: ", col, " is not a relationship column")
        }
        if (length(value) != length(object)) {
            stop(
                "The length of the new value should be: ",
                "equal to the length of the pedigree"
            )
        }
        object@rel[col] <- value
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
setGeneric("fill", function(object, col) {
    standardGeneric("fill")
})

setMethod("fill",
    signature(object = "Pedigree", col = "ANY"),
    function(object, col) {
        object@scales$fill[col]
    }
)

setMethod("fill",
    signature(object = "Pedigree", col = "missing"),
    function(object) {
        object@scales$fill
    }
)

setGeneric("fill<-", function(object, col, value) {
    standardGeneric("fill<-")
})

setMethod(
    "fill<-",
    signature(object = "Pedigree", col = "ANY", value = "ANY"),
    function(object, col, value) {
        fill_cols <- c("column", "column_values")
        if (! col %in% fill_cols) {
            stop("Col selected: ", col, " is not a fill column")
        }
        object@scales$fill[col] <- value
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
setGeneric("border", function(object, col) {
    standardGeneric("border")
})

setMethod("border",
    signature(object = "Pedigree", col = "ANY"),
    function(object, col) {
        object@scales$border[col]
    }
)

setGeneric("border<-", function(object, col, value) {
    standardGeneric("border<-")
})

setMethod(
    "border<-",
    signature(object = "Pedigree", col = "ANY", value = "ANY"),
    function(object, col, value) {
        fill_cols <- c("column", "column_values")
        if (! col %in% fill_cols) {
            stop("Col selected: ", col, " is not a border column")
        }
        object@scales$border[col] <- value
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
#' @return The slot `order` present in the Pedigree object.
#' @rdname extract-methods
#' @aliases order,Pedigree-method
#' @export
setGeneric("horder", function(object) {
    standardGeneric("horder")
})

setMethod("horder", signature(object = "Pedigree"), function(object) {
    object@hints$horder
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
        object@hints$horder <- value
        validObject(object)
        object
    }
)
#### S4 spouse Accessors ####
#' @description Pedigree spouse accessors
#' @param object A Pedigree object.
#' @return The slot `spouse` present in the Pedigree object.
#' @rdname extract-methods
#' @aliases spouse,Pedigree-method
#' @export
setGeneric("spouse", function(object) {
    standardGeneric("spouse")
})

setMethod("spouse", signature(object = "Pedigree"), function(object) {
    object@hints$spouse
})

setGeneric("spouse<-", function(object, value) {
    standardGeneric("spouse<-")
})

setMethod(
    "spouse<-",
    signature(object = "Pedigree", value = "ANY"),
    function(object, value) {
        if (length(value) != length(object)) {
            stop(
                "The length of the new value should be: ",
                "equal to the length of the pedigree"
            )
        }
        object@hints$spouse <- value
        validObject(object)
        object
    }
)