#### S4 Accessors ####
#' @include AllConstructor.R
#' @include AllClass.R
#' @importFrom S4Vectors mcols
NULL

#### S4 Ped Accessors ####

#' @section Accessors:
#' For all the following accessors, the `x` parameters is a Ped object.
#' Each getters return a vector of the same length as `x` with the values
#' of the corresponding slot. For each getter, you have a setter with the
#' same name, to be use as `slot(x) <- value`.
#' The `value` parameter is a vector of the same length as `x`, except
#' for the `mcols()` accessors where `value` is a list or a data.frame with
#' each elements with the same length as `x`.

##### Id Accessors #####

#' @rdname Ped-class
#' @usage NULL
#' @export
setGeneric("id", function(x) {
    standardGeneric("id")
})

#' @section Accessors:
#' - `id(x)` : Individuals identifiers
#' @rdname Ped-class
#' @usage NULL
#' @export
setMethod("id", signature(x = "Ped"), function(x) {
    x@id
})

#' @rdname Ped-class
#' @usage NULL
#' @export
setGeneric("id<-", function(x, value) {
    standardGeneric("id<-")
})

#' @rdname Ped-class
#' @usage NULL
#' @export
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

##### Dadid Accessors #####
#' @rdname Ped-class
#' @usage NULL
#' @export
setGeneric("dadid", function(x) {
    standardGeneric("dadid")
})

#' @section Accessors:
#' - `dadid(x)` : Individuals' father identifiers
#' @rdname Ped-class
#' @usage NULL
#' @export
setMethod("dadid", signature(x = "Ped"), function(x) {
    x@dadid
})

#' @rdname Ped-class
#' @usage NULL
#' @export
setGeneric("dadid<-", function(x, value) {
    standardGeneric("dadid<-")
})

#' @rdname Ped-class
#' @usage NULL
#' @export
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

##### Momid Accessors #####

#' @rdname Ped-class
#' @usage NULL
#' @export
setGeneric("momid", function(x) {
    standardGeneric("momid")
})

#' @section Accessors:
#' - `momid(x)` : Individuals' mother identifiers
#' @rdname Ped-class
#' @usage NULL
#' @export
setMethod("momid", signature(x = "Ped"), function(x) {
    x@momid
})

#' @rdname Ped-class
#' @usage NULL
#' @export
setGeneric("momid<-", function(x, value) {
    standardGeneric("momid<-")
})

#' @rdname Ped-class
#' @usage NULL
#' @export
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

##### Famid Accessors #####

#' @rdname Ped-class
#' @usage NULL
#' @export
setGeneric("famid", function(x) {
    standardGeneric("famid")
})

#' @section Accessors:
#' - `famid(x)` : Individuals' family identifiers
#' @rdname Ped-class
#' @usage NULL
#' @export
setMethod("famid", signature(x = "Ped"), function(x) {
    x@famid
})

#' @rdname Ped-class
#' @usage NULL
#' @export
setGeneric("famid<-", function(x, value) {
    standardGeneric("famid<-")
})

#' @rdname Ped-class
#' @usage NULL
#' @export
setMethod("famid<-",
    signature(x = "Ped", value = "character_OR_integer"),
    function(x, value) {
        if (! is.character(value) && ! is.integer(value)) {
            stop("famid must be a character or integer vector")
        }
        if (length(value) != length(x)) {
            stop(
                "The length of the new values for famid should be: ",
                "equal to the length of the Ped object"
            )
        }
        x@famid <- as.character(value)
        x <- upd_famid_id(x)
        validObject(x)
        x
    }
)

##### Sex Accessors #####

#' @rdname Ped-class
#' @usage NULL
#' @export
setGeneric("sex", function(x) {
    standardGeneric("sex")
})

#' @section Accessors:
#' - `sex(x)` : Individuals' gender
#' @rdname Ped-class
#' @usage NULL
#' @export
setMethod("sex", signature(x = "Ped"), function(x) {
    x@sex
})

#' @rdname Ped-class
#' @usage NULL
#' @export
setGeneric("sex<-", function(x, value) {
    standardGeneric("sex<-")
})

#' @rdname Ped-class
#' @usage NULL
#' @export
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

##### Affected Accessors #####

#' @rdname Ped-class
#' @usage NULL
#' @export
setGeneric("affected", function(x) {
    standardGeneric("affected")
})

#' @section Accessors:
#' - `affected(x)` : Individuals' affection status
#' @rdname Ped-class
#' @usage NULL
#' @export
setMethod("affected", signature(x = "Ped"), function(x) {
    x@affected
})

#' @rdname Ped-class
#' @usage NULL
#' @export
setGeneric("affected<-", function(x, value) {
    standardGeneric("affected<-")
})

#' @rdname Ped-class
#' @usage NULL
#' @export
setMethod("affected<-",
    signature(x = "Ped", value = "numeric_OR_logical"),
    function(x, value) {
        if (length(value) != length(x)) {
            if (length(value) == 1) {
                value <- rep(value, length(x))
            } else {
                stop(
                    "The length of the new values for affected should be: ",
                    "equal to the length of the Ped object"
                )
            }
        }
        x@affected <- vect_to_binary(value, logical = TRUE)
        validObject(x)
        x
    }
)

##### Avail Accessors #####

#' @rdname Ped-class
#' @usage NULL
#' @export
setGeneric("avail", function(x) {
    standardGeneric("avail")
})

#' @section Accessors:
#' - `avail(x)` : Individuals' availability status
#' @rdname Ped-class
#' @usage NULL
#' @export
setMethod("avail", signature(x = "Ped"), function(x) {
    x@avail
})

#' @rdname Ped-class
#' @usage NULL
#' @export
setGeneric("avail<-", function(x, value) {
    standardGeneric("avail<-")
})

#' @rdname Ped-class
#' @usage NULL
#' @export
setMethod("avail<-",
    signature(x = "Ped", value = "numeric_OR_logical"),
    function(x, value) {
        if (length(value) != length(x)) {
            if (length(value) == 1) {
                value <- rep(value, length(x))
            } else {
                stop(
                    "The length of the new values for avail should be: ",
                    "equal to the length of the Ped object"
                )
            }
        }
        x@avail <- vect_to_binary(value, logical = TRUE)
        validObject(x)
        x
    }
)

##### Status Accessors #####

#' @rdname Ped-class
#' @usage NULL
#' @export
setGeneric("status", function(x) {
    standardGeneric("status")
})

#' @section Accessors:
#' - `status(x)` : Individuals' death status
#' @rdname Ped-class
#' @usage NULL
#' @export
setMethod("status", signature(x = "Ped"), function(x) {
    x@status
})

#' @rdname Ped-class
#' @usage NULL
#' @export
setGeneric("status<-", function(x, value) {
    standardGeneric("status<-")
})

#' @rdname Ped-class
#' @usage NULL
#' @export
setMethod("status<-",
    signature(x = "Ped", value = "numeric_OR_logical"),
    function(x, value) {
        if (length(value) != length(x)) {
            if (length(value) == 1) {
                value <- rep(value, length(x))
            } else {
                stop(
                    "The length of the new values for avail should be: ",
                    "equal to the length of the Ped object"
                )
            }
        }
        x@status <- vect_to_binary(value, logical = TRUE)
        validObject(x)
        x
    }
)

##### Isinf Accessors #####

#' @rdname Ped-class
#' @usage NULL
#' @export
setGeneric("isinf", function(x) {
    standardGeneric("isinf")
})

#' @section Accessors:
#' - `isinf(x)` : Individuals' informativeness status
#' @rdname Ped-class
#' @usage NULL
#' @export
setMethod("isinf", signature(x = "Ped"), function(x) {
    x@isinf
})

#' @rdname Ped-class
#' @usage NULL
#' @export
setGeneric("isinf<-", function(x, value) {
    standardGeneric("isinf<-")
})

#' @rdname Ped-class
#' @usage NULL
#' @export
setMethod("isinf<-",
    signature(x = "Ped", value = "numeric_OR_logical"),
    function(x, value) {
        if (length(value) != length(x)) {
            if (length(value) == 1) {
                value <- rep(value, length(x))
            } else {
                stop(
                    "The length of the new values for isinf should be: ",
                    "equal to the length of the Ped object"
                )
            }
        }
        x@isinf <- value
        validObject(x)
        x
    }
)

##### Kin Accessors #####

#' @rdname Ped-class
#' @usage NULL
#' @export
setGeneric("kin", function(x) {
    standardGeneric("kin")
})

#' @section Accessors:
#' - `kin(x)` : Individuals' kinship distance to the
#' informative individuals
#' @rdname Ped-class
#' @usage NULL
#' @export
setMethod("kin", signature(x = "Ped"), function(x) {
    x@kin
})

#' @rdname Ped-class
#' @usage NULL
#' @export
setGeneric("kin<-", function(x, value) {
    standardGeneric("kin<-")
})

#' @rdname Ped-class
#' @usage NULL
#' @export
setMethod("kin<-",
    signature(x = "Ped", value = "numeric"),
    function(x, value) {
        if (length(value) != length(x)) {
            if (length(value) == 1) {
                value <- rep(value, length(x))
            } else {
                stop(
                    "The length of the new values for kin should be: ",
                    "equal to the length of the Ped object"
                )
            }
        }
        x@kin <- value
        validObject(x)
        x
    }
)

##### Useful Accessors #####

#' @rdname Ped-class
#' @usage NULL
#' @export
setGeneric("useful", function(x) {
    standardGeneric("useful")
})

#' @section Accessors:
#' - `useful(x)` : Individuals' usefullness status
#' @rdname Ped-class
#' @usage NULL
#' @export
setMethod("useful", signature(x = "Ped"), function(x) {
    x@useful
})

#' @rdname Ped-class
#' @usage NULL
#' @export
setGeneric("useful<-", function(x, value) {
    standardGeneric("useful<-")
})

#' @rdname Ped-class
#' @usage NULL
#' @export
setMethod("useful<-",
    signature(x = "Ped", value = "numeric_OR_logical"),
    function(x, value) {
        if (length(value) != length(x)) {
            if (length(value) == 1) {
                value <- rep(value, length(x))
            } else {
                stop(
                    "The length of the new values for useful should be: ",
                    "equal to the length of the Ped object"
                )
            }
        }
        x@useful <- vect_to_binary(value, logical = TRUE)
        validObject(x)
        x
    }
)

##### Mcols Accessors #####

#' @section Accessors:
#' - `mcols(x)` : Individuals' metadata
#' @rdname Ped-class
#' @usage NULL
#' @export
setMethod("mcols<-",
    signature(x = "Ped", value = "list"),
    function(x, value) {
        mcols(x) <- as(value, "DataFrame")
        x
    }
)

#' @rdname Ped-class
#' @usage NULL
#' @export
setMethod("mcols<-",
    signature(x = "Ped", value = "data.frame"),
    function(x, value) {
        mcols(x) <- as(value, "DataFrame")
        x
    }
)

#### S4 Rel Accessors ####

#' @section Accessors:
#' For all the following accessors, the `x` parameters is a Rel object.
#' Each getters return a vector of the same length as `x` with the values
#' of the corresponding slot.

##### Code Accessors #####
#' @rdname Rel-class
#' @usage NULL
#' @export
setGeneric("code", function(x) {
    standardGeneric("code")
})

#' @section Accessors:
#' - `code(x)` : Relationships' code
#' @rdname Rel-class
#' @usage NULL
#' @export
setMethod("code", signature(x = "Rel"), function(x) {
    x@code
})

##### Id1 Accessors #####
#' @rdname Rel-class
#' @usage NULL
#' @export
setGeneric("id1", function(x) {
    standardGeneric("id1")
})

#' @section Accessors:
#' - `id1(x)` : Relationships' first individuals' identifier
#' @rdname Rel-class
#' @usage NULL
#' @export
setMethod("id1", signature(x = "Rel"), function(x) {
    x@id1
})

##### Id2 Accessors #####

#' @rdname Rel-class
#' @usage NULL
#' @export
setGeneric("id2", function(x) {
    standardGeneric("id2")
})

#' @section Accessors:
#' - `id2(x)` : Relationships' second individuals' identifier
#' @rdname Rel-class
#' @usage NULL
#' @export
setMethod("id2", signature(x = "Rel"), function(x) {
    x@id2
})

#' @section Accessors:
#' - `famid(x)` : Relationships' individuals' family identifier
#' @rdname Rel-class
#' @usage NULL
#' @export
setMethod("famid", signature(x = "Rel"), function(x) {
    x@famid
})

#' @section Accessors:
#' - `famid(x) <- value` : Set the relationships' individuals' family
#' identifier
#'    - `value` : A character or integer vector of the same length as x
#'      with the family identifiers
#' @rdname Rel-class
#' @usage NULL
#' @export
setMethod("famid<-",
    signature(x = "Rel", value = "character_OR_integer"),
    function(x, value) {
        if (! is.character(value) && ! is.integer(value)) {
            stop("famid must be a character or integer vector")
        }
        if (length(value) != length(x)) {
            stop(
                "The length of the new values for famid should be: ",
                "equal to the length of the Ped object"
            )
        }
        x@famid <- as.character(value)
        x <- upd_famid_id(x)
        validObject(x)
        x
    }
)

#### S4 Pedigree Accessors ####

#' @section Accessors:
#' For all the following accessors, the `x` parameters is a Pedigree object.
#' Each getters return a vector of the same length as `x` with the values
#' of the corresponding slot.

#' @section Accessors:
#' - `famid(x)` : Get the family identifiers of a Pedigree object. This
#' function is a wrapper around `famid(ped(x))`.
#'
#' @rdname Pedigree-class
#' @usage NULL
#' @export
setMethod("famid", signature(x = "Pedigree"), function(x) {
    famid(ped(x))
})

##### S4 ped Accessors #####

#' @rdname Pedigree-class
#' @usage NULL
#' @export
setGeneric("ped", function(object, slot) {
    standardGeneric("ped")
})

#' @section Accessors:
#' - `ped(x, slot)` : Get the value of a specific slot of the Ped object
#' @rdname Pedigree-class
#' @usage NULL
#' @export
setMethod(
    "ped",
    signature(object = "Pedigree", slot = "ANY"),
    function(object, slot) {
        slot(object@ped, slot)
    }
)

#' @section Accessors:
#' - `ped(x)` : Get the Ped object
#' @rdname Pedigree-class
#' @usage NULL
#' @export
setMethod(
    "ped",
    signature(object = "Pedigree", slot = "missing"),
    function(object) {
        object@ped
    }
)

#' @rdname Pedigree-class
#' @usage NULL
#' @export
setGeneric("ped<-", function(object, slot, value) {
    standardGeneric("ped<-")
})

#' @section Accessors:
#' - `ped(x, slot) <- value` : Set the value of a specific slot of
#' the Ped object
#' Wrapper of `slot(ped(x)) <- value`
#' @rdname Pedigree-class
#' @usage NULL
#' @export
setMethod(
    "ped<-",
    signature(object = "Pedigree", slot = "ANY", value = "ANY"),
    function(object, slot, value) {
        ped_slots <- c(
            "id", "dadid", "momid", "sex", "famid",
            "steril", "status", "avail", "affected",
            "kin", "useful", "isinf",
            "num_child_tot", "num_child_dir", "num_child_ind"
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

#' @section Accessors:
#' - `ped(x) <- value` : Set the Ped object
#' @rdname Pedigree-class
#' @usage NULL
#' @export
setMethod(
    "ped<-",
    signature(object = "Pedigree", slot = "missing", value = "Ped"),
    function(object, slot, value) {
        object@ped <- value
        validObject(object)
        object
    }
)

##### S4 mcols Accessors #####

#' @section Accessors:
#' - `mcols(x)` : Get the metadata of a Pedigree object.
#' This function is a wrapper around `mcols(ped(x))`.
#' @rdname Pedigree-class
#' @usage NULL
#' @importFrom S4Vectors 'mcols'
#' @export
setMethod(
    "mcols",
    signature(x = "Pedigree"),
    function(x) {
        mcols(x@ped)
    }
)

#' @section Accessors:
#' - `mcols(x) <- value` : Set the metadata of a Pedigree object.
#' This function is a wrapper around `mcols(ped(x)) <- value`.
#' @rdname Pedigree-class
#' @usage NULL
#' @importFrom S4Vectors 'mcols<-'
#' @export
setMethod(
    "mcols<-",
    signature(x = "Pedigree", value = "ANY"),
    function(x, value) {
        mcols(x@ped) <- value
        x
    }
)

##### S4 rel Accessors #####

#' @rdname Pedigree-class
#' @usage NULL
#' @export
setGeneric("rel", function(object, slot) {
    standardGeneric("rel")
})

#' @section Accessors:
#' - `rel(x, slot)` : Get the value of a specific slot of the Rel object
#' @rdname Pedigree-class
#' @usage NULL
#' @export
setMethod(
    "rel",
    signature(object = "Pedigree", slot = "ANY"),
    function(object, slot) {
        slot(object@rel, slot)
    }
)

#' @section Accessors:
#' - `rel(x)` : Get the Rel object
#' @rdname Pedigree-class
#' @usage NULL
#' @export
setMethod(
    "rel",
    signature(object = "Pedigree", slot = "missing"),
    function(object, slot) {
        object@rel
    }
)

#' @rdname Pedigree-class
#' @usage NULL
#' @export
setGeneric("rel<-", function(object, slot, value) {
    standardGeneric("rel<-")
})

#' @section Accessors:
#' - `rel(x, slot) <- value` : Set the value of a specific slot of the
#' Rel object
#' Wrapper of `slot(rel(x)) <- value`
#' @rdname Pedigree-class
#' @usage NULL
#' @export
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

#' @section Accessors:
#' - `rel(x) <- value` : Set the Rel object
#' @rdname Pedigree-class
#' @usage NULL
#' @export
setMethod(
    "rel<-",
    signature(object = "Pedigree", slot = "missing", value = "Rel"),
    function(object, slot, value) {
        object@rel <- value
        validObject(object)
        object
    }
)

##### S4 scales Accessors ####

#' @rdname Pedigree-class
#' @usage NULL
#' @export
setGeneric("scales", function(object) {
    standardGeneric("scales")
})

#' @section Accessors:
#' - `scales(x)` : Get the Scales object
#' @rdname Pedigree-class
#' @usage NULL
#' @export
setMethod("scales", signature(object = "Pedigree"), function(object) {
    object@scales
})

#' @rdname Pedigree-class
#' @usage NULL
#' @export
setGeneric("scales<-", function(object, value) {
    standardGeneric("scales<-")
})

#' @section Accessors:
#' - `scales(x) <- value` : Set the Scales object
#' @rdname Pedigree-class
#' @usage NULL
#' @export
setMethod(
    "scales<-", signature(object = "Pedigree", value = "Scales"),
    function(object, value) {
        object@scales <- value
        object
    }
)

#### S4 fill Accessors ####

#' @rdname Scales-class
#' @usage NULL
#' @export
setGeneric("fill", function(object) {
    standardGeneric("fill")
})

#' @section Accessors:
#' - `fill(x)` : Get the fill data.frame
#' @rdname Scales-class
#' @usage NULL
#' @export
setMethod("fill",
    signature(object = "Scales"),
    function(object) {
        object@fill
    }
)

#' @section Accessors:
#' - `fill(x)` : Get the fill data.frame from the Scales object.
#' Wrapper of `fill(scales(x))`
#' @rdname Pedigree-class
#' @usage NULL
#' @export
setMethod("fill",
    signature(object = "Pedigree"),
    function(object) {
        fill(scales(object))
    }
)

#' @rdname Scales-class
#' @usage NULL
#' @export
setGeneric("fill<-", function(object, value) {
    standardGeneric("fill<-")
})

#' @section Accessors:
#' - `fill(x) <- value` : Set the fill data.frame
#' @rdname Scales-class
#' @usage NULL
#' @export
setMethod(
    "fill<-",
    signature(object = "Scales", value = "data.frame"),
    function(object, value) {
        object@fill <- value
        validObject(object)
        object
    }
)

#' @section Accessors:
#' - `fill(x) <- value` : Set the fill data.frame from the Scales object.
#' Wrapper of `fill(scales(x)) <- value`
#' @rdname Pedigree-class
#' @usage NULL
#' @export
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

#' @rdname Scales-class
#' @usage NULL
#' @export
setGeneric("border", function(object) {
    standardGeneric("border")
})

#' @section Accessors:
#' - `border(x)` : Get the border data.frame
#' @rdname Scales-class
#' @usage NULL
#' @export
setMethod("border",
    signature(object = "Scales"),
    function(object) {
        object@border
    }
)

#' @section Accessors:
#' - `border(x)` : Get the border data.frame from the Scales object.
#' Wrapper of `border(scales(x))`
#' @rdname Pedigree-class
#' @usage NULL
#' @export
setMethod("border",
    signature(object = "Pedigree"),
    function(object) {
        border(scales(object))
    }
)

#' @rdname Scales-class
#' @usage NULL
#' @export
setGeneric("border<-", function(object, value) {
    standardGeneric("border<-")
})

#' @section Accessors:
#' - `border(x) <- value` : Set the border data.frame
#' @rdname Scales-class
#' @usage NULL
#' @export
setMethod(
    "border<-",
    signature(object = "Scales", value = "data.frame"),
    function(object, value) {
        object@border <- value
        validObject(object)
        object
    }
)

#' @section Accessors:
#' - `border(x) <- value` : Set the border data.frame from the Scales object.
#' Wrapper of `border(scales(x)) <- value`
#' @rdname Pedigree-class
#' @usage NULL
#' @export
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

#' @rdname Pedigree-class
#' @usage NULL
#' @export
setGeneric("hints", function(object) {
    standardGeneric("hints")
})

#' @section Accessors:
#' - `hints(x)` : Get the Hints object
#' @rdname Pedigree-class
#' @usage NULL
#' @export
setMethod("hints", signature(object = "Pedigree"), function(object) {
    object@hints
})

#' @rdname Pedigree-class
#' @usage NULL
#' @export
setGeneric("hints<-", function(object, value) {
    standardGeneric("hints<-")
})

#' @section Accessors:
#' - `hints(x) <- value` : Set the Hints object
#' @rdname Pedigree-class
#' @usage NULL
#' @export
setMethod("hints<-", signature(object = "Pedigree", value = "Hints"), function(
    object, value
) {
    object@hints <- value
    validObject(object)
    object
})

#### S4 horder Accessors ####

#' @rdname Hints-class
#' @usage NULL
#' @export
setGeneric("horder", function(object) {
    standardGeneric("horder")
})

#' @section Accessors:
#' - `horder(x)` : Get the horder vector
#' @rdname Hints-class
#' @usage NULL
#' @export
setMethod("horder", "Hints", function(object) {
    object@horder
})

#' @section Accessors:
#' - `horder(x)` : Get the horder vector from the Hints object.
#' Wrapper of `horder(hints(x))`
#' @rdname Pedigree-class
#' @usage NULL
#' @export
setMethod("horder", "Pedigree", function(object) {
    horder(hints(object))
})

#' @rdname Hints-class
#' @usage NULL
#' @export
setGeneric("horder<-", function(object, value) {
    standardGeneric("horder<-")
})

#' @section Accessors:
#' - `horder(x) <- value` : Set the horder vector
#' @rdname Hints-class
#' @usage NULL
#' @export
setMethod(
    "horder<-",
    signature(object = "Hints", value = "ANY"),
    function(object, value) {
        if (length(value) > 0 && is.null(names(value))) {
            stop("horder must be named")
        }
        object@horder <- value
        validObject(object)
        object
    }
)

#' @section Accessors:
#' - `horder(x) <- value` : Set the horder vector from the Hints object.
#' Wrapper of `horder(hints(x)) <- value`
#' @rdname Pedigree-class
#' @usage NULL
#' @export
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

#### S4 spouse Accessors ####

#' @rdname Hints-class
#' @usage NULL
#' @export
setGeneric("spouse", function(object) {
    standardGeneric("spouse")
})

#' @section Accessors:
#' - `spouse(x)` : Get the spouse data.frame
#' @rdname Hints-class
#' @usage NULL
#' @export
setMethod("spouse", signature(object = "Hints"), function(object) {
    object@spouse
})

#' @section Accessors:
#' - `spouse(x)` : Get the spouse data.frame from the Hints object.
#' Wrapper of `spouse(hints(x))`.
#' @rdname Pedigree-class
#' @usage NULL
#' @export
setMethod("spouse", signature(object = "Pedigree"), function(object) {
    spouse(hints(object))
})

#' @rdname Pedigree-class
#' @usage NULL
#' @export
setGeneric("spouse<-", function(object, value) {
    standardGeneric("spouse<-")
})

#' @section Accessors:
#' - `spouse(x) <- value` : Set the spouse data.frame
#' @rdname Hints-class
#' @usage NULL
#' @export
setMethod(
    "spouse<-",
    signature(object = "Hints", value = "data.frame"),
    function(object, value) {
        df <- check_columns(value, c("idl", "idr", "anchor"))
        df$anchor <- anchor_to_factor(df$anchor)
        object@spouse <- df
        validObject(object)
        object
    }
)

#' @section Accessors:
#' - `spouse(x) <- value` : Set the spouse data.frame from the Hints object.
#' Wrapper of `spouse(hints(x)) <- value`.
#' @rdname Pedigree-class
#' @usage NULL
#' @export
setMethod(
    "spouse<-",
    signature(object = "Pedigree", value = "data.frame"),
    function(object, value) {
        spouse(hints(object)) <- value
        validObject(object)
        object
    }
)
