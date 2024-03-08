#### S4 Ped generics ####

#' @section Generics:
#' - `summary(x)`: Compute the summary of a Ped object
#' @export
#' @importFrom S4Vectors classNameForDisplay
#' @importFrom S4Vectors summary
#' @rdname Ped-class
#' @usage NULL
setMethod("summary", "Ped",
    function(object) {
        object_class <- classNameForDisplay(object)
        object_len <- length(object)
        object_mcols <- mcols(object, use.names = FALSE)
        object_nmc <- if (is.null(object_mcols)) 0L else ncol(object_mcols)
        paste0(object_class, " object with ", object_len, " ",
            ifelse(object_len == 1L, "individual", "individuals"),
            " and ", object_nmc, " metadata ",
            ifelse(object_nmc == 1L, "column", "columns")
        )
    }
)

#' @section Generics:
#' - `show(x)`: Convert the Ped object to a data.frame
#' and print it with its summary.
#' @export
#' @importFrom S4Vectors cbind_mcols_for_display
#' @importFrom methods show
#' @importFrom S4Vectors makeClassinfoRowForCompactPrinting
#' @rdname Ped-class
#' @usage NULL
setMethod("show", "Ped",
    function(object) {
        cat(summary(object), ":\n", sep = "")
        df <- as.data.frame(object)
        df <- df[, !colnames(df) %in% colnames(mcols(object))]
        out <- S4Vectors::cbind_mcols_for_display(df, object)
        class_df <- lapply(df, class)
        classinfo <- S4Vectors::makeClassinfoRowForCompactPrinting(
            object, class_df
        )
        stopifnot(identical(colnames(classinfo), colnames(out)))
        out <- rbind(classinfo, out)
        print(out, quote = FALSE, right = TRUE)
    }
)

#' @section Generics:
#' - `as.list(x)`: Convert a Ped object to a list with
#' the metadata columns at the end.
#' @rdname Ped-class
#' @importFrom methods slotNames
#' @importFrom S4Vectors as.list
#' @export
#' @usage NULL
setMethod("as.list", "Ped", function(x) {
    to <- list()
    for (slot in slotNames(x)) {
        if (slot %in% c("metadata", "elementMetadata")) {
            next
        } else {
            to[[slot]] <- slot(x, slot)
        }
    }
    # Add the metadata in separate slot
    c(to, as.list(mcols(x)))
})

#' @section Generics:
#' - `as.data.frame(x)`: Convert a Ped object to a data.frame with
#' the metadata columns at the end.
#' @rdname Ped-class
#' @importFrom S4Vectors as.data.frame
#' @export
#' @usage NULL
setMethod("as.data.frame", "Ped", function(x) {
    lst <- as.list(x)
    if (length(unique(lapply(lst, length))) != 1) {
        stop("All slots should have the same length")
    }
    ped_df <- data.frame(lst)
    rownames(ped_df) <- ped_df$id
    ped_df
})

#' @section Generics:
#' - `subset(x, i, del_parents = FALSE, keep = TRUE)`: Subset a Ped object
#' based on the individuals identifiers given.
#'      - `i` : A vector of individuals identifiers to keep.
#'      - `del_parents` : A logical value indicating if the parents
#'      of the individuals should be deleted.
#'      - `keep` : A logical value indicating if the individuals
#'      should be kept or deleted.
#' @rdname Ped-class
#' @importFrom S4Vectors subset
#' @export
#' @usage NULL
setMethod("subset", "Ped", function(x, i, del_parents = FALSE, keep = TRUE) {
    if (is.factor(i)) {
        i <- as.character(i)
    }
    if (is.character(i)) {
        i <- x@id %in% i
    } else if (!is.numeric(i) & !is.logical(i)) {
        stop("i must be a character, an integer or a logical vector")
    }
    if (!keep) {
        i <- !i
    }
    col_computed <- c(
        "num_child_tot", "num_child_dir", "num_child_ind"
    )
    ped_df <- as.data.frame(x)[i, ]
    ped_df <- ped_df[, ! colnames(ped_df) %in% col_computed]

    if (del_parents) {
        ped_df$dadid[!ped_df$dadid %in% ped_df$id] <- NA_character_
        ped_df$momid[!ped_df$momid %in% ped_df$id] <- NA_character_
    }
    new_ped <- Ped(ped_df)
    validObject(new_ped)
    new_ped
})

#### S4 Rel generics ####

#' @section Generics:
#' - `summary(x)`: Compute the summary of a Rel object
#' @export
#' @importFrom S4Vectors classNameForDisplay
#' @importFrom S4Vectors summary
#' @rdname Rel-class
#' @usage NULL
setMethod("summary", "Rel",
    function(object) {
        object_class <- classNameForDisplay(object)
        object_len <- length(object)
        tbl <- table(object@code)
        if (length(tbl) == 0L) {
            sum_codes <- 0L
        } else {
            sum_codes <- paste0(
                paste(tbl, levels(object@code)), collapse = ", "
            )
        }
        paste0(object_class, " object with ", object_len, " ",
            ifelse(object_len == 1L, "relationship", "relationships"),
            ifelse(sum_codes == 0L, "", paste0("with ", sum_codes))
        )
    }
)

#' @section Generics:
#' - `show(x)`: Convert the Rel object to a data.frame
#' and print it with its summary.
#' @export
#' @importFrom S4Vectors cbind_mcols_for_display
#' @importFrom S4Vectors makeClassinfoRowForCompactPrinting
#' @importFrom methods show
#' @rdname Rel-class
#' @usage NULL
setMethod("show", signature(object = "Rel"),
    function(object) {
        cat(summary(object), ":\n", sep = "")
        df <- as.data.frame(object)
        df <- df[, !colnames(df) %in% colnames(mcols(object))]
        out <- S4Vectors::cbind_mcols_for_display(df, object)
        class_df <- lapply(df, class)
        classinfo <- S4Vectors::makeClassinfoRowForCompactPrinting(
            object, class_df
        )
        stopifnot(identical(colnames(classinfo), colnames(out)))
        out <- rbind(classinfo, out)
        print(out, quote = FALSE, right = TRUE)
    }
)

#' @section Generics:
#' - `as.list(x)`: Convert a Rel object to a list
#' @rdname Rel-class
#' @importFrom S4Vectors as.list
#' @export
#' @usage NULL
setMethod("as.list", "Rel", function(x) {
    to <- list()
    for (slot in slotNames(x)) {
        if (slot %in% c("metadata", "elementMetadata")) {
            next
        } else {
            to[[slot]] <- slot(x, slot)
        }
    }
    # Add the metadata in separate slot
    c(to, as.list(mcols(x)))
})

#' @section Generics:
#' - `as.data.frame(x)`: Convert a Rel object to a data.frame
#' @rdname Rel-class
#' @importFrom S4Vectors as.data.frame
#' @export
#' @usage NULL
setMethod("as.data.frame", "Rel", function(x) {
    lst <- as.list(x)
    if (length(unique(lapply(lst, length))) != 1) {
        stop("All slots should have the same length")
    }
    data.frame(lst)
})

#' @section Generics:
#' - `subset(x, i, keep = TRUE)`: Subset a Rel object
#' based on the individuals identifiers given.
#'      - `i` : A vector of individuals identifiers to keep.
#'      - `keep` : A logical value indicating if the individuals
#'      should be kept or deleted.
#' @rdname Rel-class
#' @importFrom S4Vectors subset
#' @export
#' @usage NULL
setMethod("subset", "Rel", function(x, idlist, keep = TRUE) {
    if (is.factor(idlist)) {
        idlist <- as.character(idlist)
    }
    if (! is.character(idlist)) {
        stop("idlist must be a character")
    }

    if (! keep) {
        id_all <- c(id1(x), id2(x))
        idlist <- setdiff(id_all, idlist)
    }

    rel_df <- as.data.frame(x)

    id1 <- rel_df$id1 %in% idlist
    id2 <- rel_df$id2 %in% idlist
    rel_df <- rel_df[id1 & id2, ]
    new_rel <- Rel(rel_df)
    validObject(new_rel)
    new_rel
})

#### S4 Hints generics ####

#' @section Generics:
#' - `as.list(x)`: Convert a Hints object to a list
#' @rdname Hints-class
#' @importFrom S4Vectors as.list
#' @export
#' @usage NULL
setMethod("as.list", "Hints", function(x) {
    list(horder = x@horder, spouse = x@spouse)
})

#' @section Generics:
#' - `subset(x, i, keep = TRUE)`: Subset a Hints object
#' based on the individuals identifiers given.
#'      - `i` : A vector of individuals identifiers to keep.
#'      - `keep` : A logical value indicating if the individuals
#'      should be kept or deleted.
#' @rdname Hints-class
#' @importFrom S4Vectors subset
#' @export
#' @usage NULL
setMethod("subset", "Hints", function(x, idlist, keep = TRUE) {
    horder <- horder(x)
    spouse <- spouse(x)

    if (is.factor(idlist)) {
        idlist <- as.character(idlist)
    }
    if (! is.character(idlist)) {
        stop("idlist must be a character")
    }

    if (length(horder) > 0) {
        if (! keep) {
            idlist <- setdiff(names(horder), idlist)
        }
        horder <- horder[names(horder) %in% idlist]
    }

    if (nrow(spouse) > 0) {
        if (! keep) {
            id_all <- c(spouse$idl, spouse$idr)
            idlist <- setdiff(id_all, idlist)
        }
        spouse <- spouse[spouse$idl %in% idlist & spouse$idr %in% idlist, ]
    }
    new_hints <- Hints(horder = horder, spouse = spouse)
    validObject(new_hints)
    new_hints
})

#### S4 Scales generics ####

#' @section Generics:
#' - `as.list(x)`: Convert a Scales object to a list
#' @rdname Scales-class
#' @importFrom S4Vectors as.list
#' @export
#' @usage NULL
setMethod("as.list", "Scales", function(x) {
    list(fill = x@fill, border = x@border)
})

#### S4 Pedigree generics ####

#' @section Generics:
#' - `length(x)`: Get the length of a Pedigree object.
#' Wrapper of `length(ped(x))`.
#' @rdname Pedigree-class
#' @export
#' @usage NULL
setMethod("length", c(x = "Pedigree"),
    function(x) {
        length(ped(x))
    }
)

#' @section Generics:
#' - `show(x)`: Print the information of the Ped and Rel
#' object inside the Pedigree object.
#' @export
#' @importFrom methods show
#' @rdname Pedigree-class
#' @usage NULL
setMethod("show", signature(object = "Pedigree"), function(object) {
    cat("Pedigree object with: \n")
    print(ped(object))
    print(rel(object))
})

#' @section Generics:
#' - `summary(x)`: Compute the summary of the Ped and Rel object
#' inside the Pedigree object.
#' @export
#' @rdname Pedigree-class
#' @usage NULL
setMethod("summary", signature(object = "Pedigree"), function(object) {
    cat("Pedigree object with \n")
    print(summary(ped(object)))
    print(summary(rel(object)))
})

#' @section Generics:
#' - `as.list(x)`: Convert a Pedigree object to a list
#' @rdname Pedigree-class
#' @importFrom S4Vectors as.list
#' @export
#' @usage NULL
setMethod("as.list", "Pedigree", function(x) {
    list(
        ped = as.list(ped(x)),
        rel = as.list(rel(x)),
        scales = as.list(scales(x)),
        hints = as.list(hints(x))
    )
})

#' @section Generics:
#' - `subset(x, i, keep = TRUE)`: Subset a Pedigree object
#' based on the individuals identifiers given.
#'      - `i` : A vector of individuals identifiers to keep.
#'      - `del_parents` : A logical value indicating if the parents
#'      of the individuals should be deleted.
#'      - `keep` : A logical value indicating if the individuals
#'      should be kept or deleted.
#' @rdname Pedigree-class
#' @importFrom S4Vectors subset
#' @export
#' @usage NULL
setMethod("subset", "Pedigree",
    function(x, i, del_parents = FALSE, keep = TRUE) {
        new_ped <- subset(ped(x), i, del_parents = del_parents, keep = keep)
        all_id <- id(new_ped)
        new_rel <- subset(rel(x), all_id)
        new_hints <- subset(hints(x), all_id)

        new_pedi <- new("Pedigree",
            ped = new_ped, rel = new_rel,
            hints = new_hints, scales = scales(x)
        )
        validObject(new_pedi)
        new_pedi
    }
)

#' @section Generics:
#' - `x[i, del_parents, keep]`: Subset a Pedigree object
#' based on the individuals identifiers given.
#' @rdname Pedigree-class
#' @importFrom S4Vectors subset
#' @export
#' @usage NULL
setMethod("[", c(x = "Pedigree", i = "ANY", j = "missing"),
    function(x, i, j, del_parents = FALSE, keep = TRUE, drop = TRUE) {
        subset(x, i, del_parents, keep)
    }
)
