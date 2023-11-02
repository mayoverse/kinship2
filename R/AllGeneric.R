#### S4 Ped generics ####

#' Summary function of Ped object
#'
#' @description Compute the summary of a Ped object
#'
#' @param object A Ped object.
#'
#' @return A character vector with the summary of the object.
#'
#' @export
#' @importFrom S4Vectors classNameForDisplay
#' @importFrom S4Vectors summary
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

#' Show function of Ped object
#'
#' @description Convert the Ped object to a data.frame
#' and print it with its summary.
#'
#' @param object A Ped object.
#'
#' @return The Ped object with the individuals informations.
#'
#' @export
#' @importFrom S4Vectors cbind_mcols_for_display
#' @importFrom S4Vectors makeClassinfoRowForCompactPrinting
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

#' @title Ped object to list
#' @description Convert a Ped object to a list
#' @param from A Ped object.
#' @return A list with the individuals informations.
#' The metadata are put at the end.
#' @rdname extract-methods
#' @aliases as.list,Ped-method
#' @importFrom S4Vectors as.list
#' @export
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

#' @title Ped object to data.frame
#' @description Convert a Ped object to a data.frame
#' @param from A Ped object.
#' @return A data.frame with the individuals informations.
#' The metadata are put at the end.
#' @rdname extract-methods
#' @aliases as.data.frame,Ped-method
#' @importFrom S4Vectors as.data.frame
#' @export
setMethod("as.data.frame", "Ped", function(x) {
    lst <- as.list(x)
    if (length(unique(lapply(lst, length))) != 1) {
        stop("All slots should have the same length")
    }
    ped_df <- data.frame(lst)
    rownames(ped_df) <- ped_df$id
    ped_df
})

#' Subset a Ped object
#'
#' @description Subset a Ped object based on the individuals
#' identifiers given.
#'
#' @param x A Ped object.
#' @param i A vector of individuals identifiers to keep.
#'
#' @return A Ped object subsetted.
#'
#' @rdname extract-methods
#' @aliases subset,Ped-method
#' @importFrom S4Vectors subset
#' @export
setMethod("subset", "Ped", function(x, i, del_parents = FALSE) {
    if (is.factor(i)) {
        i <- as.character(i)
    }
    if (is.character(i)) {
        i <- which(x@id %in% i)
    } else if (!is.numeric(i) & !is.logical(i)) {
        stop("i must be a character, an integer or a logical vector")
    }
    col_computed <- c(
        "num_child_total", "num_child_direct", "num_child_indirect"
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
#' Summary function of Rel object
#'
#' @description Compute the summary of a Rel object
#'
#' @param object A Rel object.
#'
#' @return A character vector with the summary of the object.
#'
#' @export
#' @importFrom S4Vectors classNameForDisplay
#' @importFrom S4Vectors summary
#' @rdname Rel
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

#' Show function of Rel object
#'
#' @description Convert the Rel object to a data.frame
#' and print it with its summary.
#'
#' @param object A Rel object.
#'
#' @return The Rel object with the relationship informations.
#'
#' @export
#' @importFrom S4Vectors cbind_mcols_for_display
#' @importFrom S4Vectors makeClassinfoRowForCompactPrinting
#' @rdname Rel
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

#' @title Rel object to list
#' @description Convert a Rel object to a list
#' @param from A Rel object.
#' @return A list with the relationship informations.
#' @rdname Rel
#' @aliases as.list,Rel-method
#' @importMethodsFrom S4Vectors as.list
#' @export
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

#' @title Rel to data.frame
#' @description Convert a Rel object to a data.frame
#' @param from A Rel object.
#' @return A data.frame with the relationship informations.
#' @rdname Rel
#' @aliases as.data.frame,Rel-method
#' @importFrom S4Vectors as.data.frame
#' @export
setMethod("as.data.frame", "Rel", function(x) {
    lst <- as.list(x)
    if (length(unique(lapply(lst, length))) != 1) {
        stop("All slots should have the same length")
    }
    data.frame(lst)
})

#' Subset a Rel object
#'
#' @description Subset a Rel object based on the individuals
#' identifiers given.
#'
#' @param x A Rel object.
#' @param idlist A vector of individuals identifiers to keep.
#'
#' @return A Rel object subsetted.
#'
#' @rdname extract-methods
#' @aliases subset,Rel-method
#' @importFrom S4Vectors subset
#' @export
setMethod("subset", "Rel", function(x, idlist) {
    if (is.factor(idlist)) {
        idlist <- as.character(idlist)
    }
    if (! is.character(idlist)) {
        stop("idlist must be a character")
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
#' Set Hints object to list
#'
#' @description Convert a Hints object to a list
#'
#' @param from A Hints object.
#'
#' @return A list with the hints informations.
#'
#' @rdname Hints
#' @aliases as.list,Hints-method
#' @importMethodsFrom S4Vectors as.list
#' @export
setMethod("as.list", "Hints", function(x) {
    list(horder = x@horder, spouse = x@spouse)
})

#' Hints subscripting
#' @description Subset the Hints object based on the identifiers
#' given
#' @param hints A Hints object
#' @param idlist A vector of identifiers to subset
#' @return A list of Hints object subsetted
#' @rdname extract-methods
#' @aliases subset_hints,Hints-method
#' @keywords internal
setMethod("subset", "Hints", function(x, idlist) {
    horder <- horder(x)
    spouse <- spouse(x)

    if (is.factor(idlist)) {
        idlist <- as.character(idlist)
    }
    if (! is.character(idlist)) {
        stop("idlist must be a character")
    }

    if (length(horder) > 0) {
        horder <- horder[names(horder) %in% idlist]
    }

    if (nrow(spouse) > 0) {
        spouse <- spouse[spouse$idl %in% idlist & spouse$idr %in% idlist, ]
    }
    new_hints <- Hints(horder = horder, spouse = spouse)
    validObject(new_hints)
    new_hints
})

#### S4 Scales generics ####
#' Set Scales object to list
#'
#' @description Convert a Scales object to a list
#'
#' @param from A Scales object.
#'
#' @return A list with the hints informations.
#'
#' @rdname Scales
#' @aliases as.list,Scales-method
#' @importMethodsFrom S4Vectors as.list
#' @export
setMethod("as.list", "Scales", function(x) {
    list(fill = x@fill, border = x@border)
})

#### S4 Pedigree generics ####

#' Compute the length of a Pedigree object
#' @param x A Pedigree object.
#' @return The number of individuals in the Pedigree object.
#' @docType methods
#' @aliases length,Pedigree-method
#' @export
setMethod("length", c(x = "Pedigree"),
    function(x) {
        length(ped(x))
    }
)
#' @title Pedigree methods
#' @description Pedigree show method
#' @param object A Pedigree object.
#' @return A character vector with the informations about the object.
#' @rdname extract-methods
#' @aliases show,Pedigree-method
setMethod("show", signature(object = "Pedigree"), function(object) {
    cat("Pedigree object with: \n")
    print(ped(object))
    print(rel(object))
})

#' @description Pedigree summary method.
#' @param object A Pedigree object.
#' @return A character vector with the summary of the object.
#' @rdname extract-methods
#' @aliases summary,Pedigree-method
setMethod("summary", signature(object = "Pedigree"), function(object) {
    cat("Pedigree object with \n")
    print(summary(ped(object)))
    print(summary(rel(object)))
})

#' Convert a Pedigree object to a list
#'
#' @description Convert a Pedigree object to a list
#'
#' @param from A Pedigree object.
#'
#' @return A list with the individuals informations.
#'
#' @rdname extract-methods
#' @aliases as.list,Pedigree-method
#' @importMethodsFrom S4Vectors as.list
#' @export
setMethod("as.list", "Pedigree", function(x) {
    list(
        ped = as.list(ped(x)),
        rel = as.list(rel(x)),
        scales = as.list(scales(x)),
        hints = as.list(hints(x))
    )
})

#' @description Extract parts of a Pedigree object
#' @param x A Pedigree object.
#' @param i A vector of individuals id or a vector of index.
#' @param j A vector of columns names.
#' @param drop A logical value indicating if the dimensions should be dropped.
#' @return A Pedigree object subsetted.
#' @rdname extract-methods
setMethod("[", c(x = "Pedigree", i = "ANY", j = "missing"),
    function(x, i, j, drop = TRUE) {
        new_ped <- subset(ped(x), i)
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