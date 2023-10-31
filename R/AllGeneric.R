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
    nb_fam <- length(levels(as.factor(famid(ped(object)))))
    cat("Pedigree object with", length(ped(object)), "individuals and",
        length(rel(object)), "special relationships across", nb_fam, "families",
        fill = TRUE)
})

#' @description Pedigree summary method.
#' @param object A Pedigree object.
#' @return A character vector with the summary of the object.
#' @rdname extract-methods
#' @aliases summary,Pedigree-method
setMethod("summary", signature(object = "Pedigree"), function(object) {
    cat("Pedigree object with", nrow(object@ped), "individuals", fill = TRUE)
    print(summary(object@ped, maxsum = 5))
    cat("and", nrow(object@rel), "special relationships.", fill = TRUE)
    print(summary(object@rel))
    cat("The filling scales columns are:",
        levels(as.factor(object@scales$fill$column_values)), fill = TRUE
    )
    cat("The border scale column are:",
        levels(as.factor(object@scales$border$column)), fill = TRUE
    )
})

## Subscripting
#' @description Subset the hints list based on the index given
#' @param hints A list of hints
#' @param index A vector of index
#' @return A list of hints subsetted
#' @rdname extract-methods
#' @aliases sub_sel_hints,Pedigree-method
#' @keywords internal
sub_sel_hints <- function(hints, index) {
    if (!is.null(hints$horder)) {
        temp <- list(horder = hints$horder[index])
    } else {
        temp <- list(horder = NULL)
    }

    if (!is.null(hints$spouse)) {
        indx1 <- match(hints$spouse[, 1], index, nomatch = 0)
        indx2 <- match(hints$spouse[, 2], index, nomatch = 0)
        keep <- (indx1 > 0 & indx2 > 0)  # keep only if both id's are kept
        if (any(keep)) {
            temp$spouse <- cbind(indx1[keep], indx2[keep],
                hints$spouse[keep, 3]
            )
        }
    } else {
        temp$spouse <- NULL
    }
    temp
}

#' @description Extract parts of a Pedigree object
#' @param x A Pedigree object.
#' @param i A vector of individuals id or a vector of index.
#' @param j A vector of columns names.
#' @param drop A logical value indicating if the dimensions should be dropped.
#' @return A Pedigree object subsetted.
#' @rdname extract-methods
setMethod("[", c(x = "Pedigree", i = "ANY", j = "missing"),
    function(x, i, j, drop = TRUE) {
        if (is.factor(i)) {
            i <- as.character(i)
        }
        if (is.character(i)) {
            i <- which(x$ped$id %in% i)
        }
        ped_df <- x$ped[i, ]
        allId <- unique(c(ped_df$id, ped_df$dadid, ped_df$momid))
        rel_df <- x$rel[x$rel$id1 %in% allId & x$rel$id2 %in% allId, ]
        idx <- match(allId, ped_df$id, nomatch = 0)
        sub_hints <- sub_sel_hints(hints(x), idx)
        new_ped <- Pedigree(ped_df, rel_df, x$scales,
            hints = sub_hints, cols_ren_ped = NULL, normalize = FALSE
        )
        validObject(new_ped)
        new_ped
    }
)