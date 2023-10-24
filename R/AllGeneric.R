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

#' Convert a Pedigree object to a data.frame
#' @param x A Pedigree object.
#' @return A data.frame with the individuals informations.
#' @docType methods
#' @aliases as.data.frame,Pedigree-method
#' @rdname extract-methods
#' @export
setAs("ped", "data.frame", function(object) {
    data.frame(as.list(object))
})

#' @title Pedigree methods
#' @description Pedigree show method
#' @param object A Pedigree object.
#' @return A character vector with the informations about the object.
#' @rdname extract-methods
#' @aliases show,Pedigree-method
setMethod("show", signature(object = "Pedigree"), function(object) {
    nb_fam <- length(levels(as.factor(object@ped$family)))
    cat("Pedigree object with", nrow(object@ped), "individuals and",
        nrow(object@rel), "special relationships across", nb_fam, "families",
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

#### Subscripting ####
#' @description Subset the hints list based on the index given
#' @param hints A list of hints
#' @param index A vector of index
#' @return A list of hints subsetted
#' @rdname extract-methods
#' @aliases sub_sel_hints,Pedigree-method
#' @keywords internal
sub_sel_hints <- function(hints, index) {
    if (!is.null(hints$order)) {
        temp <- list(order = hints$order[index])
    } else {
        temp <- list(order = NULL)
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