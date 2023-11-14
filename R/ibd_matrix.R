# Repack IBD data as a Matrix object
#' @importFrom Matrix sparseMatrix
NULL

#' IBD matrix
#'
#' @description
#' Transform identity by descent (IBD) matrix data from the form produced by
#' external programs such as SOLAR into the compact form used by the coxme and
#' lmekin routines.
#'
#' @details
#' The IBD matrix for a set of n subjects will be an n by n symmetric matrix
#' whose i,j element is the contains, for some given genetic location, a 0/1
#' indicator of whether 0, 1/2 or 2/2 of the alleles for i and j are identical
#' by descent.  Fractional values occur if the IBD fraction must be imputed.
#' The diagonal will be 1.  Since a large fraction of the values will be zero,
#' programs such as Solar return a data set containing only the non-zero
#' elements.  As well, Solar will have renumbered the subjects as seq_len(n)
#' in such a way that families are grouped together in the matrix; a separate
#' index file contains the mapping between this new id and the original one.
#' The final matrix should be labeled with the original identifiers.
#'
#' @param id1 A character vector with the id of the first individuals of each
#' pairs
#' @param id2 A character vector with the id of the second individuals of each
#' pairs
#' @param ibd the IBD value for that pair
#' @param idmap an optional 2 column matrix or data frame whose first element
#' is the internal value (as found in `id1` and `id2`, and whose
#' second element will be used for the dimnames of the result
#' @param diagonal optional value for the diagonal element. If present, any
#' missing diagonal elements in the input data will be set to this value.
#'
#' @return a sparse matrix of class `dsCMatrix`.  This is the same form
#' used for kinship matrices.
#'
#' @seealso [kinship()]
#' @export
ibd_matrix <- function(id1, id2, ibd, idmap, diagonal) {
    if (!is.null(ncol(id1)) && ncol(id1) == 1)
        id1 <- id1[, 1]
    if (!is.null(ncol(id1))) {
        # can be a matrix or a data frame
        if (ncol(id1) != 3) {
            stop("Argument id1 is a matrix or dataframe",
                "but does not have 3 columns"
            )
        }
        if (!missing(id2)) {
            stop("First argument is a matrix or dataframe,",
                "but id2 argument is present"
            )
        }
        if (!missing(ibd)) {
            stop("First argument is a matrix or dataframe,",
                "but ibd argument is present"
            )
        }
        id2 <- id1[, 2]
        ibd <- id1[, 3]
        id1 <- id1[, 1]
    }

    # Reset each pair such that id1 <= id2
    temp <- pmin(id1, id2)
    id2 <- pmax(id1, id2)
    id1 <- temp

    # Add diagonal elements
    if (!missing(diagonal) && diagonal != 0) {
        idlist <- unique(c(id1, id2))
        id1 <- c(id1, idlist)
        id2 <- c(id2, idlist)
        ibd <- c(ibd, rep(diagonal, length(idlist)))
    }

    # Toss away any zeros and duplicates
    keep <- (ibd != 0 & !duplicated(cbind(id1, id2)))
    if (!all(keep)) {
        id1 <- id1[keep]
        id2 <- id2[keep]
        ibd <- ibd[keep]
    }


    # If the set of ids is a list of integers 1-n for some n, we'll assume the
    # the data is already in the optimal order.  Otherwise figure out families.
    # I expect the latter to happen rarely to never.
    temp <- sort(unique(id1, id2))
    maxid <- max(id1, id2)
    if (maxid != as.integer(maxid) ||
            length(temp) != maxid ||
            any(is.na(match(temp, seq_len(maxid))))
    ) {
        # drat, need to figure out family blocks
        idlist <- sort(unique(c(id1, id2)))
        nid <- length(idlist)
        id1 <- match(id1, idlist)
        id2 <- match(id2, idlist)
        famid <- seq_len(nid)  # everyone a singleton
        indx1 <- sort(unique(id2))  # the result vector for remap1 below
        indx2 <- sort(unique(id1))
        while (1) {
            remap1 <- tapply(famid[id1], id2, min)  # map each id2 to min id1
            if (all(famid[indx1] == remap1))
                break
            famid[indx1] <- remap1
            remap2 <- tapply(famid[id2], id1, min)
            famid[indx2] <- remap2
        }
        remap <- (seq_len(nid))[order(famid)]  # reordering of subjects
        id1 <- match(id1, remap)
        id2 <- match(id2, remap)
        idlist <- idlist[remap]
    } else {
        idlist <- seq_len(maxid)
    }

    # dimid will be the dimnames
    if (missing(idmap)) {
        dimid <- idlist
    } else {
        if (!is.null(dim(idmap)) || ncol(idmap) != 2) {
            stop("idmap must have 2 columns")
        }
        temp <- match(idlist, idmap[, 1])
        if (any(is.na(temp))) {
            stop("Values appear in id1 or id2 that are not in idmap")
        }
        dimid <- idmap[temp, 2]
    }

    sparseMatrix(i = id1, j = id2, p = ibd, symmetric = TRUE,
        dimnames = list(dimid, dimid)
    )
}
