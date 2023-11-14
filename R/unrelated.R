#' @importFrom stats runif
NULL

#' Find Unrelated subjects
#'
#' @description
#' Determine set of maximum number of unrelated available subjects from a
#' Pedigree.
#'
#' @details
#' Determine set of maximum number of unrelated available subjects from a
#' Pedigree, given vectors id, father, and mother for a Pedigree structure, and
#' status vector of `TRUE` / `FALSE` for whether each subject is
#' available (e.g. has DNA).
#'
#' This is a greedy algorithm that uses the kinship matrix, sequentially
#' removing rows/cols that are non-zero for subjects that have the most number
#' of zero kinship coefficients (greedy by choosing a row of kinship matrix
#' that has the most number of zeros, and then remove any cols and their
#' corresponding rows that are non-zero. To account for ties of the count of
#' zeros for rows, a random choice is made. Hence, running this function
#' multiple times can return different sets of unrelated subjects.
#'
#' If **avail** is `NULL`, it is extracted with its
#' corresponding accessor from the Ped object.
#'
#' @inheritParams shrink
#'
#' @return A vector of the ids of subjects that are unrelated.

#' @examples
#'
#' data(sampleped)
#' fam1 <- sampleped[sampleped$famid == 1, ]
#' ped1 <- Pedigree(fam1)
#' unrelated(ped1)
#' ## some possible vectors
#' ## [1] '110' '113' '133' '109'
#' ## [1] '113' '118' '141' '109'
#' ## [1] '113' '118' '140' '109'
#' ## [1] '110' '113' '116' '109'
#' ## [1] '113' '133' '141' '109'
#'
#' @author Dan Schaid and Shannon McDonnell updated by Jason Sinnwell
#' @export
#' @usage NULL
setGeneric("unrelated", signature = "obj",
    function(obj, ...) standardGeneric("unrelated")
)

#' @rdname unrelated
#' @export
setMethod("unrelated", "Ped",
    function(obj, avail = NULL) {
        if (is.null(avail)) {
            avail <- avail(obj)
        }
        # Requires: kinship function

        # Given vectors id, father, and mother for a Pedigree structure,
        # and avail = vector of T/F or 1/0 for whether each subject
        # (corresponding to id vector) is available
        # (e.g., has DNA available), determine set of maximum
        # number of unrelated available subjects from a Pedigree.

        # This is a greedy algorithm that uses the kinship matrix, sequentially
        # removing rows/cols that are non-zero for subjects that have the most
        # number of zero kinship coefficients
        # (greedy by choosing a row of kinship matrix that has the most number
        # of zeros, and then remove any cols and their corresponding rows that
        # are non-zero).
        # To account for ties of the count of zeros for rows, a random choice
        # is made.
        # Hence, running this function multiple times can return different
        # sets of unrelated subjects.

        id <- id(obj)

        kin <- kinship(obj)

        ord <- order(id)
        id <- id[ord]
        avail <- as.logical(avail[ord])
        kin <- kin[ord, ][, ord]

        rord <- order(runif(nrow(kin)))

        id <- id[rord]
        avail <- avail[rord]
        kin <- kin[rord, ][, rord]

        kin_avail <- kin[avail, , drop = FALSE][, avail, drop = FALSE]

        diag(kin_avail) <- 0

        while (any(kin_avail > 0)) {
            nr <- nrow(kin_avail)
            indx <- seq_len(nrow(kin_avail))
            zero_count <- apply(kin_avail == 0, 1, sum)

            mx <- max(zero_count[zero_count < nr])
            zero_mx <- indx[zero_count == mx][1]

            exclude <- indx[kin_avail[, zero_mx] > 0]

            kin_avail <- kin_avail[-exclude, , drop = FALSE][
                , -exclude, drop = FALSE
            ]
        }

        sort(dimnames(kin_avail)[[1]])
    }
)

#' @rdname unrelated
#' @export
setMethod("unrelated", "Pedigree",
    function(obj, avail = NULL) {
        unrelated(ped(obj), avail = avail)
    }
)
