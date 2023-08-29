# Automatically generated from all.nw using noweb Authors: Dan Schaid, Shannon
# McDonnell Updated by Jason Sinnwell

#' Get unrelated subjects
#'
#' @description
#' Determine set of maximum number of unrelated available subjects from a
#' pedigree
#'
#' @details
#' Determine set of maximum number of unrelated available subjects from a
#' pedigree, given vectors id, father, and mother for a pedigree structure, and
#' status vector of T/F for whether each subject is available (e.g. has DNA)
#'
#' This is a greedy algorithm that uses the kinship matrix, sequentially
#' removing rows/cols that are non-zero for subjects that have the most number
#' of zero kinship coefficients (greedy by choosing a row of kinship matrix
#' that has the most number of zeros, and then remove any cols and their
#' corresponding rows that are non-zero. To account for ties of the count of
#' zeros for rows, a random choice is made. Hence, running this function
#' multiple times can return different sets of unrelated subjects.
#'
#' @param ped A pedigree objects with unique id, father index, mother index
#' @param avail Vector of availability status (e.g., genotyped) 0/1 or
#' TRUE/FALSE
#'
#' @return A vector of the ids of subjects that are unrelated.

#' @examples
#' data(sampleped)
#' fam1 <- sampleped[sampleped$ped == 1, ]
#'
#'
#' ped1 <- pedigree(
#'   fam1$id, fam1$father, fam1$mother,
#'   fam1$sex, fam1$affected, fam1$avail
#' )
#'
#' ## to see plot:
#' ## plot.pedigree(ped1, align=FALSE)
#' id1 <- unrelated(ped1, avail = fam1$avail)
#'
#' id1
#' ## some possible vectors
#' ## [1] '110' '113' '133' '109'
#' ## [1] '113' '118' '141' '109'
#' ## [1] '113' '118' '140' '109'
#' ## [1] '110' '113' '116' '109'
#' ## [1] '113' '133' '141' '109'
#'
#'
#' fam2 <- sampleped[sampleped$ped == 2, ]
#'
#' ped2 <- pedigree(
#'   fam2$id, fam2$father, fam2$mother,
#'   fam2$sex, fam2$affected, fam2$avail
#' )
#'
#' ## to see plot:
#' ## plot.pedigree(ped2, align=FALSE)
#'
#' id2 <- unrelated(ped2, avail = fam2$avail)
#'
#' ## some possible vectors
#' ## [1] '203' '207'
#' ## [1] '203' '204'
#' ## [1] '201' '203'
#' ## [1] '214' '203'
#' id2
#'
#' @seealso `kinship`, `pedigree`
#' @export
unrelated <- function(ped, avail = ped$ped$avail) {
    # Requires: kinship function

    # Given vectors id, father, and mother for a pedigree structure, and avail
    # = vector of T/F or 1/0 for whether each subject (corresponding to id
    # vector) is available (e.g., has DNA available), determine set of maximum
    # number of unrelated available subjects from a pedigree.

    # This is a greedy algorithm that uses the kinship matrix, sequentially
    # removing rows/cols that are non-zero for subjects that have the most
    # number of zero kinship coefficients (greedy by choosing a row of kinship
    # matrix that has the most number of zeros, and then remove any cols and
    # their corresponding rows that are non-zero.  To account for ties of the
    # count of zeros for rows, a random choice is made. Hence, running this
    # function multiple times can return different sets of unrelated subjects.

    id <- ped$ped$id

    kin <- kinship(ped)

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

        kin_avail <- kin_avail[-exclude, , drop = FALSE][, -exclude, drop = FALSE]
    }

    sort(dimnames(kin_avail)[[1]])
}
TRUE
