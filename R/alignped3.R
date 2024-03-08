# Automatically generated from all.nw using noweb

#' Alignment third routine
#'
#' @description
#' Third of the four co-routines to merges two pedigree trees which
#' are side by side into a single object.
#'
#' @details
#' The primary special case is when the rightmost person in
#' the left tree is the same as the leftmost person in the right tree; we
#' need not plot two copies of the same person side by side.
#' (When initializing the output structures do not worry about this,
#' there is no harm if they are a column bigger than finally needed.)
#' Beyond that the work is simple book keeping.
#'
#' ## 1. Slide:
#'
#' For the unpacked case, which is the traditional way to draw
#' a Pedigree when we can assume the paper is infinitely wide, all parents are
#' centered over their children. In this case we think if the two trees to be
#' merged as solid blocks. On input they both have a left margin of 0.
#' Compute how far over we have to slide the right tree.
#'
#' ## 2. Merge:
#'
#' Now merge the two trees. Start at the top level and work down.
#'
#' @param alt1 Alignment of the first tree
#' @param alt2 Alignment of the second tree
#' @param space Space between two subjects
#' @inheritParams align
#'
#' @return A list containing the elements to plot the Pedigree.
#' It contains a set of matrices along with the spouselist matrix.
#' The latter has marriages removed as they are processed.
#' - `n` : A vector giving the number of subjects on each horizonal level of the
#'     plot
#' - `nid` : A matrix with one row for each level, giving the numeric id of
#'       each subject plotted.
#'       (A value of `17` means the 17th subject in the Pedigree).
#' - `pos` : A matrix giving the horizontal position of each plot point
#' - `fam` : A matrix giving the family id of each plot point.
#'       A value of `3` would mean that the two subjects in positions 3 and 4,
#'       in the row above, are this subject's parents.
#' - `spouselist` : Spouse matrix with anchors informations
#'
#' @examples
#' data(sampleped)
#' ped <- Pedigree(sampleped)
#' align(ped)
#'
#' @seealso [align()]
#' @keywords internal, alignment
alignped3 <- function(alt1, alt2, packed, space = 1) {
    maxcol <- max(alt1$n + alt2$n)
    maxlev <- length(alt1$n)
    n1 <- max(alt1$n)  # These are always >1
    n <- alt1$n + alt2$n

    nid <- matrix(0, maxlev, maxcol)
    nid[, seq_len(n1)] <- alt1$nid
    pos <- matrix(0, maxlev, maxcol)
    pos[, seq_len(n1)] <- alt1$pos

    fam <- matrix(0, maxlev, maxcol)
    fam[, seq_len(n1)] <- alt1$fam
    fam2 <- alt2$fam
    if (!packed) {
        ## Doc: alignped3: slide
        slide <- 0
        for (i in seq_len(maxlev)) {
            n1 <- alt1$n[i]
            n2 <- alt2$n[i]
            if (n1 > 0 && n2 > 0) {
                if (nid[i, n1] == alt2$nid[i, 1]) {
                    temp <- pos[i, n1] - alt2$pos[i, 1]
                } else {
                    temp <- space + pos[i, n1] - alt2$pos[i, 1]
                }
                if (temp > slide)
                    slide <- temp
            }
        }
    }
    ## Doc: alignped3-merge
    for (i in seq_len(maxlev)) {
        n1 <- alt1$n[i]
        n2 <- alt2$n[i]
        if (n2 > 0) {
            # If anything needs to be done for this row...
            if (n1 > 0 && (nid[i, n1] == floor(alt2$nid[i, 1]))) {
                # two subjects overlap
                overlap <- 1
                fam[i, n1] <- max(fam[i, n1], fam2[i, 1])
                nid[i, n1] <- max(nid[i, n1], alt2$nid[i, 1])  # preserve a '.5'
                if (!packed) {
                    if (fam2[i, 1] > 0) {
                        if (fam[i, n1] > 0) {
                            pos[i, n1] <- (alt2$pos[i, 1] + pos[i, n1] +
                                    slide
                            ) / 2
                        } else {
                            pos[i, n1] <- alt2$pos[i, 1] + slide
                        }
                    }
                }
                n[i] <- n[i] - 1
            } else {
                overlap <- 0
            }

            if (packed)
                slide <- if (n1 == 0) {
                    0
                } else {
                    pos[i, n1] + space - overlap
                }

            zz <- seq(from = overlap + 1, length = n2 - overlap)
            nid[i, n1 + zz - overlap] <- alt2$nid[i, zz]
            fam[i, n1 + zz - overlap] <- fam2[i, zz]
            pos[i, n1 + zz - overlap] <- alt2$pos[i, zz] + slide

            if (i < maxlev) {
                # adjust the pointers of any children (look ahead)
                temp <- fam2[i + 1, ]
                fam2[i + 1, ] <- ifelse(temp == 0, 0, temp + n1 - overlap)
            }
        }
    }
    ## Doc: rest of alignped3
    if (max(n) < maxcol) {
        maxcol <- max(n)
        nid <- nid[, seq_len(maxcol)]
        pos <- pos[, seq_len(maxcol)]
        fam <- fam[, seq_len(maxcol)]
    }

    list(n = n, nid = nid, pos = pos, fam = fam)
}
TRUE
