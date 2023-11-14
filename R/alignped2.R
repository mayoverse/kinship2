# Automatically generated from all.nw using noweb

#' Alignment second routine
#'
#' @description
#' Second of the four co-routines which takes a collection of siblings,
#' grows the tree for each, and appends them side by side into a single tree.
#'
#' @details
#' The input arguments are the same as those to `alignped1()` with the
#' exception that **idx** will be a vector. This routine does nothing
#' to the spouselist matrix, but needs to pass it down the tree and back
#' since one of the routines called by `alignped2()` might change the matrix.
#'
#' The code below has one non-obvious special case. Suppose that two sibs marry.
#' When the first sib is processed by `alignped1` then both partners
#' (and any children) will be added to the rval structure below.
#' When the second sib is processed they will come back as a 1 element tree
#' (the marriage will no longer be on the **spouselist**), which should be added
#' onto rval. The rule thus is to not add any 1 element tree whose value
#' (which must be `idx[i]` is already in the rval structure for this level.
#'
#' @inheritParams align
#' @inheritParams alignped1
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
alignped2 <- function(idx, dadx, momx, level, horder, packed, spouselist) {
    idx <- idx[order(horder[idx])]  # Use the hints to order the sibs
    rval <- alignped1(idx[1], dadx, momx, level, horder, packed, spouselist)
    spouselist <- rval$spouselist

    if (length(idx) > 1) {
        mylev <- level[idx[1]]
        for (i in 2:length(idx)) {
            rval2 <- alignped1(idx[i], dadx, momx,
                level, horder, packed, spouselist
            )
            spouselist <- rval2$spouselist

            # Deal with the unusual special case:
            if ((rval2$n[mylev] > 1) ||
                    (is.na(match(idx[i], floor(rval$nid[mylev, ]))))
            ) {
                rval <- alignped3(rval, rval2, packed)
            }
        }
        rval$spouselist <- spouselist
    }
    rval
}
