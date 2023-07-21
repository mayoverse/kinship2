# Automatically generated from all.nw using noweb

# TODO add params and example

#' Second routine alignement
#'
#' @description
#' This is the second of the four co-routines.
#'
#' @details
#' This routine takes a collection of siblings, grows the tree for each,
#' and appends them side by side into a single tree.
#' The input arguments are the same as those to `alignped1` with the
#' exception that `[[x]]` will be a vector. This routine does nothing
#' to the spouselist matrix, but needs to pass it down the tree and back
#' since one of the routines called by `alignped2` might change the matrix.
#'
#' The code below has one non-obvious special case. Suppose that two sibs marry.
#' When the first sib is processed by `alignped1` then both partners
#' (and any children) will be added to the rval structure below.
#' When the second sib is processed they will come back as a 1 element tree
#' (the marriage will no longer be on the spouselist), which should be added
#' onto rval. The rule thus is to not add any 1 element tree whose value
#' (which must be `x[i]` is already in the rval structure for this level.
#'
#' @param x
#' @param dad
#' @param mom
#' @param level
#' @param horder
#' @param packed
#' @param spouselist
#'
#' @return A set of matrices along with the spouselist matrix.
#' The latter has marriages removed as they are processed.
#'
#' @examples
#' data(sample.ped)
#' ped <- with(sample.ped, pedigree(id, father, mother, sex, affected))
#' align.pedigree(ped)
#'
#' @seealso `plot.pedigree`, `autohint`
#' @keywords dplot
#' @export alignped2
alignped2 <- function(x, dad, mom, level, horder, packed, spouselist) {
    x <- x[order(horder[x])]  # Use the hints to order the sibs
    rval <- alignped1(x[1], dad, mom, level, horder, packed, spouselist)
    spouselist <- rval$spouselist

    if (length(x) > 1) {
        mylev <- level[x[1]]
        for (i in 2:length(x)) {
            rval2 <- alignped1(x[i], dad, mom, level, horder, packed, spouselist)
            spouselist <- rval2$spouselist

            # Deal with the unusual special case:
            if ((rval2$n[mylev] > 1) || (is.na(match(x[i], floor(rval$nid[mylev,
                ]))))) {
                rval <- alignped3(rval, rval2, packed)
            }
        }
        rval$spouselist <- spouselist
    }
    rval
}
TRUE
