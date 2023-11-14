# Automatically generated from all.nw using noweb

#' Alignment first routine
#'
#' @description
#' First alignment routine which create the subtree founded on a single
#' subject as though it were the only tree.
#'
#' @details
#' In this routine the **nid** array consists of the final
#' `nid array + 1/2` of the final spouse array.
#' Note that the **spouselist** matrix will only contain spouse pairs
#' that are not yet processed. The logic for anchoring is slightly tricky.
#'
#' ## 1. Anchoring:
#'    First, if col 4 of the spouselist matrix is 0, we anchor at the first
#'    opportunity. Also note that if `spouselist[, 3] == spouselist[, 4]`
#'    it is the husband who is the anchor (just write out the possibilities).
#'
#' ## 2. Return values initialization:
#'    Create the set of 3 return structures, which will be matrices
#'    with `1 + nspouse` columns.
#'    If there are children then other routines will widen the result.
#'
#' ## 3. Create **lspouse** and **rspouse**:
#'    This two complimentary lists denote the spouses plotted on the left
#'    and on the right.
#'    For someone with lots of spouses we try to split them evenly.
#'    If the number of spouses is odd, then men should have more on
#'    the right than on the left, women more on the right.
#'    Any hints in the spouselist matrix override.
#'    We put the undecided marriages closest to **idx**, then add
#'    predetermined ones to the left and right. The majority of marriages will
#'    be undetermined singletons, for which **nleft** will be `1` for
#'    female (put my husband to the left) and `0` for male. In one bug found
#'    by plotting canine data, lspouse could initially be empty but
#'    `length(rspouse) > 1`. This caused `nleft > length(indx)`.
#'    A fix was to not let **indx** to be indexed beyond its length,
#'    fix by JPS 5/2013.
#'
#' ## 4. List the children:
#'    For each spouse get the list of children. If there are any we
#'    call [alignped2()] to generate their tree and
#'    then mark the connection to their parent.
#'    If multiple marriages have children we need to join the trees.
#'
#' ## 5. Splice the tree:
#'    To finish up we need to splice together the tree made up from
#'    all the kids, which only has data from `lev + 1` down, with the data here.
#'    There are 3 cases:
#'
#'    1. No children were found.
#'    2. The tree below is wider than the tree here, in which case we add
#'       the data from this level onto theirs.
#'    3. The tree below is narrower, for instance an only child.
#'
#' @param level Vector of the level of each subject
#' @param spouselist Matrix of spouses with 4 columns:
#'    - `1`: husband index
#'    - `2`: wife index
#'    - `3`: husband anchor
#'    - `4`: wife anchor
#' @inheritParams ancestors
#' @inheritParams Hints
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
alignped1 <- function(idx, dadx, momx, level, horder, packed, spouselist) {
    # Set a few constants
    maxlev <- max(level)
    lev <- level[idx]
    n <- integer(maxlev)

    if (length(spouselist) == 0) {
        spouse <- NULL
    } else {
        if (any(spouselist[, 1] == idx)) {
            sex <- 1  # I'm male
            sprows <- (spouselist[, 1] == idx &
                    (spouselist[, 4] == spouselist[, 3] |
                            spouselist[, 4] == 0
                    )
            )
            spouse <- spouselist[sprows, 2]  # ids of the spouses
        } else {
            sex <- 2
            sprows <- (spouselist[, 2] == idx &
                    (spouselist[, 4] != spouselist[, 3] |
                            spouselist[, 4] == 0
                    )
            )
            spouse <- spouselist[sprows, 1]
        }
    }
    # Marriages that cross levels are plotted at the higher level (lower on the
    # paper).
    if (length(spouse)) {
        keep <- level[spouse] <= lev
        spouse <- spouse[keep]
        sprows <- (which(sprows))[keep]
    }
    nspouse <- length(spouse)  # Almost always 0, 1 or 2
    ## Doc: alignped1 part2
    nid <- fam <- matrix(0L, maxlev, nspouse + 1)
    pos <- matrix(0, maxlev, nspouse + 1)
    n[lev] <- nspouse + 1
    pos[lev, ] <- 0:nspouse
    if (nspouse == 0) {
        # Easy case: the 'tree rooted at idx' is only idx itself
        nid[lev, 1] <- idx
        return(list(nid = nid, pos = pos, fam = fam, n = n,
            spouselist = spouselist
        ))
    }
    ## Doc: alignped1 -part3
    lspouse <- spouse[spouselist[sprows, 3] == 3 - sex]  # 1-2 or 2-1
    rspouse <- spouse[spouselist[sprows, 3] == sex]  # 1-1 or 2-2
    if (any(spouselist[sprows, 3] == 0)) {
        # Not yet decided spouses
        indx <- which(spouselist[sprows, 3] == 0)
        # total number to left
        nleft <- floor((length(sprows) + (sex == 2)) / 2)
        nleft <- nleft - length(lspouse)  # number of undecideds to the left
        if (nleft > 0) {
            # JPS fixed 5/2013, don't index when nleft > length(indx)
            lspouse <- c(lspouse,
                spouse[indx[seq_len(min(nleft, length(indx)))]]
            )
            indx <- indx[-(seq_len(min(nleft, length(indx))))]
        }
        if (length(indx))
            rspouse <- c(spouse[indx], rspouse)
    }

    nid[lev, ] <- c(lspouse, idx, rspouse)
    nid[lev, seq_len(nspouse)] <- nid[lev, seq_len(nspouse)] + 0.5  # marriages

    spouselist <- spouselist[-sprows, , drop = FALSE]
    ## Doc: alignped1 - part4
    nokids <- TRUE  # haven't found any kids yet
    spouse <- c(lspouse, rspouse)  # reorder
    for (i in seq_len(nspouse)) {
        ispouse <- spouse[i]
        children <- which((dadx == idx & momx == ispouse) |
                (dadx == ispouse & momx == idx)
        )
        if (length(children) > 0) {
            rval1 <- alignped2(children, dadx, momx,
                level, horder, packed, spouselist
            )
            spouselist <- rval1$spouselist
            # set the parentage for any kids a nuisance: it's possible to have
            # a child appear twice, when via inbreeding two children marry ---
            # makes the 'indx' line below more complicated
            temp <- floor(rval1$nid[lev + 1, ])  # cut off the .5's for matching
            indx <- (seq_along(temp))[match(temp, children, nomatch = 0) > 0]
            rval1$fam[lev + 1, indx] <- i  # set the kids parentage
            if (!packed) {
                # line the kids up below the parents The advantage at this
                # point: we know that there is nothing to the right that has to
                # be cared for
                kidmean <- mean(rval1$pos[lev + 1, indx])
                parmean <- mean(pos[lev, i + 0:1])
                if (kidmean > parmean) {
                    # kids to the right of parents: move the parents
                    indx <- i:(nspouse + 1)
                    pos[lev, indx] <- pos[lev, indx] + (kidmean - parmean)
                } else {
                    # move the kids and their spouses and all below
                    shift <- parmean - kidmean
                    for (j in (lev + 1):maxlev) {
                        jn <- rval1$n[j]
                        if (jn > 0) {
                            rval1$pos[j, seq_len(jn)] <- rval1$pos[j,
                                seq_len(jn)
                            ] + shift
                        }
                    }
                }
            }
            if (nokids) {
                rval <- rval1
                nokids <- FALSE
            } else {
                rval <- alignped3(rval, rval1, packed)
            }
        }
    }
    ## Doc: alignped1 -part5
    if (nokids) {
        return(list(nid = nid, pos = pos, fam = fam, n = n,
            spouselist = spouselist
        ))
    }

    if (ncol(rval$nid) >= 1 + nspouse) {
        # The rval list has room for me!
        rval$n[lev] <- n[lev]
        indx <- seq_len(nspouse + 1)
        rval$nid[lev, indx] <- nid[lev, ]
        rval$pos[lev, indx] <- pos[lev, ]
    } else {
        # my structure has room for them
        indx <- seq_len(ncol(rval$nid))
        rows <- (lev + 1):maxlev
        n[rows] <- rval$n[rows]
        nid[rows, indx] <- rval$nid[rows, ]
        pos[rows, indx] <- rval$pos[rows, ]
        fam[rows, indx] <- rval$fam[rows, ]
        rval <- list(nid = nid, pos = pos, fam = fam, n = n)
    }
    rval$spouselist <- spouselist
    rval
}
