#' Individual's depth in a pedigree
#'
#' @description
#' Computes the depth of each subject in the Pedigree.
#'
#' @details
#' Mark each person as to their depth in a Pedigree; `0` for a founder,
#' otherwise :
#'
#' \deqn{depth = 1 + \max(fatherDepth, motherDepth)}
#'
#' In the case of an inbred Pedigree a perfect alignment may not exist.
#'
#' @param ... Additional arguments
#' @inheritParams Ped
#' @param align_parents If `align_parents = TRUE`, go one step further
#' and try to make both parents of each child have the same depth.
#' (This is not always possible).
#' It helps the drawing program by lining up pedigrees that
#' 'join in the middle' via a marriage.
#'
#' @return An integer vector containing the depth for each subject
#'
#' @author Terry Therneau, updated by Louis Le Nézet
#' @seealso [align()]
#' @include AllClass.R
#' @export
setGeneric("kindepth", signature = "obj",
    function(obj, ...) standardGeneric("kindepth")
)

#' @rdname kindepth
#' @examples
#' kindepth(
#'      c("A", "B", "C", "D", "E"),
#'      c("C", "D", "0", "0", "0"),
#'      c("E", "E", "0", "0", "0")
#' )
setMethod("kindepth", "character_OR_integer", function(obj, dadid, momid,
    align_parents = FALSE
) {
    id <- obj
    n <- length(id)
    if (missing(dadid) || length(dadid) != n) {
        stop("Invalid father id")
    }
    if (missing(momid) || length(momid) != n) {
        stop("Invalid mother id")
    }
    midx <- match(momid, id, nomatch = 0)  # row number of my mom
    didx <- match(dadid, id, nomatch = 0)  # row number of my dad

    if (n == 1) {
        return(0)
    }  # special case of a single subject

    parents <- which(midx == 0 & didx == 0)  # founders
    depth <- rep(0, n)
    child_old <- rep(0, n)
    # At each iteration below, all children of the current 'parents' are
    # labeled with depth 'i', and become the parents of the next iteration
    for (i in seq_len(n)) {
        child <- match(midx, parents, nomatch = 0) +
            match(didx, parents, nomatch = 0)  # Index of parent's childs
        ## version 1.8.5 did not have this check with child_old.
        ## Keeping it here because it was not the issue being fixed in 9/2023.
        if (all(child == child_old)) {
            stop("Impossible Pedigree: no progress made at iteration", i)
        }
        if (all(child == 0)) {
            break
        }
        if (i == n) {
            stop("Impossible Pedigree: someone is their own ancestor")
        }
        # Old child are parents of the next generation
        parents <- which(child > 0)
        depth[parents] <- i
        child_old <- child
    }
    if (!align_parents) {
        return(depth)
    }

    ## align Assume that subjects A and B marry, we have some ancestry
    ## information for both, and that A's ancestors go back 3 generations,
    ## B's for only two.
    ## If we add +1 to the depth of B and all her ancestors, then A and B
    ## will be the same depth, and will plot on the same line.
    ## Founders who marry in are also aligned. However, if an inbred
    ## Pedigree, may not be a simple fix of this sort.

    ## The algorithm is 1 First deal with founders. If a founder marries in
    ## multiple times at multiple deaths (animal pedigrees), given that
    ## subject the min(depth of spouses). These subjects cause trouble for
    ## the general algorithm below: the result would depend on the data order.
    ## 2. Find any remaining mother-father pairs that are mismatched in depth.
    ## Deal with them one at a time.
    ## 3.  The children's depth is max(father, mother) +1.
    ## Call the parent closest to the children ``good'' and the other ``bad''.
    ## 4. Chase up the good side, and get a list of all subjects connected to
    ## 'good', including in-laws (spouse connections) and sibs that are at this
    ## level or above.  Call this agood (ancestors of good).  We do not follow
    ## any connections at a depth lower than the marriage in question, to get
    ## the highest marriages right.  For the bad side, just get ancestors.
    ## 5. Avoid pedigree loops!  If the agood list contains anyone in abad, then
    ## don't try to fix the alignment, otherwise: Push abad down, then run the
    ## pushdown algorithm to repair any descendents --- you may have pulled
    ## down a grandparent but not the sibs of that grandparent.

    ## It may be possible to do better alignment when the Pedigree has loops,
    ## but it is definitely beyond this program, perhaps in auto_hint one day.

    ## First deal with any parents who are founders They all start with depth 0
    dads <- didx[midx > 0 & didx > 0]  # the father side of all spouse pairs
    moms <- midx[midx > 0 & didx > 0]
    if (0) {
        ## somewhere between version 1.8.5 and 1.9.6 this portion added
        ## check against other test cases, but was trying to fix where a mother
        ## had multiple marriages
        founder <- (midx == 0 & didx == 0)
        if (any(founder[dads])) {
            drow <- which(founder[dads]) # which pairs
            id <- unique(dads[drow]) # id
            depth[id] <- tapply(depth[moms[drow]], dads[drow], min)
            dads <- dads[-drow]
            moms <- moms[-drow]
        }
        if (any(founder[moms])) {
            mrow <- which(founder[moms]) # which pairs
            id <- unique(moms[mrow]) # id
            depth[id] <- tapply(depth[dads[mrow]], moms[mrow], min)
            dads <- dads[-mrow]
            moms <- moms[-mrow]
        }
    }
    ## Get rid of duplicate pairs, which occur for any spouse with multiple
    ## offspring
    dups <- duplicated(dads + moms * n)
    if (any(dups)) {
        dads <- dads[!dups]
        moms <- moms[!dups]
    }
    npair <- length(dads)
    done <- rep(FALSE, npair)  # couples that are taken care of
    while (TRUE) {
        ## Select parents pairs to fix
        pairs_to_fix <- which((depth[dads] != depth[moms]) & !done)

        if (length(pairs_to_fix) == 0) {
            break
        }
        ## Get max depth of all pairs to fix
        temp <- pmax(depth[dads], depth[moms])[pairs_to_fix]

        ## Select the couple to fix
        ## that have the minimal depth
        who <- min(pairs_to_fix[temp == min(temp)])

        ## Good is the individuals with the higher depth
        good <- moms[who]
        bad <- dads[who]
        if (depth[dads[who]] > depth[moms[who]]) {
            good <- dads[who]
            bad <- moms[who]
        }

        ## Move depth of all bad individuals
        ## All id linked to bad
        abad <- c(bad, ancestors(bad, midx, didx))

        if (length(abad) == 1 && sum(c(dads, moms) == bad) == 1) {
            ## Simple case, a solitary marry-in
            ## Only one in ancestry and is dad or mom of only one
            depth[bad] <- depth[good]
        } else {
            ## Ancestors of the 'good' side
            agood <- c(good, ancestors(good, midx, didx))

            ## If individual already in agood not bad
            abad1 <- abad[!abad %in% agood]

            ## For spouse chasing, I need to exclude the given pair
            tdad <- dads[-who]
            tmom <- moms[-who]
            ## Get all individuals affiliated to agood
            while (TRUE) {
                ## Add spouse
                spouse <- c(tmom[!is.na(match(tdad, agood))],
                    tdad[!is.na(match(tmom, agood))]
                )
                temp <- unique(c(agood, spouse))

                ## Add ancestors
                temp <- unique(c(temp, ancestors(temp, midx, didx)))

                ## Add kids
                kids <- (!is.na(match(midx, temp)) | !is.na(match(didx, temp)))
                temp <- unique(c(temp, (seq_len(n))[
                    kids & depth <= depth[good]
                ]))

                if (length(temp) == length(agood)) {
                    ## If no addition to good ancestors break
                    break
                } else {
                    ## Else do other iteration
                    agood <- temp
                }
            }
            ## Update agood but only if not in abad1
            agood <- agood[!agood %in% abad1]

            ## Change all depth
            if (all(match(abad, agood, nomatch = 0) == 0)) {
                ## shift it down
                depth[abad] <- depth[abad] + (depth[good] - depth[bad])

                ## Siblings may have had children: make sure all kids are below
                ## their parents.  It's easiest to run through the whole tree
                for (i in 0:n) {
                    parents <- which(depth == i)
                    child <- match(midx, parents, nomatch = 0) +
                        match(didx, parents, nomatch = 0)
                    if (all(child == 0)) {
                        break
                    }
                    depth[child > 0] <- pmax(i + 1, depth[child > 0])
                }
            }
        }
        ## Once a subject has been shifted, we don't allow them to instigate
        ## yet another shift, possibly on another level
        done[who] <- TRUE
        ##  This snunk into version 1.9.6, which was part of
        ## bug: done[dads == bad | moms == bad] <- TRUE
    }
    if (all(depth > 0)) {
        stop(
            "You found a bug in kindepth's alignment code!",
            "Depth found:", depth
        )
    }
    depth
}
)

#' @rdname kindepth
#' @examples
#' data(sampleped)
#' ped1 <- Pedigree(sampleped[sampleped$famid == "1",])
#' kindepth(ped1)
setMethod("kindepth", "Pedigree",
    function(obj, align_parents = FALSE) {
        kindepth(ped(obj), align_parents)
    }
)

#' @rdname kindepth
setMethod("kindepth", "Ped",
    function(obj, align_parents = FALSE) {
        kindepth(id(obj), dadid(obj), momid(obj), align_parents)
    }
)