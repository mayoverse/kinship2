#' Generate all possible permutation
#'
#' Given a vector of length **n**, generate all possible permutations of
#' the numbers 1 to **n**.
#' This is a recursive routine, and is not very efficient.
#'
#' @param x A vector of length **n**
#' @return A matrix with **n** cols and **n!** rows
#' @keywords internal, auto_hint
permute <- function(x) {
    n <- length(x)
    if (n == 1) {
        x
    } else if (n == 2) {
        rbind(x, x[c(2, 1)])
    } else if (n == 3) {
        rbind(x[seq_len(3)], x[c(2, 1, 3)], x[c(3, 1, 2)])
    } else {
        temp <- paste(
            "cbind(x[", seq_len(n), "], permute(x[-", seq_len(n), "]))",
            collapse = ","
        )
        temp <- paste("rbind(", temp, ")")
        eval(parse(text = temp))
    }
}

#' Best hint for a Pedigree alignment
#'
#' @description
#' When computer time is cheap, use this routine to get a *best*
#' Pedigree alignment.
#' This routine will try all possible founder orders, and return the one
#' with the least **stress**.
#'
#' @details
#' The [auto_hint()] routine will rearrange sibling order, but not
#' founder order.
#' This calls [auto_hint()] with every possible founder order, and finds that
#' plot with the least "stress".
#' The stress is computed as a weighted sum of three error measures:
#'
#' - nbArcs The number of duplicate individuals in the plot
#' - lgArcs The sum of the absolute values of the differences in the
#'   positions of duplicate individuals
#' - lgParentsChilds The sum of the absolute values of the differences between
#'   the center of the children and the parents
#'
#' \deqn{stress =
#'      wt[1] * nbArcs +
#'      wt[2] * lgArcs +
#'      wt[3] * lgParentsChilds
#'}
#'
#' If during the search, a plot is found with a stress level less than
#' **tolerance**, the search is terminated.
#'
#' @param wt A vector of three weights for the three error measures.
#' Default is `c(1000, 10, 1)`.
#' 1. The number of duplicate individuals in the plot
#' 2. The sum of the absolute values of the differences in the
#'   positions of duplicate individuals
#' 3. The sum of the absolute values of the differences between
#'   the center of the children and the parents.
#'
#' @param tolerance The maximum stress level to accept.
#' Default is `0`
#' @inheritParams align
#'
#' @return The best Hints object out of all the permutations
#'
#' @seealso [auto_hint()], [align()]
#' @export
#' @examples
#' data(sampleped)
#' ped <- Pedigree(sampleped[sampleped$famid == 1,])
#' best_hint(ped)
#' @include auto_hint.R
#' @include align.R
#' @keywords alignment, auto_hint
#' @usage NULL
setGeneric(
    "best_hint", signature = "obj",
    function(obj, ...) {
        standardGeneric("best_hint")
    }
)

#' @rdname best_hint
setMethod(
    "best_hint", "Pedigree",
    function(obj, wt = c(1000, 10, 1), tolerance = 0) {

        # find founders married to founders the female of such pairs
        # determines the plot order of founders
        momid <- momid(ped(obj))
        dadid <- dadid(ped(obj))
        id <- id(ped(obj))
        mom <- match(momid, id)
        dad <- match(dadid, id)
        # founders and marry-ins
        founders <- id[is.na(mom) & is.na(dad)]
        fpair <- !(is.na(match(momid, founders)) |
                is.na(match(dadid, founders))
        )
        # row num of founding moms
        fmom <- unique(match(momid[fpair], id))
        pmat <- permute(seq_along(fmom))
        # Put the subsets into a random order For most Pedigrees,
        # there are several permutations that will give a tolerance
        # or near tolerance plot.
        # This way we should hit one of them soon.
        pmat <- pmat[order(runif(nrow(pmat))), ]

        n <- length(obj)
        for (perm in seq_len(nrow(pmat))) {
            hint <- cbind(seq_len(n), rep(0, n))
            hint[fmom, 1] <- pmat[perm, ]
            # this fixes up marriages and such
            newhint <- auto_hint(
                obj, hints = Hints(
                    horder = setNames(hint[, 1], id(ped(obj)))
                ), reset = TRUE
            )
            plist <- align(
                obj, packed = TRUE, align = TRUE, width = 8, hints = newhint
            )

            # Compute the error measures
            err <- rep(0, 3)
            maxlev <- nrow(plist$nid)
            for (lev in seq_len(maxlev)) {
                idlist <- plist$nid[lev, seq_len(plist$n[lev])]
                dups <- duplicated(idlist)
                if (any(dups)) {
                    err[1] <- err[1] + sum(dups)
                    for (i in idlist[dups]) {
                        who <- (seq_along(idlist))[match(
                            idlist, i, nomatch = 0
                        ) > 0]
                        err[2] <- err[2] + abs(diff(plist$pos[lev, who]))
                    }
                }

                # get parent-child pulls
                fam2 <- plist$fam[lev, ]
                if (any(fam2 > 0)) {
                    # center of kids
                    centers <- tapply(plist$pos[lev, ], fam2, mean)
                    if (any(fam2 == 0)) {
                        centers <- centers[-1]
                    }
                    # parents
                    above <- plist$pos[lev - 1, sort(unique(fam2))] + 0.5
                    err[3] <- err[3] + sum(abs(centers - above))
                }
            }

            # best one so far?
            total <- sum(err * wt)
            if (perm == 1 || total < besttot) {
                besttot <- total
                besthint <- newhint
            }
            if (besttot <= tolerance)
                break  # we needn't do better than this!
        }
        besthint
    }
)
