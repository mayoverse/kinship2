#' Best hint for alignment
#'
#' @description
#' When computer time is cheap, use this routine to get a 'best' Pedigree.
#' This routine will try all possible founder orders, and return the one
#' with the least 'stress'.
#'
#' @details
#' The auto_hint routine will rearrange sibling order, but not founder order.
#' This calls auto_hint with every possible founder order, and finds that
#' plot with the least 'stress'.
#' The stress is computed as a weighted sum of three error measures:
#'
#' - nbArcs The number of duplicate individuals in the plot
#' - lgArcs The sum of the absolute values of the differences in the
#'   positions of duplicate individuals
#' - lgParentsChilds The sum of the absolute values of the differences between
#'   the center of the children and the parents
#'
#' \eqn{stress =
#'      wt[1] \times nbArcs +
#'      wt[2] \times lgArcs +
#'      wt[3] \times lgParentsChilds
#'}
#'
#' If during the search, a plot is found with a stress level less than
#' **tolerance**, the search is terminated.
#'
#' @param wt A vector of three weights for the three error measures
#' - The number of duplicate individuals in the plot
#' - The sum of the absolute values of the differences in the
#'   positions of duplicate individuals
#' - The sum of the absolute values of the differences between
#'   the center of the children and the parents
#' Default is `c(1000, 10, 1)`.
#' @param tolerance The maximum stress level to accept. Default is `0`
#' @inheritParams align
#'
#' @return The best hint object out of all the permutations
#'
#' @seealso [auto_hint()]
#' @export
#' @examples
#' data(sampleped)
#' ped <- Pedigree(sampleped[sampleped$family == 1,])
#' best_hint(ped)
#' @include auto_hint.R
#' @include align.R
best_hint <- function(ped, wt = c(1000, 10, 1), tolerance = 0) {
    # find founders married to founders the female of such pairs
    # determines the plot order of founders
    mom <- match(ped(ped, "momid"), ped(ped, "id"))
    dad <- match(ped(ped, "dadid"), ped(ped, "id"))
    # founders and marry-ins
    founders <- ped(ped, "id")[is.na(mom) & is.na(dad)]
    fpair <- !(is.na(match(ped(ped, "momid"), founders)) |
            is.na(match(ped(ped, "dadid"), founders))
    )
    # row num of founding moms
    fmom <- unique(match(ped(ped, "momid")[fpair], ped(ped, "id")))

    # This function generates the permutations one after the other
    permute <- function(x) {
        n <- length(x)
        if (n == 3) {
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
    pmat <- permute(seq_along(fmom))
    # Put the subsets into a random order For most Pedigrees,
    # there are several permutations that will give a tolerance
    # or near tolerance plot.
    # This way we should hit one of them soon.
    pmat <- pmat[order(runif(nrow(pmat))), ]

    n <- length(ped(ped, "id"))
    for (perm in seq_len(nrow(pmat))) {
        hint <- cbind(seq_len(n), rep(0, n))
        hint[fmom, 1] <- pmat[perm, ]
        # this fixes up marriages and such
        newhint <- auto_hint(ped, hints = hint[, 1])
        plist <- align(
            ped, packed = TRUE, align = TRUE, width = 8, hints = newhint
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
