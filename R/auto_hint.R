# Automatically generated from all.nw using noweb

## Doc: Shift
#' Routine to shift set of siblings to the left or right
#'
#' @param id The id of the subject to be shifted
#' @param sibs The ids of the siblings
#' @param goleft If TRUE, shift to the left, otherwise to the right
#' @param hint The current hint vector
#' @param twinrel The twin relationship matrix
#' @param twinset The twinset vector
#'
#' @return The updated hint vector
#'
#' @keywords internal
#' @export
shift <- function(id, sibs, goleft, hint, twinrel, twinset) {
    if (twinset[id] > 0) {
        # enough to avoid overlap
        shift_amt <- 1 + diff(range(hint[sibs]))
        twins <- sibs[twinset[sibs] == twinset[id]]
        if (goleft) {
            hint[twins] <- hint[twins] - shift_amt
        } else {
            hint[twins] <- hint[twins] + shift_amt
        }

        mono <- any(twinrel[c(
            match(id, twinrel[, 1], nomatch = 0),
            match(id, twinrel[, 2], nomatch = 0)
        ), 3] == 1)
        if (mono) {
            #
            # ok, we have to worry about keeping the monozygotics
            #  together within the set of twins.
            # first, decide who they are, by finding those monozygotic
            #  with me, then those monozygotic with the results of that
            #  iteration, then ....  If I were the leftmost, this could
            #  take (#twins -1) iterations to get us all
            #
            monoset <- id
            rel2 <- twinrel[twinrel[, 3] == 1, 1:2, drop = FALSE]
            for (i in 2:length(twins)) {
                newid1 <- rel2[
                    match(monoset, rel2[, 1], nomatch = 0), 2
                ]
                newid2 <- rel2[
                    match(monoset, rel2[, 2], nomatch = 0), 1
                ]
                monoset <- unique(c(monoset, newid1, newid2))
            }
            if (goleft) {
                hint[monoset] <- hint[monoset] - shift_amt
            } else {
                hint[monoset] <- hint[monoset] + shift_amt
            }
        }
    }

    # finally, move the subject himself
    if (goleft) {
        hint[id] <- min(hint[sibs]) - 1
    } else {
        hint[id] <- max(hint[sibs]) + 1
    }

    hint[sibs] <- rank(hint[sibs]) # aesthetics -- no negative hints
    hint
}

#' Routine to find the spouse of a subject
#'
#' @param mypos The position of the subject
#' @param plist The alignment structure
#' @param lev The level of the subject
#' @param ped The pedigree structure
#'
#' @return The position of the spouse
#'
#' @keywords internal
#' @export
findspouse <- function(mypos, plist, lev, ped) {
    lpos <- mypos
    while (lpos > 1 && plist$spouse[lev, lpos - 1]) {
        lpos <- lpos - 1
    }
    rpos <- mypos
    while (plist$spouse[lev, rpos]) {
        rpos <- rpos + 1
    }
    if (rpos == lpos) {
        stop("auto_hint bug 3")
    }

    opposite <- ped$ped$sex[plist$nid[lev, lpos:rpos]] !=
        ped$ped$sex[plist$nid[lev, mypos]]

    ## Can happen with a triple marriage
    if (!any(opposite)) {
        stop("auto_hint bug 4") # no spouse
    }
    spouse <- min((lpos:rpos)[opposite])
    spouse
}

#' Routine to find the siblings of a subject
#'
#' @param mypos The position of the subject
#' @param plist The alignment structure
#' @param lev The level of the subject
#'
#' @return The positions of the siblings
#'
#' @keywords internal
#' @export
findsibs <- function(mypos, plist, lev) {
    family <- plist$fam[lev, mypos]
    if (family == 0) {
        stop("auto_hint bug 6")
    }
    which(plist$fam[lev, ] == family)
}

#' Routine to find the duplicate pairs of a subject
#'
#' @param idlist The list of ids
#' @param plist The alignment structure
#' @param lev The level of the subject
#' @param ped The pedigree structure
#'
#' @return A matrix of duplicate pairs
#'
#' @keywords internal
#' @export
duporder <- function(idlist, plist, lev, ped) {
    temp <- table(idlist)
    if (all(temp == 1)) {
        return(matrix(0L, nrow = 0, ncol = 3))
    }

    # make an intial list of all pairs's positions
    # if someone appears 4 times they get 3 rows
    npair <- sum(temp - 1)
    dmat <- matrix(0L, nrow = npair, ncol = 3)
    dmat[, 3] <- 2
    dmat[1:(npair / 2), 3] <- 1
    i <- 0
    for (id in unique(idlist[duplicated(idlist)])) {
        j <- which(idlist == id)
        for (k in 2:length(j)) {
            i <- i + 1
            dmat[i, 1:2] <- j[k + -1:0]
        }
    }
    if (nrow(dmat) == 1) {
        return(dmat)
    } # no need to sort it

    ## Does families touch?
    famtouch <- logical(npair)
    for (i in 1:npair) {
        if (plist$fam[lev, dmat[i, 1]] > 0) {
            sib1 <- max(findsibs(dmat[i, 1], plist, lev))
        } else {
            spouse <- findspouse(dmat[i, 1], plist, lev, ped)
            ## If spouse is marry-in then move on without looking
            ## for sibs
            if (plist$fam[lev, spouse] == 0) {
                famtouch[i] <- FALSE
                next
            }
            sib1 <- max(findsibs(spouse, plist, lev))
        }

        if (plist$fam[lev, dmat[i, 2]] > 0) {
            sib2 <- min(findsibs(dmat[i, 2], plist, lev))
        } else {
            spouse <- findspouse(dmat[i, 2], plist, lev, ped)
            ## If spouse is marry-in then move on without looking
            ## for sibs
            if (plist$fam[lev, spouse] == 0) {
                famtouch[i] <- FALSE
                next
            }
            sib2 <- min(findsibs(spouse, plist, lev))
        }
        famtouch[i] <- (sib2 - sib1 == 1)
    }
    dmat[order(famtouch, dmat[, 1] - dmat[, 2]), , drop = FALSE]
}

#' Routine to get twin relationships
#'
#' @details This routine function determine the twin relationships
#' in a pedigree. It complete the missing twin relationships for
#' triplets, quads, etc. It also determine the order of the twins
#' in the pedigree.
#' It is used by \code{\link{auto_hint}}.
#'
#' @param ped The pedigree structure
#'
#' @return A list containing components `twinset`, `twinrel` and `twinord`
get_twin_rel <- function(ped) {
    if (is.null(ped$rel)) {
        relation <- NULL
    } else {
        relation <- cbind(
            as.matrix(ped$rel[, c("id1", "id2")]),
            as.numeric(ped$rel[, "code"])
        )
    }
    n <- length(ped$ped$id)
    twinset <- rep(0, n)
    twinord <- rep(1, n)
    twinrel <- NULL
    if (!is.null(relation) && any(relation[, 3] < 4)) {
        ## Select only siblings relationships
        temp <- (relation[, 3] < 4)
        twinlist <- unique(c(relation[temp, 1:2])) # list of twin id's
        twinrel <- relation[temp, , drop = FALSE]
        for (i in 2:length(twinlist)) {
            # Now, for any pair of twins on a line of twinrel, give both
            # of them the minimum of the two ids
            # For a set of triplets, it might take two iterations for the
            # smallest of the 3 numbers to "march" across the threesome.
            # For quads, up to 3 iterations, for quints, up to 4, ....
            newid <- pmin(twinrel[, 1], twinrel[, 2])
            twinset[twinrel[, 1]] <- newid
            twinset[twinrel[, 2]] <- newid
            twinord[twinrel[, 2]] <- pmax(
                twinord[twinrel[, 2]],
                twinord[twinrel[, 1]] + 1
            )
        }
    }
    list(twinset = twinset, twinrel = twinrel, twinord = twinord)
}

#' Align a pedigree
#'
#' @description
#' Align a pedigree to print well
#'
#' @details
#' A pedigree structure can contain a `hints` object which helps to
#' reorder the pedigree (e.g. left-to-right order of children within family) so
#' as to plot with minimal distortion. This routine is used to create an
#' initial version of the hints.  They can then be modified if desired.
#'
#' This routine would not normally be called by a user.  It moves children
#' within families, so that marriages are on the "edge" of a set children,
#' closest to the spouse.  For pedigrees that have only a single connection
#' between two families this simple-minded approach works surprisingly well.
#' For more complex structures hand-tuning of the hints matrix may be required.
#'
#' The pedigree in the example below is one where rearranging the founders
#' greatly decreases the number of extra connections. When auto_hint is called
#' with a a vector of numbers as the second argument, the values for the
#' founder females are used to order the founder families left to right across
#' the plot.  The values within a sibship are used as the preliminary order of
#' siblings within a family; this may be changed to move one of them to the
#' edge so as to match up with a spouse. The actual values in the vector are
#' not important, only their order.
#'
#' @param ped A pedigree structure
#' @param hints Optional hints object. Only the order component is used.
#' @param packed If TRUE, uniform distance between all individuals at a given
#' level.
#' @param align These parameters control the extra effort spent trying to align
#' children underneath parents, but without making the pedigree too wide.  Set
#' to FALSE to speed up plotting.
#'
#' @return A list containing components `order` and `spouse`
#'
#' @examples
#'
#' data(testped1)
#' ped1 <- with(testped1, pedigree(id, father, mother, sex))
#' plot(ped1, cex = .7, symbolsize = .7)
#'
#' # rearrange some founders
#' temp <- 1:nrow(testped1)
#' temp[76] <- .1
#' temp[77] <- .2
#' temp[74] <- .3
#' temp[60] <- .4
#' temp[30] <- temp[8] + .1
#' temp[65] <- temp[4] + .1
#' temp[14] <- temp[3] + .1
#' ped1$hints <- auto_hint(ped1, temp)
#' plot(ped1, cex = .7)
#'
#' @seealso pedigree, besthint
#' @keywords genetics
#' @export
setGeneric("auto_hint", signature = "obj",
    function(obj, ...) standardGeneric("auto_hint")
)

#' @include kindepth.R
#' @export
setMethod("auto_hint", "Pedigree", function(
    obj, hints = NULL, packed = TRUE, align = FALSE, reset = FALSE
) {
    ## full documentation now in vignette: align_code_details.Rmd
    ## References to those sections appear here as:
    ## Doc: auto_hint
    if ((!is.null(obj$hints$order) ||
                !is.null(obj$hints$spouse)
        ) & !reset
    ) {
        return(obj$hints)
    } # nothing to do

    if (length(unique(obj$ped$family)) > 1) {
        stop("auto_hint only works on pedigrees with a single family")
    }

    n <- length(obj$ped$id)
    depth <- kindepth(obj, align = TRUE)

    ## Doc: init-auto_hint
    if (!is.null(hints)) {
        if (is.vector(hints)) {
            hints <- list(order = hints)
        }
        if (is.matrix(hints)) {
            hints <- list(spouse = hints)
        }
        if (is.null(hints$order)) {
            horder <- integer(n)
        } else {
            horder <- hints$order
        }
    } else {
        horder <- integer(n)
    }

    for (i in unique(depth)) {
        who <- (depth == i & horder == 0)
        # screwy input - overwrite it
        if (any(who)) {
            horder[who] <- 1:sum(who)
        }
    }

    twin_rel <- get_twin_rel(ped = obj)
    twinset <- twin_rel$twinset
    twinord <- twin_rel$twinord
    twinrel <- twin_rel$twinrel

    if (any(twinset > 0)) {
        ## First, make any set of twins a cluster: 6.01, 6.02, ...
        ## By using fractions, I don't have to worry about
        ## other sib's values
        for (i in unique(twinset)) {
            if (i == 0) next
            who <- (twinset == i)
            horder[who] <- mean(horder[who]) + twinord[who] / 100
        }

        # Then reset to integers
        for (i in unique(obj$depth)) {
            who <- (obj$depth == i)
            horder[who] <- rank(horder[who]) # there should be no ties
        }
    }

    if (!is.null(hints)) {
        sptemp <- hints$spouse
    } else {
        sptemp <- NULL
    }

    plist <- align(obj,
        packed = packed, align = align,
        hints = list(order = horder, spouse = sptemp)
    )


    ## Doc: fixup-2
    ## Fix if duplicate individuales present
    maxlev <- nrow(plist$nid)
    for (lev in 1:maxlev) {
        # subjects on this level
        idlist <- plist$nid[lev, 1:plist$n[lev]]
        # duplicates to be dealt with
        dpairs <- duporder(idlist, plist, lev, obj)
        if (nrow(dpairs) == 0) next
        for (i in seq_len(nrow(dpairs))) {
            anchor <- spouse <- rep(0, 2)
            for (j in 1:2) {
                direction <- c(FALSE, TRUE)[j]
                mypos <- dpairs[i, j]
                if (plist$fam[lev, mypos] > 0) {
                    # Am connected to parents at this location
                    anchor[j] <- 1 # familial anchor
                    sibs <- idlist[findsibs(mypos, plist, lev)]
                    if (length(sibs) > 1) {
                        horder <- shift(
                            idlist[mypos], sibs, direction,
                            horder, twinrel, twinset
                        )
                    }
                } else {
                    # spouse at this location connected to parents ?
                    spouse[j] <- findspouse(mypos, plist, lev, obj)
                    if (plist$fam[lev, spouse[j]] > 0) { # Yes they are
                        anchor[j] <- 2 # spousal anchor
                        sibs <- idlist[findsibs(spouse[j], plist, lev)]
                        if (length(sibs) > 1) {
                            horder <- shift(
                                idlist[spouse[j]], sibs, direction,
                                horder, twinrel, twinset
                            )
                        }
                    }
                }
            }
            # add the marriage(s)
            ## Doc: Fixup2
            ## i,1 and i,2 point to the same person
            id1 <- idlist[dpairs[i, 1]]
            id2 <- idlist[spouse[1]]
            id3 <- idlist[spouse[2]]

            temp <- switch(paste(anchor, collapse = ""),
                "21" = c(id2, id1, dpairs[i, 3]), # the most common case
                "22" = rbind(c(id2, id1, 1), c(id1, id3, 2)),
                "02" = c(id2, id1, 0),
                "20" = c(id2, id1, 0),
                "00" = rbind(c(id1, id3, 0), c(id2, id1, 0)),
                "01" = c(id2, id1, 2),
                "10" = c(id1, id2, 1),
                NULL
            )

            if (is.null(temp)) {
                warning("Unexpected result in auto_hint,",
                    "please contact developer"
                )
                return(list(order = 1:n)) # punt
            } else {
                sptemp <- rbind(sptemp, temp)
            }
        }
        #
        # Recompute, since this shifts things on levels below
        #
        plist <- align(obj,
            packed = packed, align = align,
            hints = list(order = horder, spouse = sptemp)
        )
    }
    list(order = horder, spouse = sptemp)
})

TRUE
