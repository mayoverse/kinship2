# Automatically generated from all.nw using noweb

#' Shift set of siblings to the left or right
#'
#' @details This routine is used by `auto_hint()`.
#' It shifts a set of siblings to the left or right, so that the
#' marriage is on the edge of the set of siblings, closest to the spouse.
#' It also shifts the subject himself, so that he is on the edge of the
#' set of siblings, closest to the spouse.
#' It also shifts the monozygotic twins, if any, so that they are
#' together within the set of twins.
#'
#' @param id The id of the subject to be shifted
#' @param sibs The ids of the siblings
#' @param goleft If `TRUE`, shift to the left, otherwise to the right
#' @param hint The current hint vector
#' @param twinrel The twin relationship matrix
#' @param twinset The twinset vector
#'
#' @return The updated hint vector
#' @seealso [auto_hint()]
#' @keywords internal, auto_hint
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
            rel2 <- twinrel[twinrel[, 3] == 1, seq_len(2), drop = FALSE]
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

#' Find the spouse of a subject
#'
#' @details This routine is used by `auto_hint()`.
#' It finds the spouse of a subject.
#'
#' @param idpos The position of the subject
#' @param plist The alignment structure representing the Pedigree layout.
#' See [align()] for details.
#' @param lev The generation level of the subject
#' @inheritParams align
#'
#' @return The position of the spouse
#' @seealso [auto_hint()]
#' @keywords internal, auto_hint
findspouse <- function(idpos, plist, lev, obj) {
    lpos <- idpos
    while (lpos > 1 && plist$spouse[lev, lpos - 1]) {
        lpos <- lpos - 1
    }
    rpos <- idpos
    while (plist$spouse[lev, rpos]) {
        rpos <- rpos + 1
    }
    if (rpos == lpos) {
        stop("auto_hint bug 3")
    }

    opposite <- sex(ped(obj))[plist$nid[lev, lpos:rpos]] !=
        sex(ped(obj))[plist$nid[lev, idpos]]

    ## Can happen with a triple marriage
    if (!any(opposite)) {
        stop("auto_hint bug 4") # no spouse
    }
    spouse <- min((lpos:rpos)[opposite])
    spouse
}

#' Find the siblings of a subject
#'
#' @details This routine is used by `auto_hint()`.
#' It finds the siblings of a subject.
#'
#' @inheritParams findspouse
#'
#' @return The positions of the siblings
#' @seealso [auto_hint()]
#' @keywords internal, auto_hint
findsibs <- function(idpos, plist, lev) {
    family <- plist$fam[lev, idpos]
    if (family == 0) {
        stop("auto_hint bug 6")
    }
    which(plist$fam[lev, ] == family)
}

#' Find the duplicate pairs of a subject
#'
#' @details This routine is used by `auto_hint()`.
#' It finds the duplicate pairs of a subject and returns them in
#' the order they should be plotted.
#'
#' @param idlist List of individuals identifiers to be considered
#' @inheritParams findspouse
#' @inheritParams align
#'
#' @return A matrix of duplicate pairs
#' @seealso [auto_hint()]
#' @keywords internal, auto_hint
duporder <- function(idlist, plist, lev, obj) {
    temp <- table(idlist)
    if (all(temp == 1)) {
        return(matrix(0L, nrow = 0, ncol = 3))
    }

    # make an intial list of all pairs's positions
    # if someone appears 4 times they get 3 rows
    npair <- sum(temp - 1)
    dmat <- matrix(0L, nrow = npair, ncol = 3)
    dmat[, 3] <- 2
    dmat[seq_len(npair / 2), 3] <- 1
    i <- 0
    for (id in unique(idlist[duplicated(idlist)])) {
        j <- which(idlist == id)
        for (k in 2:length(j)) {
            i <- i + 1
            dmat[i, seq_len(2)] <- j[k + c(-1, 0)]
        }
    }
    if (nrow(dmat) == 1) {
        return(dmat)
    } # no need to sort it

    ## Does families touch?
    famtouch <- logical(npair)
    for (i in seq_len(npair)) {
        if (plist$fam[lev, dmat[i, 1]] > 0) {
            sib1 <- max(findsibs(dmat[i, 1], plist, lev))
        } else {
            spouse <- findspouse(dmat[i, 1], plist, lev, obj)
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
            spouse <- findspouse(dmat[i, 2], plist, lev, obj)
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

#' Get twin relationships
#'
#' @details This routine function determine the twin relationships
#' in a Pedigree. It determine the order of the twins
#' in the Pedigree.
#' It is used by `auto_hint()`.
#'
#' @inheritParams align
#'
#' @keywords internal, auto_hint
#' @return A list containing components
#'  1. `twinset` the set of twins
#'  2. `twinrel` the twins relationships
#'  3. `twinord` the order of the twins
#' @seealso [auto_hint()]
get_twin_rel <- function(obj) {
    if (length(rel(obj)) == 0) {
        relation <- NULL
    } else {
        relation <- as.data.frame(rel(obj))[, c("id1", "id2", "code")]
        relation$code <- as.numeric(relation$code)
    }
    n <- length(obj)
    twinset <- setNames(rep(0, n), id(ped(obj)))
    twinord <- setNames(rep(1, n), id(ped(obj)))
    twinrel <- NULL

    if (!is.null(relation) && any(relation$code < 4)) {
        ## Select only siblings relationships
        temp <- (relation$code < 4)
        ## list of twin id's
        twinlist <- unique(unlist(c(relation[temp, seq_len(2)])))
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
            twinrel[, 1] <- twinset[twinrel[, 1]]
        }
    }
    list(twinset = twinset, twinrel = twinrel, twinord = twinord)
}

#' Initial hint for a Pedigree alignment
#'
#' @description
#' Compute an initial guess for the alignment of a Pedigree
#'
#' @details
#' A Pedigree structure can contain a [Hints-class] object which helps to
#' reorder the Pedigree (e.g. left-to-right order of children within family) so
#' as to plot with minimal distortion. This routine is used to create an
#' initial version of the hints. They can then be modified if desired.
#'
#' This routine would not normally be called by a user. It moves children
#' within families, so that marriages are on the "edge" of a set children,
#' closest to the spouse. For pedigrees that have only a single connection
#' between two families this simple-minded approach works surprisingly well.
#' For more complex structures hand-tuning of the hints may be required.
#'
#' When `auto_hint()` is called with a a vector of numbers as the **hints**
#' argument, the values for the founder females are used to order the founder
#' families left to right across the plot.
#' The values within a sibship are used as the preliminary order of
#' siblings within a family; this may be changed to move one of them to the
#' edge so as to match up with a spouse. The actual values in the vector are
#' not important, only their order.
#'
#' @param reset If `TRUE`, then even if  the Ped object has Hints, reset
#' them to the initial values.
#' @inheritParams align
#'
#' @return The initial [Hints-class] object.
#'
#' @seealso [align()], [best_hint()], [Hints-class]
#' @examples
#' data(sampleped)
#' ped <- Pedigree(sampleped[sampleped$famid == 1, ])
#' auto_hint(ped)
#' @export
#' @keywords internal, alignment, auto_hint
#' @usage NULL
setGeneric("auto_hint", signature = "obj",
    function(obj, ...) standardGeneric("auto_hint")
)

#' @rdname auto_hint
#' @export
setMethod("auto_hint", "Pedigree", function(obj,
    hints = NULL, packed = TRUE, align = FALSE, reset = FALSE
) {
    ## full documentation now in vignette: align_code_details.Rmd
    ## References to those sections appear here as:
    ## Doc: auto_hint
    if (!is.null(hints) && is(hints, "Hints")) {
        if (
            (length(horder(hints)) != 0 || length(spouse(hints)) != 0) && !reset
        ) {
            return(hints(obj))
        }
    } # nothing to do

    if (length(unique(famid(ped(obj)))) > 1) {
        stop("auto_hint only works on Pedigrees with a single family")
    }

    n <- length(obj)
    depth <- kindepth(obj, align_parents = TRUE)

    ## Doc: init-auto_hint horder
    horder <- setNames(rep(0, n), id(ped(obj)))
    if (!is.null(hints)) {
        if (is.list(hints)) {
            hints <- Hints(hints)
        } else if (!is(hints, "Hints")) {
            stop("hints must be a list or a Hints object")
        }
        horder <- horder(hints)
    } else {
        hints <- Hints(horder = horder)
    }

    for (i in unique(depth)) {
        who <- (depth == i & horder == 0)
        # screwy input - overwrite it
        if (any(who)) {
            horder[who] <- seq_len(sum(who))
        }
    }

    twin_rel <- get_twin_rel(obj)
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
        for (i in unique(depth)) {
            who <- (depth == i)
            horder[who] <- rank(horder[who]) # there should be no ties
        }
    }

    if (nrow(spouse(hints)) > 0) {
        sptemp <- spouse(hints)
    } else {
        sptemp <- NULL
    }

    plist <- align(obj,
        packed = packed, align = align,
        hints = Hints(horder = horder, spouse = sptemp)
    )


    ## Doc: fixup-2
    ## Fix if duplicate individuals present
    maxlev <- nrow(plist$nid)
    for (lev in seq_len(maxlev)) {
        # subjects on this level
        idlist <- plist$nid[lev, seq_len(plist$n[lev])]
        # duplicates to be dealt with
        dpairs <- duporder(idlist, plist, lev, obj)
        if (nrow(dpairs) == 0) next
        for (i in seq_len(nrow(dpairs))) {
            anchor <- spouse <- rep(0, 2)
            for (j in seq_len(2)) {
                direction <- c(FALSE, TRUE)[j]
                idpos <- dpairs[i, j]
                if (plist$fam[lev, idpos] > 0) {
                    # Am connected to parents at this location
                    anchor[j] <- 1 # familial anchor
                    sibs <- idlist[findsibs(idpos, plist, lev)]
                    if (length(sibs) > 1) {
                        horder <- shift(
                            idlist[idpos], sibs, direction,
                            horder, twinrel, twinset
                        )
                    }
                } else {
                    # spouse at this location connected to parents ?
                    spouse[j] <- findspouse(idpos, plist, lev, obj)
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
                return(Hints(horder = seq_len(n))) # punt
            } else {
                if (is.vector(temp)) {
                    temp <- data.frame(
                        idl = temp[1], idr = temp[2], anchor = temp[3]
                    )
                } else if (is.matrix(temp)) {
                    temp <- data.frame(
                        idl = temp[, 1], idr = temp[, 2],
                        anchor = temp[, 3]
                    )
                }
                sptemp <- rbind(sptemp, temp)
            }
        }
        #
        # Recompute, since this shifts things on levels below
        #
        new_spouse <- data.frame(
            idl = id(ped(obj))[sptemp$idl],
            idr = id(ped(obj))[sptemp$idr],
            anchor = anchor_to_factor(sptemp$anchor)
        )
        plist <- align(obj,
            packed = packed, align = align,
            hints = Hints(horder = horder, spouse = new_spouse)
        )
    }

    new_spouse <- data.frame(
        idl = id(ped(obj))[sptemp$idl],
        idr = id(ped(obj))[sptemp$idr],
        anchor = anchor_to_factor(sptemp$anchor)
    )
    Hints(horder = horder, spouse = new_spouse)
})
