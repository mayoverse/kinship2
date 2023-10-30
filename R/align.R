## Automatically generated from all.nw using noweb

#' Routine function to get ancestors of a subject
#'
#' @description Given the index of one or multiple individual(s), this
#' function iterate through the mom and dad indexes to
#' list out all the ancestors of the said individual(s).
#' This function is use in the `align()` function to
#' identify which spouse pairs has a common ancestor and
#' therefore if they need to be connected with a double line
#' (i.e. inbred).
#'
#' @param idx Indexes of the subjects
#' @param dadx Indexes of the fathers
#' @param momx Indexes of the mothers
#'
#' @return A vector of ancestor indexes
#' @seealso [align()]
ancestors <- function(idx, momx, dadx) {
    alist <- idx
    repeat {
        newlist <- c(alist, momx[alist], dadx[alist])
        newlist <- sort(unique(newlist[newlist > 0]))
        if (length(newlist) == length(alist)) {
            break
        }
        alist <- newlist
    }
    alist[alist != idx]
}


#' Generate plotting information for a Pedigree
#'
#' @description
#' Given a Pedigree, this function creates helper matrices that describe the
#' layout of a plot of the Pedigree.
#'
#' @details
#' This is an internal routine, used almost exclusively by
#' `ped_to_plotdf()`.  The subservient functions `auto_hint()`,
#' `alignped1()`, `alignped2()`,
#' `alignped3()`, and `alignped4()`
#' contain the bulk of the computation.
#' If the **hints** are missing the `auto_hint()` routine is called to
#' supply an initial guess.
#' If multiple families are present in the Pedigree, this routine is called
#' once for each family, and the results are combined in the list returned.
#' For more information you can read the associated vignette:align
#' `vignette("alignment_details")`.
#'
#' @param ped A Pedigree object
#' @param packed Should the Pedigree be compressed, i.e., allow diagonal
#' lines connecting parents to children in order to have a smaller overall
#' width for the plot.
#' @param width for a packed output, the minimum width of the plot, in
#' inches.
#' @param align for a packed Pedigree, align children under parents `TRUE`,
#' to the extent possible given the page width, or align to to the left
#' margin `FALSE`.
#' This argument can be a two element vector, giving the alignment
#' parameters, or a logical value.
#' If `TRUE`, the default is `c(1.5, 2)`, or numeric the routine
#' `alignped4()` will be called.
#' @param hints Plotting hints for the Pedigree.
#' This is a list with components `horder` and `spouse`, the second one
#' is optional.
#' - **horder** is a numeric vector with one element per subject in the
#' Pedigree.  It determines the relative horizontal order of subjects within a sibship, as
#' well as the relative order of processing for the founder couples. (For this
#' latter, the female founders are ordered as though they were sisters).
#' - **spouse** is a matrix with one row per hinted marriage, usually
#' only a few marriages in a pedigree will need an added hint, for instance
#' reverse the plot order of a husband/wife pair. Each row contains the
#' index of the left spouse, the right hand spouse, and the anchor
#' (i.e : `1` = left, `2` = right, `0` = either).
#' Children will preferentially appear under the parents of the anchored
#' spouse.
#' @inheritParams is_parent
#'
#' @return A list with components
#' - n A vector giving the number of subjects on each horizonal level of the
#' plot
#' - nid A matrix with one row for each level, giving the numeric id of
#' each subject plotted.
#' (A value of `17` means the 17th subject in the Pedigree).
#' - pos A matrix giving the horizontal position of each plot point
#' - fam A matrix giving the family id of each plot point.
#' A value of `3` would mean that the two subjects in positions 3 and 4,
#' in the row above, are this subject's parents.
#' - spouse A matrix with values
#'     - `0` = not a spouse
#'     - `1` = subject plotted to the immediate right is a spouse
#'     - `2` = subject plotted to the immediate right is an inbred spouse
#' - twins Optional matrix which will only be present if the Pedigree
#' contains twins :
#'     - `0` = not a twin
#'     - `1` = sibling to the right is a monozygotic twin
#'     - `2` = sibling to the right is a dizygotic twin
#'     - `3` = sibling to the right is a twin of unknown zygosity
#'
#' @examples
#' data(sampleped)
#' ped <- Pedigree(sampleped)
#' align(ped)
#'
#' @seealso [alignped1()],
#' [alignped2()],
#' [alignped3()],
#' [alignped4()],
#' [auto_hint()]
#' @export
#' @include auto_hint.R
#' @include kindepth.R
#' @include check_hints.R
#' @include AllClass.R
#' @include alignped1.R
#' @include alignped2.R
#' @include alignped3.R
#' @include alignped4.R
setGeneric("align", signature = "obj",
    function(obj, ...) standardGeneric("align")
)

setMethod("align", "Pedigree",
    function(obj, packed = TRUE, width = 10,
    align = TRUE, hints = obj@hints, missid = "0"
) {
    align(ped(obj), packed, width, align, hints, missid)
})


align <- function(ped, packed = TRUE, width = 10,
    align = TRUE, hints = NULL, missid = "0"
) {
    famlist <- unique(ped(ped, "famid"))
    if (length(famlist) > 1) {
        nfam <- length(famlist)
        alignment <- vector("list", nfam)
        for (i_fam in famlist) {
            ped_fam <- ped[ped(ped, "famid") == i_fam]
            alignment[[i_fam]] <- align(ped_fam, packed, width, align)
        }
        return(alignment)
    }
    if (is.null(hints$horder)) {
        hints <- try({
            auto_hint(ped)
        }, silent = TRUE)
        ## sometimes appears dim(ped) is empty (ped is NULL), so try fix here:
        ## (JPS 6/6/17
        if ("try-error" %in% class(hints)) {
            hints <- list(horder = seq_len(max(1, dim(ped))))
        }
    } else {
        check_hints(hints, ped(ped, "sex"))
    }
    ## Doc: Setup-align
    n <- length(ped(ped, "id"))

    level <- 1 + kindepth(ped, align = TRUE)
    horder <- hints$horder  # relative order of siblings within a family

    if (!is.null(hints$spouse)) {
        # start with the hints list
        tsex <- ped(ped, "sex")[hints$spouse[, 1]]  # sex of the left member
        spouselist <- cbind(0, 0, 1 + (tsex != "male"), hints$spouse[, 3])
        spouselist[, 1] <- ifelse(tsex == "male", hints$spouse[, 1],
            hints$spouse[, 2]
        )
        spouselist[, 2] <- ifelse(tsex == "male", hints$spouse[, 2],
            hints$spouse[, 1]
        )
    } else {
        spouselist <- matrix(0L, nrow = 0, ncol = 4)
    }

    if (nrow(rel(ped)) > 0 && any(rel(ped, "code") == "Spouse")) {
        # Add spouses from the relationship matrix
        trel <- rel(ped, c("id1", "id2"))[
            rel(ped, "code") == "Spouse", drop = FALSE
        ]
        trel$id1 <- match(trel$id1, ped(ped, "id"))
        trel$id2 <- match(trel$id2, ped(ped, "id"))
        tsex <- ped(ped, "sex")[trel[, 1]]
        trel[tsex != "male", seq_len(2)] <- trel[tsex != "male", 2:1]
        spouselist <- rbind(spouselist, cbind(trel[, 1], trel[, 2], 0, 0))
    }
    dad <- match(ped(ped, "dadid"), ped(ped, "id"), nomatch = 0)
    mom <- match(ped(ped, "momid"), ped(ped, "id"), nomatch = 0)
    is_child <- dad > 0 & mom > 0
    if (any(is_child)) {
        # add parents
        who <- which(is_child)
        spouselist <- rbind(spouselist, cbind(dad[who], mom[who], 0, 0))
    }
    hash <- spouselist[, 1] * n + spouselist[, 2]
    spouselist <- spouselist[!duplicated(hash), , drop = FALSE]
    ## Doc: Founders -align
    noparents <- (dad[spouselist[, 1]] == 0 & dad[spouselist[, 2]] == 0)
    ## Take duplicated mothers and fathers, then founder mothers
    dupmom <- spouselist[noparents, 2][duplicated(spouselist[noparents, 2])]
    ## Founding mothers with multiple marriages
    dupdad <- spouselist[noparents, 1][duplicated(spouselist[noparents, 1])]
    ## Founding fathers with multiple marriages
    foundmom <- spouselist[
        noparents & !(spouselist[, 1] %in% c(dupmom, dupdad)), 2
    ]  # founding mothers
    founders <- unique(c(dupmom, dupdad, foundmom))
    # use the hints to order them
    founders <- founders[order(horder[founders])]
    rval <- alignped1(founders[1], dad, mom, level, horder, packed, spouselist)
    if (length(founders) > 1) {
        spouselist <- rval$spouselist
        for (i in 2:length(founders)) {
            rval2 <- alignped1(founders[i], dad, mom, level, horder, packed,
                spouselist
            )
            spouselist <- rval2$spouselist
            rval <- alignped3(rval, rval2, packed)
        }
    }
    ## Doc: finish-align (1) Unhash out the spouse and nid arrays
    nid <- matrix(as.integer(floor(rval$nid)), nrow = nrow(rval$nid))
    spouse <- 1L * (rval$nid != nid)
    maxdepth <- nrow(nid)

    for (i in (seq_along(spouse))[spouse > 0]) {
        a1 <- ancestors(nid[i], mom, dad)
        # matrices are in column order
        a2 <- ancestors(nid[i + maxdepth], mom, dad)
        if (any(duplicated(c(a1, a2)))) {
            spouse[i] <- 2
        }
    }
    ## Doc: finish align(2)
    if (nrow(rel(ped)) > 0 && any(rel(ped, "code") != "Spouse")) {
        twins <- 0 * nid
        who <- (rel(ped, "code") != "Spouse")
        ltwin <- match(rel(ped, "id1")[who], ped(ped, "id"), nomatch = 0)
        rtwin <- match(rel(ped, "id2")[who], ped(ped, "id"), nomatch = 0)
        ttype <- rel(ped, "code")[who]
        # find where each of them is plotted (any twin only appears once with a
        # family id, i.e., under their parents)
        # matrix of connected-to-parent ids
        ntemp <- ifelse(rval$fam > 0, nid, 0)
        ltemp <- (seq_along(ntemp))[match(ltwin, ntemp, nomatch = 0)]
        rtemp <- (seq_along(ntemp))[match(rtwin, ntemp, nomatch = 0)]
        twins[pmin(ltemp, rtemp)] <- ttype
    } else {
        twins <- NULL
    }
    ## Doc: finish align(3)
    if ((is.numeric(align) || align) && max(level) > 1) {
        pos <- alignped4(rval, spouse > 0, level, width, align)
    } else {
        pos <- rval$pos
    }
    if (is.null(twins)) {
        list(n = rval$n, nid = nid, pos = pos, fam = rval$fam, spouse = spouse)
    } else {
        list(n = rval$n, nid = nid, pos = pos, fam = rval$fam, spouse = spouse,
            twins = twins
        )
    }
}
