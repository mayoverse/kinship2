#' Ancestors indexes of a subject
#'
#' @description Given the index of one or multiple individual(s), this
#' function iterate through the mom and dad indexes to
#' list out all the ancestors of the said individual(s).
#' This function is use in the [align()] function to
#' identify which spouse pairs has a common ancestor and
#' therefore if they need to be connected with a double line
#' (i.e. inbred).
#'
#' @param idx Indexes of the subjects
#' @param dadx Indexes of the fathers
#' @param momx Indexes of the mothers
#'
#' @examples
#' ancestors(c(1), c(3, 4, 5, 6), c(7, 8, 9, 10))
#' ancestors(c(1, 2), c(3, 4, 5, 6), c(7, 8, 9, 10))
#' @return A vector of ancestor indexes
#' @keywords internal
#' @seealso [align()]
#' @export
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
    alist[!alist %in% idx]
}


#' Align a Pedigree object
#'
#' @description
#' Given a Pedigree, this function creates helper matrices that describe the
#' layout of a plot of the Pedigree.
#'
#' @details
#' This is an internal routine, used almost exclusively by
#' [ped_to_plotdf()].
#'
#' The subservient functions [auto_hint()],
#' [alignped1()], [alignped2()],
#' [alignped3()], and [alignped4()]
#' contain the bulk of the computation.
#'
#' If the **hints** are missing the [auto_hint()] routine is called to
#' supply an initial guess.
#'
#' If multiple families are present in the **obj** Pedigree, this routine
#' is called once for each family, and the results are combined in the
#' list returned.
#'
#' For more information you can read the associated vignette:
#' `vignette("pedigree_alignment")`.
#'
#' @param obj A Pedigree object
#' @param packed Should the Pedigree be compressed.
#' (i.e. allow diagonal lines connecting parents to children in order
#' to have a smaller overall width for the plot.)
#' @param width For a packed output, the minimum width of the plot, in
#' inches.
#' @param align For a packed Pedigree, align children under parents `TRUE`,
#' to the extent possible given the page width, or align to to the left
#' margin `FALSE`.
#' This argument can be a two element vector, giving the alignment
#' parameters, or a logical value.
#' If `TRUE`, the default is `c(1.5, 2)`, or if numeric the routine
#' `alignped4()` will be called.
#' @param hints A Hints object or a named list containing `horder` and
#' `spouse`. If `NULL` then the Hints stored in **obj** will be used.
#' @inheritParams Ped
#'
#' @return A list with components
#' - `n`: A vector giving the number of subjects on each horizonal level of the
#' plot
#' - `nid`: A matrix with one row for each level, giving the numeric id of
#' each subject plotted.
#' (A value of `17` means the 17th subject in the Pedigree).
#' - `pos`: A matrix giving the horizontal position of each plot point
#' - `fam`: A matrix giving the family id of each plot point.
#' A value of `3` would mean that the two subjects in positions 3 and 4,
#' in the row above, are this subject's parents.
#' - `spouse`: A matrix with values
#'     - `0` = not a spouse
#'     - `1` = subject plotted to the immediate right is a spouse
#'     - `2` = subject plotted to the immediate right is an inbred spouse
#' - `twins`: Optional matrix which will only be present if the Pedigree
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
#' @include AllClass.R
#' @include alignped1.R
#' @include alignped2.R
#' @include alignped3.R
#' @include alignped4.R
#' @rdname align
#' @usage NULL
setGeneric("align", signature = "obj",
    function(obj, ...) standardGeneric("align")
)

#' @rdname align
#' @docType methods
setMethod("align", "Pedigree",
    function(
        obj, packed = TRUE, width = 10,
        align = TRUE, hints = NULL, missid = "NA_character_"
    ) {
        famlist <- unique(famid(obj))
        if (length(famlist) > 1) {
            nfam <- length(famlist)
            alignment <- vector("list", nfam)
            for (i_fam in famlist) {
                ped_fam <- obj[famid(obj) == i_fam]
                alignment[[i_fam]] <- align(ped_fam, packed, width, align)
            }
            return(alignment)
        }
        if (is.null(hints)) {
            hints <- hints(obj)
        }
        if (!is(hints, "Hints")) {
            if (!is.list(hints)) {
                stop("hints argument should be a Hints object or a list")
            }
            hints <- Hints(hints)
        }
        if (length(horder(hints)) == 0) {
            hints <- try({
                auto_hint(obj)
            }, silent = TRUE)
            if ("try-error" %in% class(hints)) {
                hints <- Hints(horder = setNames(
                    seq_len(length(ped(obj))), id(ped(obj))
                ))
            }
        }
        ## Doc: Setup-align
        n <- length(obj)

        level <- 1 + kindepth(obj, align_parents = TRUE)
        ## relative order of siblings within a family
        horder <- horder(hints)

        if (nrow(spouse(hints)) > 0) {
            # start with the hints list
            idxl <- match(spouse(hints)$idl, id(ped(obj)))
            idxr <- match(spouse(hints)$idr, id(ped(obj)))
            tsex <- sex(ped(obj))[idxl]  # sex of the left member
            spouselist <- cbind(
                0, 0, 1 + (tsex != "male"), as.numeric(spouse(hints)$anchor)
            )
            spouselist[, 1] <- ifelse(tsex == "male", idxl, idxr)
            spouselist[, 2] <- ifelse(tsex == "male", idxr, idxl)
        } else {
            spouselist <- matrix(0L, nrow = 0, ncol = 4)
        }
        if (any(code(rel(obj)) == "Spouse")) {
            # Add spouses from the relationship matrix
            trel <- as.data.frame(rel(obj))
            trel <- trel[trel$code == "Spouse", c("id1", "id2")]
            trel$id1 <- match(trel$id1, id(ped(obj)))
            trel$id2 <- match(trel$id2, id(ped(obj)))
            tsex <- sex(ped(obj))[trel$id1]
            trel[tsex != "male", seq_len(2)] <- trel[tsex != "male", 2:1]
            spouselist <- rbind(spouselist, cbind(trel[, 1], trel[, 2], 0, 0))
        }

        dad <- match(dadid(ped(obj)), id(ped(obj)), nomatch = 0)
        mom <- match(momid(ped(obj)), id(ped(obj)), nomatch = 0)
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
        rval <- alignped1(
            founders[1], dad, mom, level, horder, packed, spouselist
        )
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
        if (length(rel(obj)) > 0 && any(code(rel(obj)) != "Spouse")) {
            twins <- 0 * nid
            who <- (code(rel(obj)) != "Spouse")
            ltwin <- match(id1(rel(obj))[who], id(ped(obj)), nomatch = 0)
            rtwin <- match(id2(rel(obj))[who], id(ped(obj)), nomatch = 0)
            ttype <- code(rel(obj))[who]
            ## find where each of them is plotted
            ## (any twin only appears once with a
            ## family id, i.e., under their parents)
            ## matrix of connected-to-parent ids
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
            list(
                n = rval$n, nid = nid, pos = pos,
                fam = rval$fam, spouse = spouse
            )
        } else {
            list(
                n = rval$n, nid = nid, pos = pos,
                fam = rval$fam, spouse = spouse,
                twins = twins
            )
        }
    }
)