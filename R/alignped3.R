# Automatically generated from all.nw using noweb

# TODO add params and example and return

#' Third routine alignement
#'
#' @description
#' This is the third of the four co-routines.
#'
#' @details
#' This co-routine merges two pedigree trees which are side by side into a
#' single object. The primary special case is when the rightmost person in
#' the left tree is the same as the leftmost person in the right tree; we
#' need not plot two copies of the same person side by side.
#' (When initializing the output structures do not worry about this,
#' there is no harm if they are a column bigger than finally needed.)
#' Beyond that the work is simple bookkeeping.
#'
#' \item{Slide}{ For the unpacked case, which is the traditional way to draw
#' a pedigree when we can assume the paper is infinitely wide, all parents are
#' centered over their children. In this case we think if the two trees to be
#' merged as solid blocks. On input they both have a left margin of 0.
#' Compute how far over we have to slide the right tree.}
#' \item{Merge}{ Now merge the two trees. Start at the top level and work down.}
#'
#' @param x1
#' @param x2
#' @param packed
#' @param space
#'
#' @return A list of element containing:
#' \item{n}{}
#' \item{nid}{}
#' \item{pos}{}
#' \item{fam}{}
#'
#' @examples
#' data(sample.ped)
#' ped <- with(sample.ped, pedigree(id, father, mother, sex, affected))
#' align.pedigree(ped)
#'
#' @seealso \code{\link{plot.pedigree}}, \code{\link{autohint}}
#' @keywords dplot
#' @export alignped3
alignped3 <- function(x1, x2, packed, space = 1) {
  maxcol <- max(x1$n + x2$n)
  maxlev <- length(x1$n)
  n1 <- max(x1$n) # These are always >1
  n <- x1$n + x2$n

  nid <- matrix(0, maxlev, maxcol)
  nid[, 1:n1] <- x1$nid
  pos <- matrix(0.0, maxlev, maxcol)
  pos[, 1:n1] <- x1$pos

  fam <- matrix(0, maxlev, maxcol)
  fam[, 1:n1] <- x1$fam
  fam2 <- x2$fam
  if (!packed) {
    ## Doc: alignped3: slide
    slide <- 0
    for (i in 1:maxlev) {
      n1 <- x1$n[i]
      n2 <- x2$n[i]
      if (n1 > 0 & n2 > 0) {
        if (nid[i, n1] == x2$nid[i, 1]) {
          temp <- pos[i, n1] - x2$pos[i, 1]
        } else {
          temp <- space + pos[i, n1] - x2$pos[i, 1]
        }
        if (temp > slide) slide <- temp
      }
    }
  }
  ## Doc: alignped3-merge
  for (i in 1:maxlev) {
    n1 <- x1$n[i]
    n2 <- x2$n[i]
    if (n2 > 0) { # If anything needs to be done for this row...
      if (n1 > 0 && (nid[i, n1] == floor(x2$nid[i, 1]))) {
        # two subjects overlap
        overlap <- 1
        fam[i, n1] <- max(fam[i, n1], fam2[i, 1])
        nid[i, n1] <- max(nid[i, n1], x2$nid[i, 1]) # preserve a ".5"
        if (!packed) {
          if (fam2[i, 1] > 0) {
            if (fam[i, n1] > 0) {
              pos[i, n1] <- (x2$pos[i, 1] + pos[i, n1] + slide) / 2
            } else {
              pos[i, n1] <- x2$pos[i, 1] + slide
            }
          }
        }
        n[i] <- n[i] - 1
      } else {
        overlap <- 0
      }

      if (packed) slide <- if (n1 == 0) 0 else pos[i, n1] + space - overlap

      zz <- seq(from = overlap + 1, length = n2 - overlap)
      nid[i, n1 + zz - overlap] <- x2$nid[i, zz]
      fam[i, n1 + zz - overlap] <- fam2[i, zz]
      pos[i, n1 + zz - overlap] <- x2$pos[i, zz] + slide

      if (i < maxlev) {
        # adjust the pointers of any children (look ahead)
        temp <- fam2[i + 1, ]
        fam2[i + 1, ] <- ifelse(temp == 0, 0, temp + n1 - overlap)
      }
    }
  }
  ## Doc: rest of alignped3
  if (max(n) < maxcol) {
    maxcol <- max(n)
    nid <- nid[, 1:maxcol]
    pos <- pos[, 1:maxcol]
    fam <- fam[, 1:maxcol]
  }

  list(n = n, nid = nid, pos = pos, fam = fam)
}
