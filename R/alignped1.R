# Automatically generated from all.nw using noweb

# TODO add params and example

#' First routine alignement
#'
#' @description
#' This is the first of the four co-routines.
#'
#' @details
#' It is called with a single subject, and returns the subtree founded on said
#' subject, as though it were the only tree.
#'
#' \item{Part 1}{In this routine the \code{[[nid]]} array consists of the final
#' \code{nid array + 1/2} of the final spouse array.
#' Note that the \code{[[spouselist]]} matrix will only contain spouse pairs
#' that are not yet processed. The logic for anchoring is slightly tricky.
#' First, if row 4 of the spouselist matrix is 0, we anchor at the first
#' opportunity. Also note that if \code{spouselist[,3]==spouselist[,4]}
#' it is the husband who is the anchor (just write out the possibilities). }
#' \item{Part 2}{Create the set of 3 return structures, which will be matrices
#' with (1+nspouse) columns.
#' If there are children then other routines will widen the result.}
#' \item{Part 3}{Create the two complimentary lists lspouse and rspouse to
#' denote those plotted on the left and on the right. For someone with lots
#' of spouses we try to split them evenly. If the number of spouses is odd,
#' then men should have more on the right than on the left, women more on the
#' right. Any hints in the spouselist matrix override.
#' We put the undecided marriages closest to \code{[[x]]}, then add
#' predetermined ones to the left and right. The majority of marriages will be
#' undetermined singletons, for which nleft will be 1 for female (put my husband
#' to the left) and 0 for male. In one bug found by plotting canine data,
#' lspouse could initially be empty but \code{length(rspouse)> 1}.
#' This caused \code{nleft>length(indx)}.
#' A fix was to not let indx to be indexed beyond its length,
#' fix by JPS 5/2013.}
#' \item{Part 4}{For each spouse get the list of children. If there are any we
#' call alignped2 to generate their tree and then mark the connection to their
#' parent. If multiple marriages have children we need to join the trees. }
#' \item{Part 5}{ To finish up we need to splice together the tree made up from
#' all the kids, which only has data from lev+1 down, with the data here.
#' There are 3 cases:
#' The first and easiest is when no children were found.
#' The second is when the tree below is wider than the tree here,
#' in which case we add the data from this level onto theirs.
#' The third is when below is narrower, for instance an only child. }
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
#' @seealso \code{\link{plot.pedigree}}, \code{\link{autohint}}
#' @keywords dplot
#' @export alignped1
alignped1 <- function(x, dad, mom, level, horder, packed, spouselist) {
  # Set a few constants
  maxlev <- max(level)
  lev <- level[x]
  n <- integer(maxlev)

  if (length(spouselist) == 0) {
    spouse <- NULL
  } else {
    if (any(spouselist[, 1] == x)) {
      sex <- 1 # I'm male
      sprows <- (spouselist[, 1] == x & (spouselist[, 4] == spouselist[, 3] |
        spouselist[, 4] == 0))
      spouse <- spouselist[sprows, 2] # ids of the spouses
    } else {
      sex <- 2
      sprows <- (spouselist[, 2] == x & (spouselist[, 4] != spouselist[, 3] |
        spouselist[, 4] == 0))
      spouse <- spouselist[sprows, 1]
    }
  }
  # Marriages that cross levels are plotted at the higher level (lower
  #  on the paper).
  if (length(spouse)) {
    keep <- level[spouse] <= lev
    spouse <- spouse[keep]
    sprows <- (which(sprows))[keep]
  }
  nspouse <- length(spouse) # Almost always 0, 1 or 2
  ## Doc: alignped1 part2
  nid <- fam <- matrix(0L, maxlev, nspouse + 1)
  pos <- matrix(0.0, maxlev, nspouse + 1)
  n[lev] <- nspouse + 1
  pos[lev, ] <- 0:nspouse
  if (nspouse == 0) {
    # Easy case: the "tree rooted at x" is only x itself
    nid[lev, 1] <- x
    return(list(nid = nid, pos = pos, fam = fam, n = n, spouselist = spouselist))
  }
  ## Doc: alignped1 -part3
  lspouse <- spouse[spouselist[sprows, 3] == 3 - sex] # 1-2 or 2-1
  rspouse <- spouse[spouselist[sprows, 3] == sex] # 1-1 or 2-2
  if (any(spouselist[sprows, 3] == 0)) {
    # Not yet decided spouses
    indx <- which(spouselist[sprows, 3] == 0)
    nleft <- floor((length(sprows) + (sex == 2)) / 2) # total number to left
    nleft <- nleft - length(lspouse) # number of undecideds to the left
    if (nleft > 0) {
      # JPS fixed 5/2013, don't index when nleft > length(indx)
      lspouse <- c(lspouse, spouse[indx[seq_len(min(nleft, length(indx)))]])
      indx <- indx[-(seq_len(min(nleft, length(indx))))]
    }
    if (length(indx)) rspouse <- c(spouse[indx], rspouse)
  }

  nid[lev, ] <- c(lspouse, x, rspouse)
  nid[lev, 1:nspouse] <- nid[lev, 1:nspouse] + .5 # marriages

  spouselist <- spouselist[-sprows, , drop = FALSE]
  ## Doc: alignped1 - part4
  nokids <- TRUE # haven't found any kids yet
  spouse <- c(lspouse, rspouse) # reorder
  for (i in 1:nspouse) {
    ispouse <- spouse[i]
    children <- which((dad == x & mom == ispouse) | (dad == ispouse & mom == x))
    if (length(children) > 0) {
      rval1 <- alignped2(
        children, dad, mom, level, horder,
        packed, spouselist
      )
      spouselist <- rval1$spouselist
      # set the parentage for any kids
      #  a nuisance: it's possible to have a child appear twice, when
      #  via inbreeding two children marry --- makes the "indx" line
      #  below more complicated
      temp <- floor(rval1$nid[lev + 1, ]) # cut off the .5's for matching
      indx <- (1:length(temp))[match(temp, children, nomatch = 0) > 0]
      rval1$fam[lev + 1, indx] <- i # set the kids parentage
      if (!packed) {
        # line the kids up below the parents
        # The advantage at this point: we know that there is
        #   nothing to the right that has to be cared for
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
              rval1$pos[j, 1:jn] <- rval1$pos[j, 1:jn] + shift
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
    return(list(nid = nid, pos = pos, fam = fam, n = n, spouselist = spouselist))
  }

  if (ncol(rval$nid) >= 1 + nspouse) {
    # The rval list has room for me!
    rval$n[lev] <- n[lev]
    indx <- 1:(nspouse + 1)
    rval$nid[lev, indx] <- nid[lev, ]
    rval$pos[lev, indx] <- pos[lev, ]
  } else {
    # my structure has room for them
    indx <- 1:ncol(rval$nid)
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
