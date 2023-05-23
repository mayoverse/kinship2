# Automatically generated from all.nw using noweb
## Authors: Dan Schaid, Shannon McDonnell
## Updated by Jason Sinnwell

#' Determine set of maximum number of unrelated available subjects from a
#' pedigree
#'
#' Determine set of maximum number of unrelated available subjects from a
#' pedigree, given vectors id, father, and mother for a pedigree structure, and
#' status vector of T/F for whether each subject is available (e.g. has DNA)
#'
#' This is a greedy algorithm that uses the kinship matrix, sequentially
#' removing rows/cols that are non-zero for subjects that have the most number
#' of zero kinship coefficients (greedy by choosing a row of kinship matrix
#' that has the most number of zeros, and then remove any cols and their
#' corresponding rows that are non-zero. To account for ties of the count of
#' zeros for rows, a random choice is made. Hence, running this function
#' multiple times can return different sets of unrelated subjects.
#'
#' @param ped A pedigree objects with unique id, father index, mother index
#' @param avail Vector of availability status (e.g., genotyped) 0/1 or
#' TRUE/FALSE
#' @return A vector of the ids of subjects that are unrelated.
#' @seealso \code{\link{kinship}}, \code{\link{pedigree}}
#' @examples
#'
#' data(sample.ped)
#' fam1 <- sample.ped[sample.ped$ped==1,]
#'
#'
#' ped1 <- pedigree(fam1$id, fam1$father, fam1$mother,
#'                  fam1$sex, fam1$affected, fam1$avail)
#'
#' ## to see plot:
#' ## plot.pedigree(ped1, align=FALSE)
#' id1 <- pedigree.unrelated(ped1, avail=fam1$avail)
#'
#' id1
#' ## some possible vectors
#' ##[1] "110" "113" "133" "109"
#' ##[1] "113" "118" "141" "109"
#' ##[1] "113" "118" "140" "109"
#' ##[1] "110" "113" "116" "109"
#' ##[1] "113" "133" "141" "109"
#'
#'
#' fam2 <- sample.ped[sample.ped$ped==2,]
#'
#' ped2 <- pedigree(fam2$id, fam2$father, fam2$mother,
#'                fam2$sex, fam2$affected, fam2$avail)
#'
#' ## to see plot:
#' ## plot.pedigree(ped2, align=FALSE)
#'
#' id2 <- pedigree.unrelated(ped2, avail=fam2$avail)
#'
#' ## some possible vectors
#' ##[1] "203" "207"
#' ##[1] "203" "204"
#' ##[1] "201" "203"
#' ##[1] "214" "203"
#' id2
#'
#' @export pedigree.unrelated
pedigree.unrelated <- function(ped, avail) {

  # Requires: kinship function

  # Given vectors id, father, and mother for a pedigree structure,
  # and avail = vector of T/F or 1/0 for whether each subject
  # (corresponding to id vector) is available (e.g.,
  # has DNA available), determine set of maximum number
  # of unrelated available subjects from a pedigree.

  # This is a greedy algorithm that uses the kinship
  # matrix, sequentially removing rows/cols that
  # are non-zero for subjects that have the most
  # number of zero kinship coefficients (greedy
  # by choosing a row of kinship matrix that has
  # the most number of zeros, and then remove any
  # cols and their corresponding rows that are non-zero.
  # To account for ties of the count of zeros for rows,
  # a random choice is made. Hence, running this function
  # multiple times can return different sets of unrelated
  # subjects.

  id <- ped$id
  avail <- as.integer(avail)
  
  kin <- kinship(ped)
  
  ord <- order(id)
  id <- id[ord]
  avail <- as.logical(avail[ord])
  kin <- kin[ord,][,ord]
  
  rord <- order(runif(nrow(kin)))
  
  id <- id[rord]
  avail <- avail[rord]
  kin <- kin[rord,][,rord]
  
  id.avail <- id[avail]
  kin.avail <- kin[avail,,drop=FALSE][,avail,drop=FALSE]
  
  diag(kin.avail) <- 0
  
  while(any(kin.avail > 0))  {
    nr <- nrow(kin.avail)
    indx <- 1:nrow(kin.avail)
    zero.count <- apply(kin.avail==0, 1, sum)
    
    mx <- max(zero.count[zero.count < nr])
    mx.zero <- indx[zero.count == mx][1]
    
    exclude <- indx[kin.avail[, mx.zero] > 0]
    
    kin.avail <- kin.avail[- exclude, , drop=FALSE][, -exclude, drop=FALSE]
    
  }
  
  choice <- sort(dimnames(kin.avail)[[1]])
  
  return(choice)
}

