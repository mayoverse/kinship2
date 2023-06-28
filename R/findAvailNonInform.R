# Automatically generated from all.nw using noweb

#' Find uninformative but available subject
#'
#' @details
#' Find subjects from a pedigree who are available and uninformative
#'
#' @details
#' Identify subjects to remove from a pedigree who are available but
#' non-informative.  This is the second step to remove subjects in
#' pedigree.shrink if the pedigree does not meet the desired bit size.
#'
#' @param ped A pedigree object
#' @param avail Vector of availability status (e.g. genotyped) 0/1 or
#' TRUE/FALSE
#'
#' @return Vector of subject ids who can be removed by having lowest
#' informativeness.
#'
#' @seealso `pedigree.shrink`
#' @export findAvailNonInform
findAvailNonInform <- function(ped, avail) {
  ## trim persons who are available but not informative b/c not parent
  ## by setting their availability to FALSE, then call findUnavailable()
  ## JPS 3/10/14 add strings check in case of char ids
  pedData <- data.frame(
    id = ped$id, father = ped$findex,
    mother = ped$mindex, avail = avail, stringsAsFactors = FALSE
  )

  checkParent <- is.parent(pedData$id, pedData$father, pedData$mother)

  for (i in 1:nrow(pedData)) {
    if (checkParent[i] == FALSE & avail[i] == TRUE &
      all(ped$affected[i] == 0, na.rm = TRUE)) {
      ## could use ped$affected[i,] if keep matrix

      fa <- pedData$id[pedData$father[i]]
      mo <- pedData$id[pedData$mother[i]]
      if (avail[pedData$id == fa] & avail[pedData$id == mo]) {
        pedData$avail[i] <- FALSE
      }
    }
  }

  idTrim <- findUnavailable(ped, pedData$avail)
  return(idTrim)
}
