# Automatically generated from all.nw using noweb

#' Find a single person to trim from a pedigree whose is available
#'
#' @description
#' Finds one subject from among available non-parents with indicated affection
#' status
#'
#' @details
#' When used within pedigree.shrink, this function is called with the first
#' affected indicator, if the affected item in the pedigree is a matrix of
#' multiple affected indicators.
#'
#' @param ped A pedigree objects, with id (subject ID), findex (father index),
#' mindex (mother index)
#' @param avail Vector of availability status (e.g., genotyped) 0/1 or
#' TRUE/FALSE
#' @param affstatus Vector of affection status 0/1 or TRUE/FALSE.
#'
#' @return A list is returned with the following components
#' \item{ped}{ Dataframe with trimmed subject removed }
#' \item{idTrimmed}{ Vector of IDs of trimmed individuals }
#' \item{isTrimmed}{ logical value indicating whether pedigree has been trimmed }
#' \item{bitSize}{ Bit size of the trimmed pedigree }
#'
#' @seealso \code{\link{pedigree.shrink}}
#' @export findAvailAffected
findAvailAffected <- function(ped, avail, affstatus)
                              ## Try trimming one subject by affection status indicator
## If ties for bits removed, randomly select one of the subjects
{
  notParent <- !is.parent(ped$id, ped$findex, ped$mindex)

  if (is.na(affstatus)) {
    possiblyTrim <- ped$id[notParent & avail & is.na(ped$affected)]
  } else {
    possiblyTrim <- ped$id[notParent & avail & ped$affected == affstatus]
  }
  nTrim <- length(possiblyTrim)

  if (nTrim == 0) {
    return(list(
      ped = ped,
      idTrimmed = NA,
      isTrimmed = FALSE,
      bitSize = bitSize(ped)$bitSize
    ))
  }

  trimDat <- NULL

  for (idTrim in possiblyTrim) {
    avail.try <- avail
    avail.try[ped$id == idTrim] <- FALSE
    id.rm <- findUnavailable(ped, avail.try)
    newPed <- pedigree.trim(id.rm, ped)
    trimDat <- rbind(
      trimDat,
      c(id = idTrim, bitSize = bitSize(newPed)$bitSize)
    )
  }

  bits <- trimDat[, 2]

  # trim by subject with min bits. This trims fewer subject than
  # using max(bits).
  idTrim <- trimDat[, 1][bits == min(bits)]

  ## break ties by random choice
  if (length(idTrim) > 1) {
    rord <- order(runif(length(idTrim)))
    idTrim <- idTrim[rord][1]
  }

  avail[ped$id == idTrim] <- FALSE
  id.rm <- findUnavailable(ped, avail)
  newPed <- pedigree.trim(id.rm, ped)
  pedSize <- bitSize(newPed)$bitSize
  avail <- avail[!(ped$id %in% id.rm)]

  return(list(
    ped = newPed,
    newAvail = avail,
    idTrimmed = idTrim,
    isTrimmed = TRUE,
    bitSize = pedSize
  ))
}
