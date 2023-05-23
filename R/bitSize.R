# Automatically generated from all.nw using noweb
## renamed from pedBits, part of pedigree.shrink functions

#' Calculate pedigree bitsize, defined as 2 * # NonFounders - # Founders
#'
#' This is a utility function used in pedigree.shrink()
#'
#' @param ped A pedigree object
#' @return A list with the following components: \item{bitSize}{ The bitSize of
#' input pedigree } \item{nFounder}{ The number of founders in the pedigree }
#' \item{nNonFounder}{ The number of nonfounders in the pedgiree }
#' @seealso \code{\link{pedigree.shrink}}
#' @export bitSize
bitSize <- function(ped) {
  ## calculate bit size of a pedigree

  if(!("pedigree" %in% class(ped)))
    stop("Must be a pegigree object.\n")

  father = ped$findex
  mother = ped$mindex
  id = ped$id

  founder <- father==0 & mother==0
  pedSize <- length(father)
  nFounder <- sum(founder)
  nNonFounder <- pedSize - nFounder
  bitSize <- 2*nNonFounder - nFounder  
  return(list(bitSize=bitSize,
    nFounder = nFounder,
    nNonFounder = nNonFounder))
}

