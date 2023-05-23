#' Shrink pedigree object
#'
#' Shrink pedigree object to specified bit size with priority placed on
#' trimming uninformative subjects. The algorithm is useful for getting a
#' pedigree condensed to a minimally informative size for algorithms or testing
#' that are limited by size of the pedigree.
#'
#' Iteratively remove subjects from the pedigree. The random removal of members
#' was previously controlled by a seed argument, but we remove this, forcing
#' users to control randomness outside the function. First remove uninformative
#' subjects, i.e., unavailable (not genotyped) with no available descendants.
#' Next, available terminal subjects with unknown phenotype if both parents
#' available. Last, iteratively shrinks pedigrees by preferentially removing
#' individuals (chosen at random if there are multiple of the same status): 1.
#' Subjects with unknown affected status, 2. Subjects with unaffected affected
#' status 3. Affected subjects.
#'
#' @aliases pedigree.shrink print.pedigree.shrink
#' @param ped Pedigree object created by the pedigree function,
#' @param avail vector of binary availability status (0/1), i.e. having data,
#' or sample available
#' @param affected vector of binary affected status (0/1/NA). If NULL, uses
#' first column of the pedigree object affected matrix.
#' @param maxBits Optional, the bit size for which to shrink the pedigree
#' @param x pedigree.shrink object used in method functions
#' @param ... optional arguments passed to internal functions
#' @author Original by Dan Schaid, updated to kinship2 by Jason Sinnwell
#' @seealso \code{\link{pedigree}}, \code{\link{plot.pedigree.shrink}}
#' @examples
#'
#' data(sample.ped)
#' pedAll <- pedigree(sample.ped$id, sample.ped$father, sample.ped$mother, sample.ped$sex, affected=cbind(sample.ped$affected, sample.ped$avail), famid=sample.ped$ped)
#' ped1 <- pedAll['1']
#' ped1trim <- pedigree.shrink(ped1, maxBits=12)
#'
#' @export pedigree.shrink
pedigree.shrink <- function(ped, avail, affected=NULL, maxBits = 16) {
  if(!inherits(ped, "pedigree"))
    stop("Must be a pegigree object.\n")  
  
  if(any(is.na(avail)))
    stop("NA values not allowed in avail vector.")
  
  if(is.null(affected))
    affected = if(is.matrix(ped$affected)) ped$affected[,1] else ped$affected
  
  ped$affected = affected
  
  idTrimmed <- numeric()
  idList <- list()
  nOriginal <- length(ped$id)
  
  bitSizeOriginal <- bitSize(ped)$bitSize
  
  ## first find unavailable subjects to remove anyone who is not 
  ## available and does not have an available descendant
  
  idTrimUnavail <- findUnavailable(ped, avail)
  
  if(length(idTrimUnavail)) {    
    
    pedTrimmed <- pedigree.trim(idTrimUnavail, ped)
    avail <- avail[match(pedTrimmed$id, ped$id)]
    idTrimmed <- c(idTrimmed, idTrimUnavail)
    idList$unavail <- paste(idTrimUnavail, collapse=' ')
    
  } else {
  ## no trimming, reset to original ped
    pedTrimmed <- ped
  }
  
  ## Next trim any available terminal subjects with unknown phenotype
  ## but only if both parents are available
  
  ## added nNew>0 check because no need to trim anymore if empty ped
  
  nChange <- 1
  idList$noninform = NULL
  nNew <- length(pedTrimmed$id)
  
  while(nChange > 0 & nNew > 0){
    nOld <- length(pedTrimmed$id)
    
    ## findAvailNonInform finds non-informative, but after suggesting 
    ## their removal, checks for more unavailable subjects before returning
    idTrimNonInform <- findAvailNonInform(pedTrimmed, avail)
    
    if(length(idTrimNonInform)) {
      pedNew <- pedigree.trim(idTrimNonInform, pedTrimmed)
      avail <- avail[match(pedNew$id, pedTrimmed$id)]
      idTrimmed <- c(idTrimmed, idTrimNonInform)
      idList$noninform = paste(c(idList$noninform, 
                                 idTrimNonInform), collapse=' ')
      pedTrimmed <- pedNew
      
    }
    nNew <- length(pedTrimmed$id)
    nChange <- nOld - nNew
    
  }
  
  ##  Determine number of subjects & bitSize after initial trimming
  nIntermed <- length(pedTrimmed$id)
  
  bitSize <- bitSize(pedTrimmed)$bitSize
  
  ## Now sequentially shrink to fit bitSize <= maxBits
  
  bitVec <- c(bitSizeOriginal,bitSize)
  
  isTrimmed <- TRUE
  idList$affect=NULL 
  
  while(isTrimmed & (bitSize > maxBits))
  {  
    
    ## First, try trimming by unknown status
    save <- findAvailAffected(pedTrimmed, avail, affstatus=NA)
    isTrimmed <- save$isTrimmed
    
    ## Second, try trimming by unaffected status if no unknowns to trim
    if(!isTrimmed)
    {
      save <- findAvailAffected(pedTrimmed, avail, affstatus=0)
      isTrimmed <- save$isTrimmed
      
    }
    
    
   ## Third, try trimming by affected status if no unknowns & no unaffecteds
   ## to trim
    if(!isTrimmed) {
      save <- findAvailAffected(pedTrimmed, avail, affstatus=1)
      isTrimmed <- save$isTrimmed
    }
    
    if(isTrimmed)  {
      pedTrimmed <- save$ped
      avail <- save$newAvail
      bitSize <- save$bitSize
      bitVec <- c(bitVec, bitSize)          
      idTrimmed <- c(idTrimmed, save$idTrimmed)
      idList$affect = paste(c(idList$affect, save$idTrimmed), 
                            collapse=' ')
    }
   
    
  } 
    ## end while (isTrimmed) & (bitSize > maxBits)

  nFinal <- length(pedTrimmed$id)
  
  obj <- list(pedObj = pedTrimmed,
              idTrimmed = idTrimmed,
              idList = idList,
              bitSize = bitVec,
              avail=avail,
              pedSizeOriginal = nOriginal,
              pedSizeIntermed = nIntermed,
              pedSizeFinal  = nFinal)
  
  class(obj) <- "pedigree.shrink"
  
  return(obj)
} 

#' @rdname pedigree.shrink
#' @method print pedigree.shrink
#' @export
print.pedigree.shrink <- function(x, ...){

    cat("Pedigree Size:\n")

    if(length(x$idTrimmed) > 2)
    {
        n <- c(x$pedSizeOriginal, x$pedSizeIntermed, x$pedSizeFinal)
        b <- c(x$bitSize[1], x$bitSize[2], x$bitSize[length(x$bitSize)])
        row.nms <- c("Original","Only Informative","Trimmed")
    } else {
        n <- c(x$pedSizeOriginal, x$pedSizeIntermed)
        b <- c(x$bitSize[1], x$bitSize[2])
        row.nms <- c("Original","Trimmed")
    }
    
    df <- data.frame(N.subj = n, Bits = b)
    rownames(df) <- row.nms
    print(df, quote=FALSE)
    
    if(!is.null(x$idList$unavail)) 
        cat("\n Unavailable subjects trimmed:\n", x$idList$unavail, "\n")
    
    if(!is.null(x$idList$noninform)) 
        cat("\n Non-informative subjects trimmed:\n", x$idList$noninform, "\n")
    
    if(!is.null(x$idList$affect)) 
        cat("\n Informative subjects trimmed:\n", x$idList$affect, "\n")
    
    invisible()
}
