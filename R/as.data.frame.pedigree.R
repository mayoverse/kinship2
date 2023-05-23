#' data.frame from a pedigree object
#'
#' Extract the internal data from a pedigree object into a data.frame
#'
#' @param x pedigree object
#' @param ... additional arguments passed to internal methods
#' @return A data.frame with the data necessary to re-create the pedigree,
#' minus special relationships.
#' @author Jason Sinnwell
#' @seealso \code{\link{pedigree}}
#' @export as.data.frame.pedigree
as.data.frame.pedigree <- function(x, ...) {

  dadid <- momid <- rep(0, length(x$id))
  dadid[x$findex>0] <- x$id[x$findex]
  momid[x$mindex>0] <- x$id[x$mindex]
  df <- data.frame(id=x$id, dadid=dadid, momid=momid, sex=x$sex)
  
  if(!is.null(x$affected))
    df$affected = x$affected
  
  if(!is.null(x$status))
    df$status = x$status
  return(df)
}
