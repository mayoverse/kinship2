# Automatically generated from all.nw using noweb
# TODO add documentation

#' Convert ped to data.frame
#'
#' @description
#' Convert a pedigree object to adata.frame
#'
#' @param ped
#'
#' @return A dataframe containing the following columns:affected
#' - id
#' - findex
#' - mindex
#' - sex
#' - affected (optional)
#' - status (optional)
#'
#' @examples
#' data(sample.ped)
#' ped <- with(sample.ped, pedigree(id, father, mother, sex))
#' ped2df(ped)
#' @seealso \code{\link{}}, \code{\link{}}
#' @keywords
#' @export ped2df
ped2df <- function(ped) {
  df <- data.frame(
    id = ped$id, findex = ped$findex,
    mindex = ped$mindex, sex = ped$sex
  )
  if (!is.null(ped$affected)) {
    df$affected <- ped$affected
  }
  if (!is.null(ped$status)) {
    df$status <- ped$status
  }
  return(df)
}
