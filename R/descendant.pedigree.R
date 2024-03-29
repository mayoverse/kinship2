# TODO add details param, return, examples, keywords, seealso

#' Find all the descendants
#'
#' @description
#' Find all the descendants of a particular person given a pedigree or a list
#'
#' @param idlist
#' @param id
#' @param dad.id
#' @param mom.id
#'
#' @return
#'
#' @examples
#' @seealso \code{\link{}}, \code{\link{}}
#' @keywords
#' @export descendant.pedigree
descendant.pedigree <- function(idlist, id, dad.id, mom.id) {
  child <- id[!(is.na(match(dad.id, idlist)) & is.na(match(mom.id, idlist)))]
  descend <- NULL
  while (length(child > 0)) {
    newchild <- id[!(is.na(match(dad.id, child)) &
      is.na(match(mom.id, child)))]
    descend <- unique(c(descend, child))
    child <- newchild[is.na(match(newchild, c(idlist, descend)))]
  }
  descend
}
