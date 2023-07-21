# TODO add details param, return, examples, keywords

#' Find all the descendants
#'
#' @description
#' Find all the descendants of a particular person given a pedigree or a list
#'
#' @param idlist
#' @param id
#' @param dadid
#' @param momid
#'
#' @return
#'
#' @examples
#' @keywords
#' @export descendant.pedigree
descendant.pedigree <- function(idlist, id, dadid, momid) {
    child <- id[!(is.na(match(dadid, idlist)) & is.na(match(momid, idlist)))]
    descend <- NULL
    while (length(child > 0)) {
        newchild <- id[!(is.na(match(dadid, child)) &
            is.na(match(momid, child)))]
        descend <- unique(c(descend, child))
        child <- newchild[is.na(match(newchild, c(idlist, descend)))]
    }
    descend
}
TRUE
