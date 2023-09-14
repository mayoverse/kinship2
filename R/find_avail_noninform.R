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
#' @inheritParams align
#' @inheritParams is_informative
#' @inheritParams is_parent
#'
#' @examples
#' data(sampleped)
#' ped <- pedigree(sampleped)
#' find_avail_noninform(ped)
#'
#' @return Vector of subject ids who can be removed by having lowest
#' informativeness.
#'
#' @seealso [shrink()]
#' @export
find_avail_noninform <- function(ped, avail = ped$ped$avail, missid = "0") {
    ## trim persons who are available but not informative b/c not parent by
    ## setting their availability to FALSE, then call find_unavailable() JPS
    ## 3/10/14 add strings check in case of char ids
    ped_df <- ped$ped
    ped_df$avail <- avail

    check_parent <- is_parent(ped_df$id, ped_df$dadid, ped_df$momid)
    for (i in seq_along(nrow(ped_df))) {
        if (check_parent[i] == FALSE && avail[i] == 1 &&
                all(ped_df$affected[i] == 0, na.rm = TRUE)) {
            ## could use ped$affected[i,] if keep matrix
            fa <- ped_df$dadid[i]
            mo <- ped_df$momid[i]
            if (avail[ped_df$id == fa] && avail[ped_df$id == mo] ||
                    fa == missid || mo == missid) {
                ped_df$avail[i] <- FALSE
            }
        }
    }

    find_unavailable(ped, ped_df$avail)
}
