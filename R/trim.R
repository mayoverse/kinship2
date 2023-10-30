# Automatically generated from all.nw using noweb TODO add documentation
#' Trim a Pedigree
#'
#' Carries out the removal of the subjects identified from a Pedigree object.
#'
#' @inheritParams align
#' @inheritParams is_parent
#' @param id_rm Vector of ids to remove
#'
#' @return A Pedigree object with the subjects removed
#'
#' @examples
#' data(sampleped)
#' ped1 <- Pedigree(sampleped[sampleped$famid == "1",])
#' trim(ped1, "1_101")
#' @export
trim <- function(ped, id_rm, missid = "0") {
    ## trim subjects from a Pedigree who match the removeID trim relation
    ## matrix as well
    rmidx <- match(id_rm, ped(ped, "id"))
    if (length(rmidx) > 0) {
        ped(ped, c("dadid", "momid"))[ped(ped, "dadid") %in% id_rm |
                    ped(ped, "momid") %in% id_rm] <- missid
        ped[-rmidx, ]
    } else {
        ped
    }
}
TRUE
