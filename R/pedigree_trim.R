# Automatically generated from all.nw using noweb TODO add documentation
#' Trim a pedigree
#' @export
pedigree_trim <- function(ped, id_rm, missid = "0") {
    ## trim subjects from a pedigree who match the removeID trim relation
    ## matrix as well
    rmidx <- match(id_rm, ped$ped$id)
    if (length(rmidx) > 0) {
        ped$ped[ped$ped$dadid %in% id_rm |
                    ped$ped$momid %in% id_rm,
                c("dadid", "momid")] <- missid
        ped[-rmidx, ]
    } else {
        ped
    }
}
TRUE
