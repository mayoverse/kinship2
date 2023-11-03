# Automatically generated from all.nw using noweb

#' Find uninformative but available subject
#'
#' @details
#' Find subjects from a Pedigree who are available and uninformative
#'
#' @details
#' Identify subjects to remove from a Pedigree who are available but
#' non-informative.  This is the second step to remove subjects in
#' pedigree.shrink if the Pedigree does not meet the desired bit size.
#'
#' @inheritParams align
#' @inheritParams is_informative
#' @inheritParams is_parent
#'
#' @examples
#' data(sampleped)
#' ped <- Pedigree(sampleped)
#' find_avail_noninform(ped)
#'
#' @return Vector of subject ids who can be removed by having lowest
#' informativeness.
#'
#' @seealso [shrink()]
#' @export
setGeneric("find_avail_noninform", signature = "obj",
    function(obj, ...) standardGeneric("find_avail_noninform")
)

setMethod("find_avail_noninform", "Ped",
    function(obj, avail = NULL, affected = NULL) {
        if (is.null(avail)) {
            avail <- avail(obj)
        }
        if (is.null(affected)) {
            affected <- affected(obj)
        }
        check_parent <- is_parent(id(obj), dadid(obj), momid(obj))
        for (i in seq_along(length(obj))) {
            if (check_parent[i] == FALSE && avail[i] == 1 &&
                    all(affected[i] == 0, na.rm = TRUE)) {
                ## could use ped$affected[i,] if keep matrix
                fa <- dadid(obj)[i]
                mo <- momid(obj)[i]
                if (avail[id(obj) == fa] && avail[id(obj) == mo] ||
                        is.na(fa) || is.na(mo)) {
                    avail[i] <- FALSE
                }
            }
        }

        find_unavailable(obj, avail)
    }
)
