#' Find uninformative but available subject
#'
#' Finds subjects from among available non-parents with all affection
#' equal to `0`.
#'
#' @details
#' Identify subjects to remove from a Pedigree who are available but
#' non-informative (unaffected).  This is the second step to remove subjects in
#' [shrink()] if the Pedigree does not meet the desired bit size.
#'
#' If **avail** or **affected** is null, then the function will use the
#' corresponding Ped accessor.
#'
#' @inheritParams find_avail_affected
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
#' @keywords internal, shrink
#' @export
#' @usage NULL
setGeneric("find_avail_noninform", signature = "obj",
    function(obj, ...) standardGeneric("find_avail_noninform")
)

#' @rdname find_avail_noninform
#' @export
setMethod("find_avail_noninform", "Ped",
    function(obj, avail = NULL, affected = NULL) {
        if (is.null(avail)) {
            avail <- avail(obj)
        }
        if (is.null(affected)) {
            ## TODO affected() may need to give back data.frame
            affected <- affected(obj)
        }
        check_parent <- is_parent(id(obj), dadid(obj), momid(obj))

        # For each individual if not a parent and unaffected
        # Set its avail to FALSE if both parent avail
        # or if one is absent
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

#' @rdname find_avail_noninform
#' @export
setMethod("find_avail_noninform", "Pedigree",
    function(obj, avail = NULL, affected = NULL) {
        find_avail_noninform(ped(obj), avail, affected)
    }
)
