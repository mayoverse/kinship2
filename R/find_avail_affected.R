#' Find single affected and available individual from a Pedigree
#'
#' @description
#' Finds one subject from among available non-parents with indicated affection
#' status.
#'
#' @details
#' When used within [shrink()], this function is called with the first
#' affected indicator, if the affected item in the Pedigree is a matrix of
#' multiple affected indicators.
#'
#' If **avail** or **affected** is null, then the function will use the
#' corresponding Ped accessor.
#'
#' @param affstatus Affection status to search for.
#' @param obj A Ped or Pedigree object.
#' @inheritParams Ped
#'
#' @return A list is returned with the following components
#' - ped The new Ped object
#' - newAvail Vector of availability status of trimmed individuals
#' - idTrimmed Vector of IDs of trimmed individuals
#' - isTrimmed logical value indicating whether Ped object has been trimmed
#' - bit_size Bit size of the trimmed Ped
#'
#' @examples
#' data(sampleped)
#' ped <- Pedigree(sampleped)
#' find_avail_affected(ped, affstatus = 1)
#' @seealso [shrink()]
#' @include bit_size.R
#' @include utils.R
#' @include find_unavailable.R
#' @keywords internal, shrink
#' @export
#' @usage NULL
setGeneric("find_avail_affected", signature = "obj",
    function(obj, ...) standardGeneric("find_avail_affected")
)

#' @rdname find_avail_affected
#' @export
setMethod("find_avail_affected", "Ped",
    function(obj, avail = NULL, affected = NULL, affstatus = NA) {
        if (is.null(avail)) {
            avail <- avail(obj)
        }
        if (is.null(affected)) {
            affected <- affected(obj)
        }
        not_parent <- !is_parent(id(obj), dadid(obj), momid(obj))

        if (is.na(affstatus)) {
            possibl_trim <- id(obj)[not_parent & avail == 1 &
                    is.na(affected)
            ]
        } else {
            possibl_trim <- id(obj)[not_parent & avail == 1 &
                    affected == affstatus
            ]
        }
        n_trim <- length(possibl_trim)

        if (n_trim == 0) {
            return(list(
                ped = obj, id_trimmed = NA, is_trimmed = FALSE,
                bit_size = bit_size(obj)$bit_size
            ))
        }

        trim_dat <- NULL

        for (id_trim in possibl_trim) {
            tmp_avail <- avail
            tmp_avail[id(obj) == id_trim] <- FALSE
            id_rm <- find_unavailable(obj, tmp_avail)
            new_ped <- subset(obj, id_rm, keep = FALSE, del_parents = TRUE)
            trim_dat <- rbind(trim_dat, c(id = id_trim,
                bit_size = bit_size(new_ped)$bit_size
            ))
        }

        bits <- trim_dat[, 2]

        # trim by subject with min bits. This trims fewer subject than using
        # max(bits).
        id_trim <- trim_dat[, 1][bits == min(bits)]

        ## break ties by random choice
        if (length(id_trim) > 1) {
            rord <- order(runif(length(id_trim)))
            id_trim <- id_trim[rord][1]
        }

        avail[id(obj) == id_trim] <- FALSE
        id_rm <- find_unavailable(obj, avail)
        new_ped <- subset(obj, id_rm, keep = FALSE, del_parents = TRUE)
        new_size <- bit_size(new_ped)$bit_size
        avail <- avail[!(id(obj) %in% id_rm)]

        list(ped = new_ped, new_avail = avail, id_trimmed = id_trim,
            is_trimmed = TRUE, bit_size = new_size
        )
    }
)

#' @rdname find_avail_affected
#' @export
setMethod("find_avail_affected", "Pedigree",
    function(obj, avail = NULL, affected = NULL, affstatus = NA) {
        find_avail_affected(ped(obj), avail, affected, affstatus)
    }
)
