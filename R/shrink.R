#' Shrink Pedigree object
#'
#' @description
#' Shrink Pedigree object to specified bit size with priority placed on
#' trimming uninformative subjects. The algorithm is useful for getting a
#' Pedigree condensed to a minimally informative size for algorithms or testing
#' that are limited by size of the Pedigree.
#'
#' If **avail** or **affected** are `NULL`, they are extracted with their
#' corresponding accessors from the Ped object.
#'
#' @details
#' Iteratively remove subjects from the Pedigree. The random removal of members
#' was previously controlled by a seed argument, but we remove this, forcing
#' users to control randomness outside the function. First remove uninformative
#' subjects, i.e., unavailable (not genotyped) with no available descendants.
#' Next, available terminal subjects with unknown phenotype if both parents
#' available. Last, iteratively shrinks Pedigrees by preferentially removing
#' individuals (chosen at random if there are multiple of the same status):
#'
#' 1. Subjects with unknown affected status
#' 2. Subjects with unaffected affected status
#' 3. Affected subjects.
#'
#' @param obj A Pedigree or Ped object.
#' @inheritParams Ped
#' @inheritParams is_informative
#' @param max_bits Optional, the bit size for which to shrink the Pedigree
#'
#' @return A list containing the following elements:
#' - pedObj: Pedigree object after trimming
#' - id_trim: Vector of ids trimmed from Pedigree
#' - id_lst: List of ids trimmed by category
#' - bit_size: Vector of bit sizes after each trimming step
#' - avail: Vector of availability status after trimming
#' - pedSizeOriginal: Number of subjects in original Pedigree
#' - pedSizeIntermed: Number of subjects after initial trimming
#' - pedSizeFinal: Number of subjects after final trimming
#'
#' @examples
#' data(sampleped)
#' ped1 <- Pedigree(sampleped[sampleped$famid == '1',])
#' shrink(ped1, max_bits = 12)
#'
#' @author Original by Dan Schaid,
#' updated by Jason Sinnwell and Louis Le Nézet
#' @keywords shrink
#' @seealso [Pedigree()], [bit_size()]
#' @export
#' @usage NULL
setGeneric("shrink", signature = "obj",
    function(obj, ...) standardGeneric("shrink")
)

#' @rdname shrink
#' @export
setMethod("shrink", "Pedigree",
    function(obj, avail = NULL, affected = NULL, max_bits = 16) {
        lst_trim <- shrink(ped(obj),
            avail = avail,
            affected = affected,
            max_bits = max_bits
        )
        all_ids <- id(lst_trim$pedObj)
        lst_trim$pedObj <- subset(
            obj, id(ped(obj)) %in% all_ids, del_parents = TRUE
        )
        lst_trim
    }
)

#' @rdname shrink
#' @export
setMethod("shrink", "Ped",
    function(obj, avail = NULL, affected = NULL, max_bits = 16) {
        if (is.null(avail)) {
            avail <- avail(obj)
        }
        if (is.null(affected)) {
            affected <- affected(obj)
        }
        if (any(is.na(avail))) {
            stop("NA values not allowed in avail vector.")
        }

        id_trim <- numeric()
        id_lst <- list()
        n_origin <- length(obj)

        bitsize_old <- bit_size(obj)$bit_size

        ## first find unavailable subjects to remove anyone who is not available
        ## and does not have an available descendant
        id_trim_unav <- find_unavailable(obj, avail)

        if (length(id_trim_unav)) {
            ped_trim <- subset(
                obj, id_trim_unav, keep = FALSE, del_parents = TRUE
            )
            avail <- avail[match(id(ped_trim), id(obj))]
            id_trim <- c(id_trim, id_trim_unav)
            id_lst$unavail <- id_trim_unav

        } else {
            ## no trimming, reset to original ped
            ped_trim <- obj
        }

        ## Next trim any available terminal subjects with unknown phenotype but
        ## only if both parents are available

        ## added n_new>0 check because no need to trim anymore if empty ped

        n_chg <- 1
        id_lst$noninform <- NULL
        n_new <- length(ped_trim)

        while (n_chg > 0 && n_new > 0) {
            n_old <- length(ped_trim)

            ## find_avail_noninform finds non-informative,
            ## but after suggesting their removal,
            ## checks for more unavailable subjects before returning
            id_trim_noninf <- find_avail_noninform(ped_trim, avail)

            if (length(id_trim_noninf)) {
                ped_new <- subset(ped_trim, id_trim_noninf, keep = FALSE)
                avail <- avail[match(id(ped_new), id(ped_trim))]
                id_trim <- c(id_trim, id_trim_noninf)
                id_lst$noninform <- c(id_lst$noninform, id_trim_noninf)
                ped_trim <- ped_new
            }
            n_new <- length(ped_trim)
            n_chg <- n_old - n_new
        }

        ## Determine number of subjects & bit_size after initial trimming
        n_inter <- length(ped_trim)

        bit_size <- bit_size(ped_trim)$bit_size

        ## Now sequentially shrink to fit bit_size <= max_bits

        bitsize_vec <- c(bitsize_old, bit_size)

        is_trim <- TRUE
        id_lst$affect <- NULL

        while (is_trim && (bit_size > max_bits)) {
            ## First, try trimming by unknown status
            save <- find_avail_affected(ped_trim, avail, affstatus = NA)
            is_trim <- save$is_trim

            ## Second, try trimming by unaffected status if no unknowns to trim
            if (!is_trim) {
                save <- find_avail_affected(ped_trim, avail, affstatus = 0)
                is_trim <- save$is_trim
            }

            ## Third, try trimming by affected status if no unknowns & no
            ## unaffecteds to trim
            if (!is_trim) {
                save <- find_avail_affected(ped_trim, avail, affstatus = 1)
                is_trim <- save$is_trim
            }

            if (is_trim) {
                ped_trim <- save$ped
                avail <- save$new_avail
                bit_size <- save$bit_size
                bitsize_vec <- c(bitsize_vec, bit_size)
                id_trim <- c(id_trim, save$id_trim)
                id_lst$affect <- c(id_lst$affect, save$id_trim)
            }
        }
        ## end while (is_trim) & (bit_size > max_bits)

        n_final <- length(ped_trim)

        obj <- list(
            pedObj = ped_trim, id_trim = id_trim, id_lst = id_lst,
            bit_size = bitsize_vec, avail = avail, pedSizeOriginal = n_origin,
            pedSizeIntermed = n_inter, pedSizeFinal = n_final
        )

        obj
    }
)
