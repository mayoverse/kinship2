#' Shrink pedigree object
#'
#' @description
#' Shrink pedigree object to specified bit size with priority placed on
#' trimming uninformative subjects. The algorithm is useful for getting a
#' pedigree condensed to a minimally informative size for algorithms or testing
#' that are limited by size of the pedigree.
#'
#' @details
#' Iteratively remove subjects from the pedigree. The random removal of members
#' was previously controlled by a seed argument, but we remove this, forcing
#' users to control randomness outside the function. First remove uninformative
#' subjects, i.e., unavailable (not genotyped) with no available descendants.
#' Next, available terminal subjects with unknown phenotype if both parents
#' available. Last, iteratively shrinks pedigrees by preferentially removing
#' individuals (chosen at random if there are multiple of the same status):
#' 1. Subjects with unknown affected status
#' 2. Subjects with unaffected affected status
#' 3. Affected subjects.
#'
#' @inheritParams align
#' @inheritParams is_informative
#' @param max_bits Optional, the bit size for which to shrink the pedigree
#'
#' @return A list containing the following elements:
#' - pedObj: Pedigree object after trimming
#' - id_trim: Vector of ids trimmed from pedigree
#' - id_lst: List of ids trimmed by category
#' - bit_size: Vector of bit sizes after each trimming step
#' - avail: Vector of availability status after trimming
#' - pedSizeOriginal: Number of subjects in original pedigree
#' - pedSizeIntermed: Number of subjects after initial trimming
#' - pedSizeFinal: Number of subjects after final trimming
#'
#' @examples
#' data(sampleped)
#' ped1 <- pedigree(sampleped[sampleped$family == '1',])
#' shrink(ped1, max_bits = 12)
#'
#' @author Original by Dan Schaid, updated to kinship2 by Jason Sinnwell
#' @seealso [pedigree()], [bit_size()]
#' @export
shrink <- function(
    ped, avail = ped$ped$avail, affected = ped$ped$affected, max_bits = 16
) {
    if (any(is.na(avail))) {
        stop("NA values not allowed in avail vector.")
    }

    id_trim <- numeric()
    id_lst <- list()
    n_origin <- length(ped$ped$id)

    bitsize_old <- bit_size(ped)$bit_size

    ## first find unavailable subjects to remove anyone who is not available
    ## and does not have an available descendant

    id_trim_unav <- find_unavailable(ped, avail)

    if (length(id_trim_unav)) {
        ped_trim <- trim(ped, id_trim_unav)
        avail <- avail[match(ped_trim$ped$id, ped$ped$id)]
        id_trim <- c(id_trim, id_trim_unav)
        id_lst$unavail <- id_trim_unav

    } else {
        ## no trimming, reset to original ped
        ped_trim <- ped
    }

    ## Next trim any available terminal subjects with unknown phenotype but
    ## only if both parents are available

    ## added n_new>0 check because no need to trim anymore if empty ped

    n_chg <- 1
    id_lst$noninform <- NULL
    n_new <- length(ped_trim$ped$id)

    while (n_chg > 0 && n_new > 0) {
        n_old <- length(ped_trim$ped$id)

        ## find_avail_noninform finds non-informative,
        ## but after suggesting their removal,
        ## checks for more unavailable subjects before returning
        id_trim_noninf <- find_avail_noninform(ped_trim, avail)

        if (length(id_trim_noninf)) {
            ped_new <- trim(ped_trim, id_trim_noninf)
            avail <- avail[match(ped_new$ped$id, ped_trim$ped$id)]
            id_trim <- c(id_trim, id_trim_noninf)
            id_lst$noninform <- c(id_lst$noninform, id_trim_noninf)
            ped_trim <- ped_new
        }
        n_new <- length(ped_trim$ped$id)
        n_chg <- n_old - n_new
    }

    ## Determine number of subjects & bit_size after initial trimming
    n_inter <- length(ped_trim$ped$id)

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

    n_final <- length(ped_trim$ped$id)

    obj <- list(pedObj = ped_trim, id_trim = id_trim, id_lst = id_lst,
        bit_size = bitsize_vec, avail = avail, pedSizeOriginal = n_origin,
        pedSizeIntermed = n_inter, pedSizeFinal = n_final
    )

    obj
}

TRUE