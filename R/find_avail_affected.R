# Automatically generated from all.nw using noweb

#' Find a single person to trim from a pedigree whose is available
#'
#' @description
#' Finds one subject from among available non-parents with indicated affection
#' status.
#'
#' @details
#' When used within pedigree.shrink, this function is called with the first
#' affected indicator, if the affected item in the pedigree is a matrix of
#' multiple affected indicators.
#'
#' @param ped A pedigree objects, with id (subject ID), findex (father index),
#' mindex (mother index)
#' @param avail Vector of availability status (e.g., genotyped) 0/1 or
#' TRUE/FALSE
#' @param affstatus Vector of affection status 0/1 or TRUE/FALSE.
#'
#' @return A list is returned with the following components
#' ## ped
#' Dataframe with trimmed subject removed
#' ## idTrimmed
#' Vector of IDs of trimmed individuals
#' ## isTrimmed
#' logical value indicating whether pedigree has been trimmed
#' ## bit_size
#' Bit size of the trimmed pedigree
#'
#' @seealso `pedigree.shrink`
#' @include bit_size.R
#' @include utils.R
#' @include find_unavailable.R
#' @export find_avail_affected
find_avail_affected <- function(ped, affstatus = NA) {
    ped_df <- ped$ped
    avail <- ped_df$avail
    not_parent <- !is_parent(ped_df$id, ped_df$dadid, ped_df$momid)

    if (is.na(affstatus)) {
        possibl_trim <- ped_df$id[not_parent & avail == 1 & is.na(ped_df$affected)]
    } else {
        possibl_trim <- ped_df$id[not_parent & avail == 1 & ped_df$affected == affstatus]
    }
    n_trim <- length(possibl_trim)

    if (n_trim == 0) {
        return(list(
            ped = ped, id_trimmed = NA, is_trimmed = FALSE,
            bit_size = bit_size(ped)$bit_size
        ))
    }

    trim_dat <- NULL

    for (id_trim in possibl_trim) {
        tmp_avail <- avail
        tmp_avail[ped_df$id == id_trim] <- FALSE
        id_rm <- find_unavailable(ped, tmp_avail)
        new_ped <- pedigree_trim(ped, id_rm)
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

    avail[ped_df$id == id_trim] <- FALSE
    id_rm <- find_unavailable(ped, avail)
    new_ped <- pedigree_trim(ped, id_rm)
    new_size <- bit_size(new_ped)$bit_size
    avail <- avail[!(ped_df$id %in% id_rm)]

    list(ped = new_ped, new_avail = avail, id_trimmed = id_trim,
        is_trimmed = TRUE, bit_size = new_size
    )
}
