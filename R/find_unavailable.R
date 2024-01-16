#' Find unavailable subjects in a Pedigree
#'
#' @description
#' Find the identifiers of subjects in a Pedigree iteratively,
#' as anyone who is not available and does not have an available
#' descendant by successively removing unavailable terminal nodes.
#'
#' @details
#' If **avail** is null, then the function will use the
#' corresponding Ped accessor.
#'
#' Originally written as pedTrim by Steve Iturria, modified by Dan Schaid 2007,
#' and now split into the two separate functions:
#' `find_unavailable()`, and `trim()` to do the tasks separately.
#' `find_unavailable()` calls
#' [exclude_stray_marryin()]
#' to find stray available marry-ins who are
#' isolated after trimming their unavailable offspring, and
#' [exclude_unavail_founders()].
#' If the subject ids are character, make sure none of the characters in the
#' ids is a colon (":"), which is a special character
#' used to concatenate and split subjects within the utility.
#' The `trim()` functions is now replaced by the `subset()` function.
#'
#' @inheritParams find_avail_affected
#'
#' @return Returns a vector of subject ids for who can be
#' removed.
#'
#' @section Side Effects:
#' Relation matrix from subsetting is trimmed of any
#' special relations that include the subjects to trim.
#'
#' @examples
#' data(sampleped)
#' ped1 <- Pedigree(sampleped[sampleped$famid == "1",])
#' find_unavailable(ped1)
#'
#' @seealso [shrink()]
#' @keywords internal, shrink
#' @include utils.R
#' @export
#' @usage NULL
setGeneric("find_unavailable", signature = "obj",
    function(obj, ...) standardGeneric("find_unavailable")
)

#' @rdname find_unavailable
#' @export
setMethod("find_unavailable", "Ped",
    function(obj, avail = NULL) {
        if (is.null(avail)) {
            avail <- avail(obj)
        }
        ## find id within Pedigree anyone who is not available and
        ## does not have an available descendant

        ## avail = TRUE/1 if available, FALSE/0 if not

        ## will do this iteratively by successively removing unavailable
        ## terminal nodes
        ## Steve Iturria, PhD, modified by Dan Schaid

        cont <- TRUE # flag for whether to keep iterating

        is_terminal <- (is_parent(obj) == FALSE)
        ## JPS 3/10/14 add strings check in case of char ids
        obji <- obj
        avail(obji) <- avail
        while (cont) {
            id_to_remove <- id(obji)[is_terminal & avail(obji) == 0]
            if (length(id_to_remove) > 0) {
                obji <- subset(obji, id_to_remove, keep = FALSE)
                is_terminal <- (is_parent(obji) == FALSE)
            } else {
                cont <- FALSE
            }
        }
        ## A few more clean up steps

        ## remove unavailable founders
        tmp_ped <- exclude_unavail_founders(
            id(obji), dadid(obji), momid(obji), avail(obji)
        )

        ## remove stray marry-ins
        tmp_ped <- exclude_stray_marryin(
            tmp_ped$id, tmp_ped$dadid, tmp_ped$momid
        )

        id(obj)[is.na(match(id(obj), tmp_ped$id))]
    }
)

#' @rdname find_unavailable
#' @export
setMethod("find_unavailable", "Pedigree",
    function(obj, avail = NULL) {
        find_unavailable(ped(obj), avail)
    }
)

#' Exclude stray marry-ins
#'
#' @description
#' Exclude any founders who are not parents.
#'
#' @inheritParams exclude_unavail_founders
#'
#' @return
#' Returns a data frame of subject identifiers and their parents.
#' The data frame is trimmed of any founders who are not parents.
#'
#' @keywords internal, shrink
#' @seealso [shrink()]
exclude_stray_marryin <- function(id, dadid, momid) {
    # get rid of founders who are not parents (stray available marryins
    # who are isolated after trimming their unavailable offspring)
    ## JPS 3/10/14 add strings check in case of char ids
    trio <- data.frame(id = id, dadid = dadid, momid = momid,
        stringsAsFactors = FALSE
    )
    parent <- is_parent(id, dadid, momid)
    founder <- is_founder(dadid, momid)

    exclude <- !parent & founder
    trio <- trio[!exclude, , drop = FALSE]
    return(trio)
}

#' Exclude unavailable founders
#'
#' @description
#' Exclude any unavailable founders.
#'
#' @param id A character vector with the identifiers of each individuals
#' @inheritParams Ped
#'
#' @return
#' Returns a list with the following components:
#' - n_trimmed Number of trimmed individuals
#' - id_trimmed Vector of IDs of trimmed individuals
#' - id Vector of subject identifiers
#' - dadid Vector of father identifiers
#' - momid Vector of mother identifiers
#'
#' @keywords internal, shrink
#' @seealso [shrink()]
exclude_unavail_founders <- function(
    id, dadid, momid, avail, missid = NA_character_
) {
    n_old <- length(id)

    ## zed = TRUE if both parents are present
    zed <- (!dadid %in% missid) & (!momid %in% missid)
    ## concat ids to represent marriages.
    ## Bug if there is ":" in char subj ids
    marriage <- paste(dadid[zed], momid[zed], sep = ":")
    sibship <- tapply(marriage, marriage, length)
    nm <- names(sibship)

    split_pos <- regexpr(":", nm)
    dad <- substring(nm, 1, split_pos - 1)
    mom <- substring(nm, split_pos + 1, nchar(nm))

    ##  Look for parents with > 1 marriage.  If any
    ##  marriage has > 1 child then skip this mom/dad pair.
    nmarr_dad <- table(dad)
    nmarr_mom <- table(mom)
    skip <- NULL

    if (any(nmarr_dad > 1)) {
        ## Dads in > 1 marriage
        ckdad <- which(as.logical(match(
            dad,
            names(nmarr_dad)[which(nmarr_dad > 1)],
            nomatch = FALSE
        )))
        skip <- unique(c(skip, ckdad))
    }

    if (any(nmarr_mom > 1)) {
        ## Moms in > 1 marriage
        ckmom <- which(as.logical(match(
            mom,
            names(nmarr_mom)[which(nmarr_mom > 1)],
            nomatch = FALSE
        )))
        skip <- unique(c(skip, ckmom))
    }

    if (length(skip) > 0) {
        dad <- dad[-skip]
        mom <- mom[-skip]
        zed <- (sibship[-skip] == 1)
    } else {
        zed <- (sibship == 1)
    }

    ##  Want to look at parents with only one child.
    n <- sum(zed)
    id_trimmed <- NULL
    if (n > 0) {
        # dad and mom are the parents of sibships of size 1
        dad <- dad[zed]
        mom <- mom[zed]
        for (i in seq_len(n)) {
            ## check if mom and dad are founders (where their parents = 0)
            dad_f <- (dadid[id == dad[i]] %in% missid) &
                (momid[id == dad[i]] %in% missid)
            mom_f <- (dadid[id == mom[i]] %in% missid) &
                (momid[id == mom[i]] %in% missid)
            both_f <- dad_f & mom_f

            ## check if mom and dad have avail
            dad_avail <- avail[id == dad[i]]
            mom_avail <- avail[id == mom[i]]

            ## define both_unavail = T if both mom & dad not avail
            both_unavail <- (dad_avail == 0 & mom_avail == 0)

            if (both_f && both_unavail) {
                ## remove mom and dad from ped, and zero-out parent
                ## ids of their child

                child <- which(dadid == which(id == dad[i]))
                dadid[child] <- 0
                momid[child] <- 0

                id_trimmed <- c(id_trimmed, dad[i], mom[i])

                exclude <- (id != dad[i]) & (id != mom[i])
                id <- id[exclude]
                dadid <- dadid[exclude]
                momid <- momid[exclude]

                avail <- avail[exclude]
            }
        }
    }

    n_new <- length(id)
    n_trimmed <- n_old - n_new

    return(list(
        n_trimmed = n_trimmed, id_trimmed = id_trimmed,
        id = id, dadid = dadid, momid = momid
    ))
}
