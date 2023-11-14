#' @importFrom stringr str_split_i
NULL

#' Fix parents relationship and gender
#'
#' @description
#' Fix the sex of parents, add parents that are missing from the data.
#' Can be used with a dataframe or a vector of the
#' different individuals informations.
#'
#' @details
#' First look to add parents whose ids are given in momid/dadid. Second, fix
#' sex of parents. Last look to add second parent for children for whom only
#' one parent id is given.
#' If a **famid** vector is given the family id will be added to the ids of all
#' individuals (`id`, `dadid`, `momid`)
#' separated by an underscore before proceeding.
#'
#' ## Special case for dataframe
#' Check for presence of both parents id in the **id** field.
#' If not both presence behaviour depend of **delete** parameter
#' - If `TRUE` then use fix_parents function and merge back the other fields
#' in the dataframe then set availability to O for non available parents.
#' - If `FALSE` then delete the id of missing parents
#'
#' @inheritParams Ped
#' @param obj A data.frame or a vector of the individuals identifiers. If a
#' dataframe is given it must contain the columns `id`, `dadid`,
#' `momid`, `sex` and `famid` (optional).
#'
#' @return A data.frame with id, dadid, momid, sex as columns with the
#' relationships fixed.
#'
#' @examples
#'
#' test1char <- data.frame(
#'   id = paste('fam', 101:111, sep = ''),
#'   sex = c('male', 'female')[c(1, 2, 1, 2, 1, 1, 2, 2, 1, 2, 1)],
#'   father = c(
#'     0, 0, 'fam101', 'fam101', 'fam101', 0, 0,
#'     'fam106', 'fam106', 'fam106', 'fam109'
#'   ),
#'   mother = c(
#'     0, 0, 'fam102', 'fam102', 'fam102', 0, 0,
#'     'fam107', 'fam107', 'fam107', 'fam112'
#'   )
#' )
#' test1newmom <- with(test1char, fix_parents(id, father, mother,
#'   sex,
#'   missid = NA_character_
#' ))
#' Pedigree(test1newmom)
#'
#' @author Jason Sinnwell
#' @export
#' @usage NULL
setGeneric("fix_parents", signature = "obj",
    function(obj, ...) standardGeneric("fix_parents")
)

#' @rdname fix_parents
#' @export
setMethod("fix_parents", "character", function(
    obj, dadid, momid, sex, famid = NULL, missid = NA_character_
) {
    ## fix sex of parents add parents that are missing
    n <- length(obj)
    id <- obj
    if (length(momid) != n) {
        stop("Mismatched lengths, id and momid")
    }
    if (length(dadid) != n) {
        stop("Mismatched lengths, id and momid")
    }
    if (length(sex) != n) {
        stop("Mismatched lengths, id and sex")
    }

    if (length(famid) != n & length(famid) > 0) {
        stop("Mismatched lengths, id and sex")
    }

    sex <- as.numeric(sex_to_factor(sex))
    if (min(sex) == 0) {
        warning("Sex values contain 0, but expected codes 1-4.\n",
            "Setting 0=male, 1=female, 2=unknown, 3=terminated. \n"
        )
        sex <- sex + 1
    }
    sex <- ifelse(sex < 1 | sex > 4, 3, sex)
    if (all(sex > 2)) {
        stop("Invalid values for 'sex'")
    } else if (mean(sex == 3) > 0.25) {
        warning("More than 25% of the gender values are 'unknown'")
    }
    if (any(is.na(id) | id %in% missid)) {
        stop("Missing value for the id variable")
    }

    if (!is.null(famid)) {
        id <- upd_famid_id(id, famid, missid)
        dadid <- upd_famid_id(dadid, famid, missid)
        momid <- upd_famid_id(momid, famid, missid)
    }

    addids <- paste("addin", seq_along(id), sep = "-")
    if (length(grep("^ *$", id)) > 0) {
        stop("A blank or empty string is not allowed as the id variable")
    }

    nofather <- (is.na(dadid) | dadid %in% missid)
    nomother <- (is.na(momid) | momid %in% missid)
    if (any(duplicated(id))) {
        duplist <- id[duplicated(id)]
        msg_nb <- min(length(duplist), 6)
        stop("Duplicate subject id:", duplist[seq_len(msg_nb)])
    }
    findex <- match(dadid, id, nomatch = 0)
    mindex <- match(momid, id, nomatch = 0)

    ## dadid given, not found id, so add
    if (any(findex == 0 & !nofather)) {
        dadnotfound <- unique(dadid[which(findex == 0 & !nofather)])
        id <- c(id, dadnotfound)
        sex <- c(sex, rep(1, length(dadnotfound)))
        dadid <- c(dadid, rep(missid, length(dadnotfound)))
        momid <- c(momid, rep(missid, length(dadnotfound)))
    }
    if (any(mindex == 0 & !nomother)) {
        momnotfound <- unique(momid[which(mindex == 0 & !nomother)])
        id <- c(id, momnotfound)
        sex <- c(sex, rep(2, length(momnotfound)))
        dadid <- c(dadid, rep(missid, length(momnotfound)))
        momid <- c(momid, rep(missid, length(momnotfound)))
    }
    if (any(sex[mindex] != 1)) {
        dadnotmale <- unique((id[findex])[sex[findex] != 1])
        sex[id %in% dadnotmale] <- 1
    }
    if (any(sex[mindex] != 2)) {
        momnotfemale <- unique((id[mindex])[sex[mindex] != 2])
        sex[id %in% momnotfemale] <- 2
    }
    findex <- match(dadid, id, nomatch = 0)
    mindex <- match(momid, id, nomatch = 0)
    addids <- addids[!(addids %in% id)]

    ## children with mom in Pedigree, dad missing
    if (any(findex == 0 & mindex != 0)) {
        nodad_idx <- which(findex == 0 & mindex != 0)
        dadid[nodad_idx] <- addids[seq_along(nodad_idx)]
        id <- c(id, addids[seq_along(nodad_idx)])
        sex <- c(sex, rep(1, length(nodad_idx)))
        dadid <- c(dadid, rep(missid, length(nodad_idx)))
        momid <- c(momid, rep(missid, length(nodad_idx)))
    }
    ## children with dad in ped, mom missing
    addids <- addids[!(addids %in% id)]
    if (any(mindex == 0 & findex != 0)) {
        nodad_idx <- which(mindex == 0 & findex != 0)
        momid[nodad_idx] <- addids[seq_along(nodad_idx)]
        id <- c(id, addids[seq_along(nodad_idx)])
        sex <- c(sex, rep(2, length(nodad_idx)))
        dadid <- c(dadid, rep(missid, length(nodad_idx)))
        momid <- c(momid, rep(missid, length(nodad_idx)))
    }
    if (is.null(famid)) {
        data.frame(id = id, momid = momid, dadid = dadid, sex = sex)
    } else {
        famid <- make_famid(id, dadid, momid)
        data.frame(
            id = id, momid = momid, dadid = dadid,
            sex = sex, famid = famid
        )
    }
})

#' @param delete Boolean defining if missing parents needs to be:
#' - `TRUE` : added as a new row
#' - `FALSE` : be deleted
#' @param filter Filtering column containing `0` or `1` for the
#' rows to kept before proceeding.
#' @rdname fix_parents
#' @export
setMethod("fix_parents", "data.frame", function(
    obj, delete = FALSE, filter = NULL, missid = NA_character_
) {
    cols_needed <- c("id", "dadid", "momid", "sex", filter)
    df <- check_columns(obj, cols_needed, NULL, "famid", others_cols = TRUE,
        cols_to_use_init = TRUE
    )
    df_old <- df
    if (!is.null(filter)) {
        if (is.logical(df[[filter]])) {
            df <- df[df[[filter]], ]
        } else {
            stop("Filtering column must me only TRUE or FALSE")
        }
    }
    if (nrow(df) > 2) {
        if (delete) {
            # One of the parents doesn't not have a line in id
            dad_present <- match(df$dadid, df$id, nomatch = missid)
            mom_present <- match(df$momid, df$id, nomatch = missid)
            df[dad_present %in% missid |
                    mom_present %in% missid, c("momid", "dadid")
            ] <- missid
        }
        df_fix <- fix_parents(
            df$id, df$dadid, df$momid,
            df$sex, missid = missid, famid = df$famid
        )
        col_used <- which(names(df_old) == "momid" |
                names(df_old) == "dadid" |
                names(df_old) == "sex" |
                names(df_old) == "famid"
        )
        df <- merge(df_old[, -col_used], df_fix, by = "id",
            all.y = TRUE, all.x = FALSE
        )
    }
    df
})
