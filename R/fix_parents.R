#' @importFrom stringr str_split_i
NULL

#' Fix details on the parents for children of the pedigree
#'
#' @description
#' Fix the sex of parents, add parents that are missing from the pedigree
#' Can be used with a dataframe, a pedigree object or a vector of the
#' different individuals identifiers.
#'
#' @details
#' First look to add parents whose ids are given in momid/dadid. Second, fix
#' sex of parents. Last look to add second parent for children for whom only
#' one parent id is given.
#' If a family vector is given the family id will be added to the ids of all
#' individuals (id, dadid, momid) separated by an underscore befor proceeding.
#'
#' @param obj A pedigree object, a dataframe or a vector of the individuals
#' identifiers
#' @param id Individual id column
#' @param dadid Father id column or a vector of the father identifiers
#' @param momid Mother id column or a vector of the mother identifiers
#' @param sex Gedner column or a vector of the sex of the individuals. Either
#' character ('male','female','unknown','terminated') or
#' numeric (1='male', 2='female',#' 3='unknown', 4='terminated')
#' data is allowed.
#' @param family Optional family column, set it to NULL to invalidate.
#' @param missid The founders are those with no father or mother in the
#' pedigree.  The \\code{dadid} and \\code{momid} values for these subjects will
#' either be NA or the value of this variable.  The default for \\code{missid}
#' is 0 if the \\code{id} variable is numeric, and '' (the empty string)
#' otherwise.
#' @param family Optional family identification set it to NULL to invalidate.
#' If used it will modify the ids of the individuals by pasting it with an _.
#' @param ... Additional arguments to be passed to methods.
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
#'   missid = '0'
#' ))
#' newped <- pedigree(test1newmom)
#' as.data.frame(newped)
#'
#' @author Jason Sinnwell
#' @seealso \\code{\\link{pedigree}}
#' @export
setGeneric("fix_parents", signature = "obj",
    function(obj, ...) standardGeneric("fix_parents")
)

#' @export
#' @rdname fix_parents
#' @aliases fix_parents,character
setMethod("fix_parents", "character", function(
        obj, dadid, momid, sex, family = NULL, missid = "0") {
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
    if (length(family) != n & length(family) > 0) {
        stop("Mismatched lengths, id and sex")
    }
    if (is.factor(sex)) {
        sex <- as.character(sex)
    }
    codes <- c("male", "female", "unknown", "terminated")
    if (is.character(sex)) {
        sex <- charmatch(casefold(sex, upper = FALSE), codes, nomatch = 3)
    }
    sex <- as.integer(sex)
    if (min(sex) == 0) {
        warning(paste0("Sex values contain 0, but expected codes 1-4.\n",
            "Setting 0=male, 1=female, 2=unknown, 3=terminated. \n"
        ))
        sex <- sex + 1
    }
    sex <- ifelse(sex < 1 | sex > 4, 3, sex)
    if (all(sex > 2)) {
        stop("Invalid values for 'sex'")
    } else if (mean(sex == 3) > 0.25) {
        warning("More than 25% of the gender values are 'unknown'")
    }
    if (any(is.na(id) | id == missid)) {
        stop("Missing value for the id variable")
    }

    id <- prefix_famid(family, id, missid)
    dadid <- prefix_famid(family, dadid, missid)
    momid <- prefix_famid(family, momid, missid)
    addids <- paste("addin", seq_along(id), sep = "-")
    if (length(grep("^ *$", id)) > 0) {
        stop("A blank or empty string is not allowed as the id variable")
    }

    nofather <- (is.na(dadid) | dadid == missid)
    nomother <- (is.na(momid) | momid == missid)
    if (any(duplicated(id))) {
        duplist <- id[duplicated(id)]
        msg_nb <- min(length(duplist), 6)
        stop(paste("Duplicate subject id:", duplist[1:msg_nb]))
    }
    findex <- match(dadid, id, nomatch = 0)
    mindex <- match(momid, id, nomatch = 0)
    ## dadid given, not found id, so add
    if (any(findex == 0 & !nofather)) {
        dadnotfound <- unique(dadid[which(findex == 0 & !nofather)])
        id <- c(id, dadnotfound)
        sex <- c(sex, rep(1, length(dadnotfound)))
        dadid <- c(dadid, rep(0, length(dadnotfound)))
        momid <- c(momid, rep(0, length(dadnotfound)))
    }
    if (any(mindex == 0 & !nomother)) {
        momnotfound <- unique(momid[which(mindex == 0 & !nomother)])
        id <- c(id, momnotfound)
        sex <- c(sex, rep(2, length(momnotfound)))
        dadid <- c(dadid, rep(0, length(momnotfound)))
        momid <- c(momid, rep(0, length(momnotfound)))
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

    ## children with mom in pedigree, dad missing
    if (any(findex == 0 & mindex != 0)) {
        nodad_idx <- which(findex == 0 & mindex != 0)
        dadid[nodad_idx] <- addids[seq_along(nodad_idx)]
        id <- c(id, addids[seq_along(nodad_idx)])
        sex <- c(sex, rep(1, length(nodad_idx)))
        dadid <- c(dadid, rep(0, length(nodad_idx)))
        momid <- c(momid, rep(0, length(nodad_idx)))
    }
    ## children with dad in ped, mom missing
    addids <- addids[!(addids %in% id)]
    if (any(mindex == 0 & findex != 0)) {
        nodad_idx <- which(mindex == 0 & findex != 0)
        momid[nodad_idx] <- addids[seq_along(nodad_idx)]
        id <- c(id, addids[seq_along(nodad_idx)])
        sex <- c(sex, rep(2, length(nodad_idx)))
        dadid <- c(dadid, rep(0, length(nodad_idx)))
        momid <- c(momid, rep(0, length(nodad_idx)))
    }
    if (is.null(family)) {
        data.frame(id = id, momid = momid, dadid = dadid, sex = sex)
    } else {
        family <- stringr::str_split_i(id, "_", i = 1)
        data.frame(
            id = id, momid = momid, dadid = dadid,
            sex = sex, family = family
        )
    }
})

#' Fix missing parents
#'
#' @description Apply fix_parents on a dataframe or delete missing parents.
#'
#' @details Check for presence of both parents id in the `id` field.
#' If not both presence behaviour depend of `delete` parameter
#' If TRUE then use fix_parents function and merge back the other fields
#' in the dataframe then set availability to O for non available parents.
#' If FALSE then delete the id of missing parents
#'
#' @param delete Boolean defining if missing parents needs to be:
#' TRUE: added as a new row
#' FALSE: be deleted
#' @param filter Filtering column containing 0 or 1 for the individual to kept
#'
#' @return The same dataframe with the parents ids fixed
#'
#' @export
#' @rdname fix_parents
setMethod("fix_parents", "data.frame", function(
        obj, delete = FALSE, filter = NULL, missid = "0",
        id = "id", dadid = "dadid", momid = "momid", sex = "sex",
        family = "family") {
    cols_needed <- c(id, dadid, momid, sex, filter, family)
    df <- check_columns(obj, cols_needed, "", "", others_cols = TRUE)
    df_old <- df
    if (!is.null(filter)) {
        if (is.logical(df[[filter]])) {
            df <- df[df[[filter]], ]
        } else {
            stop("Error, filtering column must me only TRUE or FALSE")
        }
    }
    all_id <- c(df[[id]], df[[dadid]], df[[momid]])
    all_id <- unique(all_id[all_id != missid])

    if (nrow(df) > 2) {
        if (delete) {
            # One of the parents doesn't not have a line in id
            dad_present <- match(df[[dadid]], df[[id]], nomatch = missid)
            mom_present <- match(df[[momid]], df[[id]], nomatch = missid)
            df[dad_present == missid |
                    mom_present == missid, c(momid, dadid)
            ] <- missid

            all_id_new <- c(df[[id]], df[[dadid]], df[[momid]])
            all_id_new <- unique(all_id_new[all_id_new != missid])
            all_id_dif <- all_id[!all_id %in% all_id_new]
        }
        df_fix <- fix_parents(
            df[[id]], df[[dadid]], df[[momid]],
            df[[sex]], missid = missid, family = df[[family]]
        )
        col_used <- which(names(df_old) == momid | names(df_old) == dadid |
                names(df_old) == sex | names(df_old) == family
        )
        df <- merge(df_old[, -col_used], df_fix, by = id,
            all.y = TRUE, all.x = FALSE
        )
        all_id_new <- c(df[[id]], df[[dadid]], df[[momid]])
        all_id_new <- unique(all_id_new[all_id_new != missid])
        all_id_dif <- all_id_new[!all_id_new %in% all_id]
    }
    df
})
