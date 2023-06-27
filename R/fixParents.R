#' Fix details on the parents for children of the pedigree
#'
#' @description
#' Fix the sex of parents, add parents that are missing from the pedigree
#'
#' @details
#' First look to add parents whose ids are given in momid/dadid. Second, fix
#' sex of parents. Last look to add second parent for children for whom only
#' one parent id is given.
#'
#' @param id Identification variable for individual
#' @param dadid Identification variable for father. Founders parents should be
#' coded to NA, or another value specified by missid.
#' @param momid Identification variable for mother. Founders parents should be
#' coded to NA, or another value specified by missid.
#' @param sex Gender of individual noted in `id`. Either character
#' ("male","female","unknown","terminated") or numeric (1="male", 2="female",
#' 3="unknown", 4="terminated") data is allowed.  For character data the string
#' may be truncated, and of arbitrary case.
#' @param missid The founders are those with no father or mother in the
#' pedigree.  The \code{dadid} and \code{momid} values for these subjects will
#' either be NA or the value of this variable.  The default for \code{missid}
#' is 0 if the \code{id} variable is numeric, and "" (the empty string)
#' otherwise.
#'
#' @return A data.frame with id, dadid, momid, sex as columns
#'
#' @examples
#'
#' test1char <- data.frame(
#'   id = paste("fam", 101:111, sep = ""),
#'   sex = c("male", "female")[c(1, 2, 1, 2, 1, 1, 2, 2, 1, 2, 1)],
#'   father = c(
#'     0, 0, "fam101", "fam101", "fam101", 0, 0,
#'     "fam106", "fam106", "fam106", "fam109"
#'   ),
#'   mother = c(
#'     0, 0, "fam102", "fam102", "fam102", 0, 0,
#'     "fam107", "fam107", "fam107", "fam112"
#'   )
#' )
#' test1newmom <- with(test1char, fixParents(id, father, mother,
#'   sex,
#'   missid = "0"
#' ))
#' newped <- with(test1newmom, pedigree(id, dadid, momid, sex, missid = "0"))
#' as.data.frame(newped)
#'
#' @author Jason Sinnwell
#' @seealso \code{\link{pedigree}}
#' @export fixParents
fix_parents <- function(id, dadid, momid, sex, missid = 0, ...) {
  ## fix sex of parents
  ## add parents that are missing
  n <- length(id)
  if (length(momid) != n) {
    stop("Mismatched lengths, id and momid")
  }
  if (length(dadid) != n) {
    stop("Mismatched lengths, id and momid")
  }
  if (length(sex) != n) {
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
    "Setting 0=male, 1=female, 2=unknown, 3=terminated. \n"))
    sex <- sex + 1
  }
  sex <- ifelse(sex < 1 | sex > 4, 3, sex)
  if (all(sex > 2)) {
    stop("Invalid values for 'sex'")
  } else if (mean(sex == 3) > 0.25) {
    warning("More than 25% of the gender values are 'unknown'")
  }
  ## #  sex <- factor(sex, 1:4, labels = codes)
  if (missing(missid)) {
    if (is.numeric(id)) {
      missid <- 0
    } else {
      missid <- ""
    }
  }
  if (any(is.na(id))) {
    stop("Missing value for the id variable")
  }
  if (!is.numeric(id)) {
    id <- as.character(id)
    addids <- paste("addin", seq_along(id), sep = "-")
    dadid <- as.character(dadid)
    momid <- as.character(momid)
    missid <- as.character(missid)
    if (length(grep("^ *$", id)) > 0) {
      stop("A blank or empty string is not allowed as the id variable")
    }
  } else {
    addids <- seq(max(id, na.rm = TRUE) + 1, max(id, na.rm = TRUE) +
      length(id))
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
  return(data.frame(
    id = id, momid = momid, dadid = dadid,
    sex = sex
  ))
}

#' Fix missing parents
#'
#' @description Apply fixParents on a dataframe or delete missing parents.
#'
#' @details Check for presence of both parents id in the `id` field.
#' If not both presence behaviour depend of `delete` parameter
#' If TRUE then use fixParents function and merge back the other fields
#' in the dataframe then set availability to O for non available parents.
#' If FALSE then delete the id of missing parents
#'
#' @param df Dataframe to process
#' @param delete Boolean defining if missing parents needs to be:
#' TRUE: added as a new row
#' FALSE: be deleted
#' @param missid The founders are those with no father or mother in the
#' pedigree.  The \code{dadid} and \code{momid} values for these subjects will
#' either be NA or the value of this variable.  The default for \code{missid}
#' is 0 if the \code{id} variable is numeric, and "" (the empty string)
#' otherwise.
#'
#' @return The same dataframe with the parents ids fixed
#'
#' @examples
#'
#' @export fix_parents_df
fix_parents.data.frame <- function(df = df, delete = FALSE, missid = "0",
  id = "id", avail = "avail",
  dadid = "dadid", momid = "momid", sex = "sex", ...) {
  cols_needed <- c(id, dadid, momid, sex, avail)
  df <- check_columns(df, cols_needed, "", "", others_cols = TRUE)

  message("Fixing incomplete couple")
  all_id <- c(df[[id]], df[[dadid]], df[[momid]])
  all_id <- unique(all_id[all_id != missid])
  message(paste(length(all_id), "individuals detected"))

  if (nrow(df) > 2) {
    if (delete) {
      # One of the parents doesn't not have a line in id
      dad_present <- match(df[[dadid]], df[[id]], nomatch = missid)
      mom_present <- match(df[[momid]], df[[id]], nomatch = missid)
      df[dad_present == missid | mom_present == missid,
          c(momid, dadid)] <- missid

      all_id_new <- c(df[[id]], df[[dadid]], df[[momid]])
      all_id_new <- unique(all_id_new[all_id_new != missid])
      all_id_dif <- all_id[!all_id %in% all_id_new]
      message(paste(length(all_id_dif), "individuals deleted"))
    }
    df_fix <- fix_parents(df[[id]], df[[dadid]], df[[momid]],
      df[[sex]], missid = missid, ...)
    col_used <- which(
      names(df) == momid |
      names(df) == dadid |
      names(df) == sex)
    df <- merge(
      df[, -col_used],
      df_fix, by = id,
      all.y = TRUE, all.x = TRUE)
    df[[avail]][is.na(df[[avail]])] <- 0
    all_id_new <- c(df[[id]], df[[dadid]], df[[momid]])
    all_id_new <- unique(all_id_new[all_id_new != missid])
    all_id_dif <- all_id_new[!all_id_new %in% all_id]
    message(paste(length(all_id_dif), "individuals added"))
  }
  message(paste("Final:", nrow(df), "individuals detected"))
  return(df)
}
