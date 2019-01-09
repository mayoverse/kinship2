## Author: Jason Sinnwell
## Date: 5/23/2017
## Updated: 1/9/2019

## id, dadid, momid, sex, missid all as would be passed to pedigree within a single family
## returned data.frame to be used with(df, pedigree(id, dadid, momid, sex))

#'Fix details on the parents for children of the pedigree
#'
#'Fix the sex of parents, add parents that are missing from the pedigree
#'
#'@param id Identification variable for individual
#'
#'@param dadid Identification variable for father. Founders parents should be coded 
#'    to NA, or another value specified by missid.
#'
#'@param momid Identification variable for mother. Founders parents should be coded
#'    to NA, or another value specified by missid.
#'
#'@param sex Gender of individual noted in `id`. Either character ("male","female","unknown","terminated")
#'    or numeric (1="male", 2="female", 3="unknown", 4="terminated")
#'    data is allowed.  For character data the string may be truncated,
#'    and of arbitrary case.
#'
#'@param missid The founders are those with no father or mother in the pedigree.  The
#'    \code{dadid} and \code{momid} values for these subjects will either be
#'    NA or the value of this variable.  The default for \code{missid} is 0
#'    if the \code{id} variable is numeric, and "" (the empty string)
#'    otherwise.
#'
#' First look to add parents whose ids are given in momid/dadid. Second,
#'    fix sex of parents. Last look to add second parent for children for whom
#'    only one parent id is given.
#'
#'@return A data.frame with id, dadid, momid, sex as columns
#'
#'@examples
#'test1char <- data.frame(id=paste("fam", 101:111, sep=""),
#'                        sex=c("male","female")[c(1,2,1,2,1, 1,2, 2,1,2, 1)],
#'                        father=c(0,0,"fam101","fam101","fam101", 0,0,"fam106","fam106","fam106", "fam109"),
#'                        mother=c(0,0,"fam102","fam102","fam102", 0,0,"fam107","fam107","fam107", "fam112"))
#'test1newmom <- with(test1char, fixParents(id, father, mother, sex, missid="0"))
#'newped <- with(test1newmom, pedigree(id, dadid, momid, sex, missid="0"))
#'as.data.frame(newped)


fixParents <- function (id, dadid, momid, sex, missid = 0)  {
  ## fix sex of parents
  ## add parents that are missing
  n <- length(id)
  if (length(momid) != n) 
    stop("Mismatched lengths, id and momid")
  if (length(dadid) != n) 
    stop("Mismatched lengths, id and momid")
  if (length(sex) != n) 
    stop("Mismatched lengths, id and sex")
  if (is.factor(sex)) 
    sex <- as.character(sex)
  codes <- c("male", "female", "unknown", "terminated")
  if (is.character(sex)) {
    sex <- charmatch(casefold(sex, upper = FALSE), codes, nomatch = 3)
  }
  sex <- as.integer(sex)
  if (min(sex) == 0) {
    warning("Sex values contain 0, but expected codes 1-4.\n Setting 0=male, 1=female, 2=unknown, 3=terminated. \n")
    sex <- sex + 1
  }
  sex <- ifelse(sex < 1 | sex > 4, 3, sex)
  if (all(sex > 2)) 
    stop("Invalid values for 'sex'")
  else if (mean(sex == 3) > 0.25) 
    warning("More than 25% of the gender values are 'unknown'")
  ## #  sex <- factor(sex, 1:4, labels = codes)
  if (missing(missid)) {
    if (is.numeric(id)) 
      missid <- 0
    else missid <- ""
  }
  if (any(is.na(id))) 
    stop("Missing value for the id variable")
  if (!is.numeric(id)) {
    id <- as.character(id)
    addids <- paste("addin", 1:length(id), sep = "-")
    dadid <- as.character(dadid)
    momid <- as.character(momid)
    missid <- as.character(missid)
    if (length(grep("^ *$", id)) > 0) 
      stop("A blank or empty string is not allowed as the id variable")
  }
  else {
    addids <- seq(max(id, na.rm = TRUE) + 1, max(id, na.rm = TRUE) + 
                  length(id))
  }
  
  nofather <- (is.na(dadid) | dadid == missid)
  nomother <- (is.na(momid) | momid == missid)
  if (any(duplicated(id))) {
    duplist <- id[duplicated(id)]
    msg.n <- min(length(duplist), 6)
    stop(paste("Duplicate subject id:", duplist[1:msg.n]))
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
    nodad.idx <- which(findex == 0 & mindex != 0)
    dadid[nodad.idx] <- addids[1:length(nodad.idx)]
    id <- c(id, addids[1:length(nodad.idx)])
    sex <- c(sex, rep(1, length(nodad.idx)))
    dadid <- c(dadid, rep(0, length(nodad.idx)))
    momid <- c(momid, rep(0, length(nodad.idx)))
  }
  ## children with dad in ped, mom missing
  addids <- addids[!(addids %in% id)]
  if (any(mindex == 0 & findex != 0)) {
    nomom.idx <- which(mindex == 0 & findex != 0)
    momid[nomom.idx] <- addids[1:length(nomom.idx)]
    id <- c(id, addids[1:length(nomom.idx)])
    sex <- c(sex, rep(2, length(nomom.idx)))
    dadid <- c(dadid, rep(0, length(nomom.idx)))
    momid <- c(momid, rep(0, length(nomom.idx)))
  }
  return(data.frame(id = id, momid = momid, dadid = dadid, 
                    sex = sex))
}

