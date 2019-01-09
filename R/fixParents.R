## Author: Jason Sinnwell
## Date: 5/23/2017
## Updated: 1/7/2019

## id, dadid, momid, sex, missid all as would be passed to pedigree within a single family
## returned data.frame to be used with(df, pedigree(id, dadid, momid, sex))

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

