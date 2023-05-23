## Extracted from checks.Rnw

#' Check family
#'
#' @description
#' Error check for a family classification
#'
#' @details
#' Given a family id vector, also compute the familial grouping from first
#' principles using the parenting data, and compare the results.
#'
#' The \code{makefamid} function is used to create a de novo family id from the
#' parentage data, and this is compared to the family id given in the data.
#'
#' If there are any joins, then an attribute "join" is attached.
#' It will be a matrix with famid as row labels, new-family-id as the columns,
#' and the number of subjects as entries.
#'
#' @param famid A vector of family identifiers
#' @param id A vector of unique subject identifiers
#' @param father.id Vector containing the id of the biological father
#' @param mother.id Vector containing the id of the biological mother
#' @param newfam The result of a call to \code{makefamid}. If this has already
#' been computed by the user, adding it as an argument shortens the running
#' time somewhat.
#'
#' @return a data frame with one row for each unique family id in the
#' \code{famid} argument. Components of the output are:
#' \item{famid}{ The family id, as entered into the data set }
#' \item{n}{ Number of subjects in the family }
#' \item{unrelated}{ Number of them that appear to be unrelated to
#' anyone else in the entire pedigree set.  This is usually marry-ins with no
#' children (in the pedigree), and if so are not a problem. }
#' \item{split}{ Number of unique "new" family ids.
#' 0 = no one in this "family" is related to anyone else (not good)
#' 1 = everythings is fine
#' 2+= the family appears to be a set of disjoint trees.
#' Are you missing some of the people? }
#' \item{join}{ Number of other families that had a unique
#' famid, but are actually joined to this one.  0 is the hope. }
#'
#' @examples
#'
#' # use 2 sample peds
#' data(sample.ped)
#' pedAll <- with(sample.ped, pedigree(id, father, mother, sex,
#'                     affected=cbind(affected, avail), famid=ped))
#'
#' ## check them giving separate ped ids
#' fcheck.sep <- with(sample.ped, familycheck(ped, id, father, mother))
#' fcheck.sep
#'
#' ## check assigning them same ped id
#' fcheck.combined <- with(sample.ped, familycheck(rep(1,nrow(sample.ped)), id, father, mother))
#' fcheck.combined
#'
#' #make person 120's father be her son.
#' sample.ped[20,3] <- 131
#' fcheck1.bad <- try({with(sample.ped, familycheck(ped, id, father, mother))}, silent=FALSE)
#'
#' ## fcheck1.bad is a try-error
#'
#' @seealso \code{\link{makefamid}}, \code{\link{makekinship}}
#' @keywords genetics
#' @export familycheck
familycheck <- function(famid, id, father.id, mother.id, newfam) {
    if (is.numeric(famid) && any(is.na(famid)))
        stop ("Family id of missing not allowed")
    nfam <- length(unique(famid))

    if (missing(newfam)) newfam <- makefamid(id, father.id, mother.id)
    else if (length(newfam) != length(famid))
        stop("Invalid length for newfam")

    xtab <- table(famid, newfam)
    if (any(newfam==0)) {
        unrelated <- xtab[,1]
        xtab <- xtab[,-1, drop=FALSE] 
        ## bug fix suggested by Amanda Blackford 6/2011
      }
    else unrelated <-  rep(0, nfam)

    splits <- apply(xtab>0, 1, sum)
    joins  <- apply(xtab>0, 2, sum)

    temp <- apply((xtab>0) * outer(rep(1,nfam), joins-1), 1, sum)

    out <- data.frame(famid = dimnames(xtab)[[1]],
                      n = as.vector(table(famid)),
                      unrelated = as.vector(unrelated),
                      split = as.vector(splits),
                      join = temp,
                      row.names=1:nfam)
    if (any(joins >1)) {
      tab1 <- xtab[temp>0,]  #families with multiple outcomes
      tab1 <- tab1[,apply(tab1>0,2,sum) >0] #only keep non-zero columns
      dimnames(tab1) <- list(dimnames(tab1)[[1]], NULL)
      attr(out, 'join') <- tab1
    }   
    out
}

