# Automatically generated from all.nw using noweb

#' Create a sparse kinship matrix
#'
#' @description
#' Compute the overall kinship matrix for a collection of families, and store
#' it efficiently.
#'
#' @details
#' This command is deprecated.  The kinship command now can be applied directly
#' to pedigreeList objects.
#'
#' @param famid a vector of family identifiers
#' @param id a vector of unique subject identifiers
#' @param father.id for each subject, the identifier of their biolgical father
#' @param mother.id for each subject, the identifier of thier biological mother
#' @param unrelated subjects with this family id are considered to be unrelated
#' singletons, i.e., not related to each other or to anyone else.
#'
#' @return a sparse kinship matrix of class \code{bdsmatrix}
#'
#' @examples
#'
#' # Data set from a large family study of breast cancer
#' #  there are 26050 subjects in the file, from 426 families
#' \dontrun{
#' > table(cdata$sex)
#'      F     M
#'  12699 13351
#' > length(unique(cdata$famid))
#' [1] 426
#'
#' > kin1 <- makekinship(cdata$famid, cdata$gid, cdata$dadid, cdata$momid)
#' > dim(kin1)
#' [1] 26050 26050
#' > class(kin1)
#' [1] "bdsmatrix"
#' # The next line shows that few of the elements of the full matrix are >0
#' > length(kin1@blocks)/ prod(dim(kin1))
#' [1] 0.00164925
#'
#' # kinship matrix for the females only
#' > femid <- cdata$gid[cdata$sex=='F']
#' > femindex <- !is.na(match(dimnames(kin1)[[1]], femid))
#' > kin2 <- kin1[femindex, femindex]
#' #
#' # Note that "femindex <- match(femid, dimnames(kin1)[[1]])" is wrong, since
#' #  then kin1[femindex, femindex] might improperly reorder the rows/cols
#' #  (if families were not contiguous in cdata).
#' # However sort(match(femid, dimnames(kin1)[[1]])) would be okay.
#' }
#'
#' @seealso \code{\link{kinship}}, \code{\link{makefamid}}
#' @keywords genetics
#' @export makekinship
makekinship <- function(famid, id, father.id, mother.id, unrelated=0) {
    n <- length(famid)
    if (length(id)    != n) stop("Mismatched lengths: famid and id")
    if (length(mother.id) != n) stop("Mismatched lengths: famid and mother.id")
    if (length(father.id) != n) stop("Mismatched lengths: famid and father.id")
    if (any(is.na(famid)))  stop("One or more subjects with missing family id")
    if (any(is.na(id)))     stop("One or more subjects with a missing id")
    if (is.numeric(famid)) {
        if (any(famid <0))      stop("Invalid family id, must be >0")
        }

    if (any(duplicated(id))) stop("Subject ids must be unique")

    famlist <- sort(unique(famid))  #same order as the counts table
    idlist <- id            # will be overwritten, but this makes it the
                            #  correct data type and length
    counts <- table(famid)
    cumcount <- cumsum(counts)
     if (any(famid==unrelated)) {
        # Assume that those with famid of 0 are unrelated uniques
        #   (usually the marry-ins)
        temp <- match(unrelated, names(counts))
        nzero <- counts[temp]
        counts <- counts[-temp]
        famlist <- famlist[famlist != unrelated]
        idlist[1:nzero] <- id[famid== unrelated]
        cumcount <- cumsum(counts) + nzero
        }
    else nzero <- 0

    mlist <- vector('list', length(counts))
    for (i in 1:length(counts)) {
        who <- (famid == famlist[i])
        if (sum(who) ==1) mlist[[i]] <- Matrix(0.5)  # family of size 1
        else {
            mlist[[i]] <- kinship(id[who], mother.id[who], father.id[who])
            }
        idlist[seq(to=cumcount[i], length=counts[i])] <- id[who]
        }

    if (nzero>0) mlist <- c(list(Diagonal(nzero)), mlist)
    kmat <- forceSymmetric(bdiag(mlist))
    dimnames(kmat) <- list(idlist, idlist)
    kmat
}
