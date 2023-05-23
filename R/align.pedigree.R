## Automatically generated from all.nw using noweb

#' Generate plotting information for a pedigree
#'
#' Given a pedigree, this function creates helper matrices that descibe the
#' layout of a plot of the pedigree.
#'
#' This is an internal routine, used almost exclusively by
#' \code{plot.pedigree}.  The subservient functions \code{alignped1},
#' \code{alignped2}, \code{alignped3}, and \code{alignped4} contain the bulk of
#' the computation.
#'
#' @param ped a pedigree object
#' @param packed should the pedigree be compressed, i.e., to allow diagonal
#' lines connecting parents to children in order to have a smaller overall
#' width for the plot.
#' @param hints plotting hints for the pedigree.  This is a list with
#' components \code{order} and \code{spouse}, the second one is optional.  If
#' the hints are missing the \code{autohint} routine is called to supply an
#' initial guess.
#'
#' The order component is a numeric vector with one element per subject in the
#' pedigree.  It determines the relative order of subjects within a sibship, as
#' well as the relative order of processing for the founder couples. (For this
#' latter, the female founders are ordered as though they were sisters). The
#' spouse component is a matrix with one row per hinted marriage, usually only
#' a few marriages in a pedigree will need an added hint, for instance reverse
#' the plot order of a husband/wife pair. Each row contains the index of the
#' left spouse, the right hand spouse, and the anchor: 1=left, 2=right,
#' 0=either.  Children will preferentially appear under the parents of the
#' anchored spouse.
#' @param width for a packed output, the minimum width
#' @param align for a packed pedigree, align children under parents (TRUE), to
#' the extent possible given the page width, or align to to the left margin
#' (FALSE).  The latter is mostly used by internal routines.
#' @return a structure with components \item{n}{a vector giving the number of
#' subjects on each horizonal level of the plot} \item{nid}{ a matrix with one
#' row for each level, giving the numeric id of each subject plotted.  (An
#' value of 17 means the 17th subject in the pedigree).  } \item{pos}{a matrix
#' giving the horizontal position of each plot point} \item{fam}{ a matrix
#' giving the family id of each plot point.  A value of "3" would mean that the
#' two subjects in positions 3 and 4, in the row above, are this subject's
#' parents.} \item{spouse}{ a matrix with values 1= subject plotted to the
#' immediate right is a spouse, 2= subject plotted to the immediate right is an
#' inbred spouse, 0 = not a spouse} \item{twins}{ optional matrix which will
#' only be present if the pedigree contains twins.  It has values 1= sibling to
#' the right is a monozygotic twin, 2= sibling to the right is a dizygotic
#' twin, 3= sibling to the right is a twin of unknown zygosity, 0 = not a twin}
#' @seealso \code{\link{plot.pedigree}}, \code{\link{autohint}}
#' @keywords dplot
#' @export align.pedigree
align.pedigree <- function(ped, packed=TRUE, width=10, align=TRUE, hints=ped$hints) {
    
    if ('pedigreeList' %in% class(ped)) {
        nped <- length(unique(ped$famid))
        alignment <- vector('list', nped)
        for (i in 1:nped) {
            temp <- align.pedigree(ped[i], packed, width, align)
            alignment[[i]] <- temp$alignment
            }
        ped$alignment <- alignment
        class(ped) <- 'pedigreeListAligned'
        return(ped)
        }
    
    if (is.null(hints)) {
      hints <- try({autohint(ped)}, silent=TRUE)
      ## sometimes appears dim(ped) is empty (ped is NULL), so try fix here: (JPS 6/6/17
      if("try-error" %in% class(hints)) hints <- list(order=seq_len(max(1, dim(ped)))) ## 1:dim(ped))
    } else {
      hints <- check.hint(hints, ped$sex)
    }
    ## Doc: Setup-align
    n <- length(ped$id)
    dad <- ped$findex; mom <- ped$mindex  #save typing
    if (any(dad==0 & mom>0) || any(dad>0 & mom==0))
            stop("Everyone must have 0 parents or 2 parents, not just one")
    level <- 1 + kindepth(ped, align=TRUE)

    horder <- hints$order   # relative order of siblings within a family

    if (is.null(ped$relation)) relation <- NULL
    else  relation <- cbind(as.matrix(ped$relation[,1:2]), 
                            as.numeric(ped$relation[,3]))

    if (!is.null(hints$spouse)) { # start with the hints list
        tsex <- ped$sex[hints$spouse[,1]]  #sex of the left member
        spouselist <- cbind(0,0,  1+ (tsex!='male'), 
                            hints$spouse[,3])
        spouselist[,1] <- ifelse(tsex=='male', hints$spouse[,1], hints$spouse[,2])
        spouselist[,2] <- ifelse(tsex=='male', hints$spouse[,2], hints$spouse[,1])
        }
    else spouselist <- matrix(0L, nrow=0, ncol=4)

    if (!is.null(relation) && any(relation[,3]==4)) {
        # Add spouses from the relationship matrix
        trel <- relation[relation[,3]==4,,drop=F]
        tsex <- ped$sex[trel[,1]]
        trel[tsex!='male',1:2] <- trel[tsex!='male',2:1]
        spouselist <- rbind(spouselist, cbind(trel[,1],
                                              trel[,2],
                                              0,0))
        }
    if (any(dad>0 & mom>0) ) {
        # add parents
        who <- which(dad>0 & mom>0)
        spouselist <- rbind(spouselist, cbind(dad[who], mom[who], 0, 0))
        }

    hash <- spouselist[,1]*n + spouselist[,2]
    spouselist <- spouselist[!duplicated(hash),, drop=F]
    
    ## Doc: Founders -align
    noparents <- (dad[spouselist[,1]]==0 & dad[spouselist[,2]]==0)
     ##Take duplicated mothers and fathers, then founder mothers
    dupmom <- spouselist[noparents,2][duplicated(spouselist[noparents,2])]
       ##^Founding mothers with multiple marriages
    dupdad <- spouselist[noparents,1][duplicated(spouselist[noparents,1])]
       ##^Founding fathers with multiple marriages
    foundmom <- spouselist[noparents&!(spouselist[,1] %in% c(dupmom,dupdad)),2] # founding mothers
    founders <-  unique(c(dupmom, dupdad, foundmom))    
    founders <-  founders[order(horder[founders])]  #use the hints to order them
    rval <- alignped1(founders[1], dad, mom, level, horder, 
                              packed=packed, spouselist=spouselist)

    if (length(founders)>1) {
        spouselist <- rval$spouselist
        for (i in 2:length(founders)) {
            rval2 <- alignped1(founders[i], dad, mom,
                               level, horder, packed, spouselist)
            spouselist <- rval2$spouselist
            rval <- alignped3(rval, rval2, packed)
            }
        }
    ## Doc: finish-align (1)
    # Unhash out the spouse and nid arrays
    # 
    nid    <- matrix(as.integer(floor(rval$nid)), nrow=nrow(rval$nid))
    spouse <- 1L*(rval$nid != nid)
    maxdepth <- nrow(nid)

    # For each spouse pair, find out if it should be connected with
    #  a double line.  This is the case if they have a common ancestor
    ancestor <- function(me, momid, dadid) {
        alist <- me
        repeat {
            newlist <- c(alist, momid[alist], dadid[alist])
            newlist <- sort(unique(newlist[newlist>0]))
            if (length(newlist)==length(alist)) break
            alist <- newlist
            }
        alist[alist!=me]
        }
    for (i in (1:length(spouse))[spouse>0]) {
        a1 <- ancestor(nid[i], mom, dad)
        a2 <- ancestor(nid[i+maxdepth],mom, dad)  #matrices are in column order
        if (any(duplicated(c(a1, a2)))) spouse[i] <- 2
        }
    ## Doc: finish align(2)
    if (!is.null(relation) && any(relation[,3] < 4)) {
        twins <- 0* nid
        who  <- (relation[,3] <4)
        ltwin <- relation[who,1]
        rtwin <- relation[who,2]
        ttype <- relation[who,3]
        
        # find where each of them is plotted (any twin only appears
        #   once with a family id, i.e., under their parents)
        ntemp <- ifelse(rval$fam>0, nid,0) # matix of connected-to-parent ids
        ltemp <- (1:length(ntemp))[match(ltwin, ntemp, nomatch=0)]
        rtemp <- (1:length(ntemp))[match(rtwin, ntemp, nomatch=0)]
        twins[pmin(ltemp, rtemp)] <- ttype
        }
    else twins <- NULL
    ## Doc: finish align(3)
    if ((is.numeric(align) || align) && max(level) >1) 
        pos <- alignped4(rval, spouse>0, level, width, align)
    else pos <- rval$pos

    if (is.null(twins))
         list(n=rval$n, nid=nid, pos=pos, fam=rval$fam, spouse=spouse)
    else list(n=rval$n, nid=nid, pos=pos, fam=rval$fam, spouse=spouse, 
                  twins=twins)
}
