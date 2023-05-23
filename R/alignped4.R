# Automatically generated from all.nw using noweb
#TODO add params and example and return

#' Fourth routine alignement
#'
#' @description
#' This is the last of the four co-routines.
#'
#' @details
#' The alignped4 routine is the final step of alignment.  It attempts to line
#' up children under parents and put spouses and siblings `close' to each other,
#' to the extent possible within the constraints of page width.
#' The current code does necessary setup and then calls the \code{quadprog}
#' function.
#' There are two important parameters for the function:
#' One is the user specified maximum width.  The smallest possible width is the
#' maximum number of subjects on a line, if the user suggestion is too low it is
#' increased to that 1+ that amount (to give just a little wiggle room).
#' The other is a vector of 2 alignment parameters $a$ and $b$.
#' For each set of siblings ${x}$ with parents at $p_1$ and $p_2$
#' the alignment penalty is :
#' $$
#'    (1/k^a)\sum{i=1}{k} (x_i - (p_1 + p_2)^2
#' $$
#' where $k$ is the number of siblings in the set.
#' Using the fact that $\sum(x_i-c)^2 = \sum(x_i-\mu)^2 + k(c-\mu)^2$,
#' when $a=1$ then moving a sibship with $k$ sibs one unit to the left or
#' right of optimal will incur the same cost as moving one with only 1 or
#' two sibs out of place.  If $a=0$ then large sibships are harder to move
#' than small ones, with the default value $a=1.5$ they are slightly easier
#' to move than small ones.  The rationale for the default is as long as the
#' parents are somewhere between the first and last siblings the result looks
#' fairly good, so we are more flexible with the spacing of a large family.
#' By tethering all the sibs to a single spot they tend are kept close to
#' each other.
#' The alignment penalty for spouses is $b(x_1 - x_2)^2$, which tends to keep
#' them together. The size of $b$ controls the relative importance of sib-parent
#' and spouse-spouse closeness.
#'
#' \item{ Part 1 } {We start by adding in these penalties.
#' The total number of parameters in the alignment problem
#' (what we hand to quadprog) is the set of \code{sum(n)} positions.
#' A work array myid keeps track of the parameter number for each position so
#' that it is easy to find. There is one extra penalty added at the end.
#' Because the penalty amount would be the same if all the final positions were
#' shifted by a constant, the penalty matrix will not be positive definite;
#' \code{solve.QP} does not like this.
#' We add a tiny amount of leftward pull to the widest line. }
#' \item{ Part 2 }{ If there are $k$ subjects on a line there will
#' be $k+1$ constraints for that line.  The first point must be $\ge 0$, each
#' subesquent one must be at least 1 unit to the right, and the final point
#' must be $\le$ the max width. }
#'
#' @param rval
#' @param spouse
#' @param level
#' @param width
#' @param align
#'
#' @return newpos
#'
#' @examples
#' data(sample.ped)
#' ped <- with(sample.ped,pedigree(id, father, mother, sex, affected))
#' align.pedigree(ped)
#'
#' @seealso \code{\link{plot.pedigree}}, \code{\link{autohint}}
#' @keywords dplot
#' @export alignped4
alignped4 <- function(rval, spouse, level, width, align) {
    ## Doc: alignped4 -part1, spacing across page
    if (is.logical(align)) align <- c(1.5, 2)  #defaults
    maxlev <- nrow(rval$nid)
    width <- max(width, rval$n+.01)   # width must be > the longest row

    n <- sum(rval$n)  # total number of subjects
    myid <- matrix(0, maxlev, ncol(rval$nid))  #number the plotting points
    for (i in 1:maxlev) {
        myid[i, rval$nid[i,]>0] <-  cumsum(c(0, rval$n))[i] + 1:rval$n[i]
    }
    # There will be one penalty for each spouse and one for each child
    npenal <- sum(spouse[rval$nid>0]) + sum(rval$fam >0)
    pmat <- matrix(0., nrow=npenal+1, ncol=n)

    ## Doc: alignped4 -part2
    indx <- 0
    # Penalties to keep spouses close
    for (lev in 1:maxlev) {
        if (any(spouse[lev,])) {
            who <- which(spouse[lev,])
            indx <- max(indx) + 1:length(who)
            pmat[cbind(indx, myid[lev,who])] <-  sqrt(align[2])
            pmat[cbind(indx, myid[lev,who+1])] <- -sqrt(align[2])
        }
    }

    # Penalties to keep kids close to parents
    for (lev in (1:maxlev)[-1])  { # no parents at the top level
        families <- unique(rval$fam[lev,])
        families <- families[families !=0]  #0 is the 'no parent' marker
        for (i in families) {  #might be none
            who <- which(rval$fam[lev,] == i)
            k <- length(who)
            indx <- max(indx) +1:k   #one penalty per child
            penalty <- sqrt(k^(-align[1]))
            pmat[cbind(indx, myid[lev,who])] <- -penalty
            pmat[cbind(indx, myid[lev-1, rval$fam[lev,who]])] <- penalty/2
            pmat[cbind(indx, myid[lev-1, rval$fam[lev,who]+1])] <- penalty/2
        }
    }
    maxrow <- min(which(rval$n==max(rval$n)))
    pmat[nrow(pmat), myid[maxrow,1]] <- 1e-5
    ncon <- n + maxlev    # number of constraints
    cmat <- matrix(0., nrow=ncon, ncol=n)
    coff <- 0  # cumulative constraint lines so var
    dvec <- rep(1., ncon)
    for (lev in 1:maxlev) {
        nn <- rval$n[lev]
        if (nn>1) {
            for (i in 1:(nn-1)) 
                cmat[coff +i, myid[lev,i + 0:1]] <- c(-1,1)
        }

        cmat[coff+nn,   myid[lev,1]]  <- 1     #first element >=0
        dvec[coff+nn] <- 0
        cmat[coff+nn+1, myid[lev,nn]] <- -1    #last element <= width-1
        dvec[coff+nn+1] <- 1-width
        coff <- coff + nn+ 1
    }

    if (exists('solve.QP')) {
         pp <- t(pmat) %*% pmat + 1e-8 * diag(ncol(pmat))
         fit <- solve.QP(pp, rep(0., n), t(cmat), dvec)
    }
    else stop("Need the quadprog package")

    newpos <- rval$pos
    #fit <- lsei(pmat, rep(0, nrow(pmat)), G=cmat, H=dvec)
    #newpos[myid>0] <- fit$X[myid]           
    newpos[myid>0] <- fit$solution[myid]
    newpos
}
