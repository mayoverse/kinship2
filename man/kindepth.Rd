\name{kindepth}
\Rdversion{1.1}
\alias{kindepth}
\title{Compute the depth of each subject in a pedigree}
\description{
  Mark each person as to their depth in a pedigree;
  0 for a founder, otherwise depth = 1 + max(father's depth, mother's
  depth)
}
\usage{kindepth(id, dad.id, mom.id, align = FALSE)}
\arguments{
  \item{id}{Identification code for each individual}
  \item{dad.id}{Id code for the father}
  \item{mom.id}{Id code for the mother}
  \item{align}{If align=T, go one step further and try to make both
    parents of each child have the same depth.
    (This is not always possible).  It helps
    the drawing program by lining up pedigrees that "join in the middle"
    via a marriage.
  }
}
\details{In the case of an inbred pedigree a perfect alignment obeying
  \code{extra=TRUE} may not exist.}
\value{
  an integer vector containing the depth for each subject}
\author{Terry Therneau}
\seealso{\code{\link{plot.pedigree}}}
\keyword{ genetics }
