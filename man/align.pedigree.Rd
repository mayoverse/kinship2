\name{align.pedigree}
\alias{align.pedigree}
\title{
  Generate plotting information for a pedigree
}
\description{
Given a pedigree, this function creates helper matrices that descibe
the layout of a plot of the pedigree.
}
\usage{
align.pedigree(ped, packed=TRUE, width=10, align=TRUE, hints=ped$hints)
}
\arguments{
\item{ped}{a pedigree object}
\item{packed}{
should the pedigree be compressed, i.e., to allow diagonal lines connecting
parents to children in order to have a smaller overall width for the plot.
}
\item{hints}{plotting hints for the pedigree.
  This is a list with components \code{order} and \code{spouse}, the
  second one is optional.  If the hints are missing the \code{autohint}
  routine is called to supply an initial guess.

The order component is a numeric vector with one element per subject in
the pedigree.  It determines the relative order of subjects within a sibship,
as well as the relative order of processing for the founder couples.
(For this latter, the female founders are ordered as though they were sisters).
The spouse component is a matrix with one row per hinted marriage,
usually only a few marriages in a pedigree will need an added hint, for
instance reverse the plot order of a husband/wife pair.
Each row contains the index of the left spouse, the right hand spouse,
and the anchor: 1=left, 2=right, 0=either.  Children will preferentially
appear under the parents of the anchored spouse.
}
\item{width}{
for a packed output, the minimum width
}
\item{align}{
  for a packed pedigree, align children under parents (TRUE), to the
  extent possible given the page width, or align to to the left margin (FALSE).
  The latter is mostly used by internal routines.
}
}
\value{
a structure with components
\item{n}{a vector giving the number of subjects on each horizonal level
of the plot}
\item{nid}{
   a matrix with one row for each level, giving the numeric id of each subject
   plotted.  (An value of 17 means the 17th subject in the pedigree).
 }
\item{pos}{a matrix giving the horizontal position of each plot point}
\item{fam}{
  a matrix giving the family id of each plot point.  A value of "3" would mean
  that the two subjects in positions 3 and 4, in the row above, are this
  subject's parents.}
\item{spouse}{
  a matrix with values 1= subject plotted to the immediate right is a spouse,
  2= subject plotted to the immediate right is an inbred spouse, 0 = not a 
  spouse}
\item{twins}{
  optional matrix which will only be present if the pedigree contains twins.
  It has values 1= sibling to the right is a monozygotic twin, 2= sibling to
  the right is a dizygotic twin, 3= sibling to the right is a twin of unknown
  zygosity, 0 = not a twin}
}
\details{ This is an internal routine, used almost exclusively by
    \code{plot.pedigree}.
  The subservient functions \code{alignped1}, 
  \code{alignped2}, 
  \code{alignped3}, 
  and \code{alignped4} contain the bulk of the computation.
}
\seealso{\code{\link{plot.pedigree}}, \code{\link{autohint}}}
\keyword{dplot}
