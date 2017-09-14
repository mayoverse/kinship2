\name{autohint}
\alias{autohint}
\title{
  Align a pedigree to print well
}
\description{
A pedigree structure can contain a \code{hints} object
which helps to reorder the pedigree (e.g. left-to-right order of
children within family) so as to plot with minimal distortion.
This routine is used to create an initial version of the
hints.  They can then be modified if desired.
}
\usage{
autohint(ped, hints, packed=TRUE, align=FALSE)
}
\arguments{
\item{ped}{ a pedigree structure}
\item{hints}{ optional hints object. Only the order
  component is used.
}
\item{packed}{
  If TRUE, uniform distance between all individuals at a given level.
}
\item{align}{ these parameters control the extra effort spent trying to
  align children underneath parents, but without making the pedigree too
  wide.  Set to FALSE to speed up plotting.
}
}
\value{a list containing components \code{order} and \code{spouse}}

\details{
This routine would not normally be called by a user. 
It moves children within families, so that marriages are on the "edge"
of a set children, closest to the spouse.  
For pedigrees that have only a single connection between two families 
this simple-minded approach works surprisingly well.
For more complex structures hand-tuning of the hints matrix may be
required.

The pedigree in the example below is one where rearranging the founders
greatly decreases the number of extra connections.
When autohint is called with a a vector of numbers as the second
argument, the values for the founder
females are used to order the founder families left to right across
the plot.  The values within a sibship are used as the preliminary
order of siblings within a family; this may be changed to move one
of them to the edge so as to match up with a spouse.
The actual values in the vector are not important, only their order.
}
\examples{
data(testped1)
ped1 <- with(testped1, pedigree(id, father, mother, sex))
plot(ped1, cex=.7, symbolsize=.7)

# rearrange some founders
temp <- 1:nrow(testped1)
temp[76] <- .1
temp[77] <- .2
temp[74] <- .3
temp[60] <- .4
temp[30] <- temp[8] + .1
temp[65] <- temp[4] + .1
temp[14] <- temp[3] + .1
ped1$hints <- autohint(ped1, temp)
plot(ped1, cex=.7)
}

\seealso{
pedigree, besthint
}
\keyword{genetics}
% docclass is function

