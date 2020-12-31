\name{legendPlot}
\alias{legendPlot}
\title{Legend Pedigree Plot}
\description{
Pedigree plot with ready-made legend along the bottom of the page to represent colors 
and affection statuses
}
\usage{
legendPlot(x, id = x$id, affected = x$affected,
  affected.label = NULL,  col = 1,
  col.label = NULL, symbolsize = 0.75,
  cex = 0.5, ...)
}
\arguments{
\item{x}{Pedigree object created by \code{kinship2:::pedigree()}}
\item{id}{Optional, a character string to replace the correspinding id for persons in the pedigree}
\item{affected}{A variable indicating affection status. A multi-column
  matrix can be used to give the status with respect to multiple
  traits. Logical, factor, and integer types are converted to 0/1
  representing unaffected and affected, respectively. NAs are
  considered missing, and represented as a question mark.}
\item{affected.label}{Set labels for affection statuses. Must be in the
  same order as the affected matrix in the pedigree object, or a named
  vector matching the column names of the affected matrix}

\item{col}{Colors for the plot symbol for each individual}
\item{col.label}{Named vector, with elements matching the unique color codes, the names are the labels used in the legend.}
\item{symbolsize}{Size of symbols. Default is 0.75 because of the extra
  space provided for the legend, versus the standard pedigree plot}
\item{cex}{Size of pedigee and legend text. Default is 0.5}
\item{\dots}{Optional plot parameters for \code{par} and \code{plot} functions}
}
\examples{
require(kinship2)
data(sample.ped)
pedAll <- pedigree(sample.ped$id, sample.ped$father, sample.ped$mother, sample.ped$sex, affected=cbind(sample.ped$affected, sample.ped$avail), famid=sample.ped$ped)
ped1 <- pedAll['1']
legendPlot(ped1,  affected.label=c("cancer","available")
}
\seealso{
\code{\link{pedigree}}, \code{\link{plot.pedigree}}
}
\author{
Jason Sinnwell, with code contributed by Sara Achenbach
}
