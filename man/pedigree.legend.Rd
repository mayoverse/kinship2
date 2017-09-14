
\name{pedigree.legend}
\alias{pedigree.legend}
\title{
Plot a pedigree legend
}
\description{
Plot a pedigree legend that describes the affected indiactors for a subject
}
\usage{
pedigree.legend(ped, labels = dimnames(ped$affected)[[2]],
    edges = 200, radius=NULL, location="bottomright", new=TRUE,
    density = c(-1, 35, 55, 25),  angle = c(90, 65, 40, 0), ...) 
}
\arguments{
\item{ped}{
  A pedigree object with id, dadid, momid, sex, affected
}
\item{labels}{
  names for the affected indicators
}
\item{edges}{
  Number of edges for each polygon. Higher numbers give better
  resolution for the circle.
}
\item{radius}{
length of the circle's radius
}
\item{location}{
  Similar to how the location of a regular legend is given, used only if
  new=TRUE.  A character string indicating which of the four corners to
  plot the legend, given by "bottomright", "bottomleft", "topleft",
  or "topright". 
}
\item{new}{
  Logical. If TRUE, plot the legend on the current plot. Otherwise,
  plot on a separate plot.
}
\item{density}{
  Density of lines shaded in sections of the circle. These match the
  density settings for the plot.pedigree function.
}
\item{angle}{
  Angle at which lines are shaded in sections of the circle. These match
  the angles for the plot.pedigree function.
}
\item{...}{
optional parameters for the plot function that apply to text.
}
}
\value{
Nothing is returned.
}
\details{
  Adapt the pie() function to plot a circle with affected
  indicators. When new=TRUE, much care is needed to keep track of
  location and scaling of lines and text.
}
\seealso{
\code{\link{plot.pedigree}}
}
\examples{
data(sample.ped)

fam1 <- sample.ped[sample.ped$ped==1,]

ped1 <- with(fam1, pedigree(id, father, mother, sex,
                 affected=cbind(avail,affected)))

plot(ped1)
pedigree.legend(ped1, location="bottomright", radius=.8)
pedigree.legend(ped1, location="topleft", radius=.6, cex=1.2)

pedigree.legend(ped1, new=FALSE)


}
\keyword{pedigree}
% docclass is function
% Converted by Sd2Rd version 43268.
