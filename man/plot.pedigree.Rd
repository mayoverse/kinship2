\name{plot.pedigree}
\alias{plot.pedigree}
\title{
  plot pedigrees
}
\description{
plot objects created with the pedigree function
}
\usage{
\method{plot}{pedigree}(x, id = x$id, status = x$status, affected =
                 x$affected, cex = 1, col = 1, symbolsize = 1, branch =
                 0.6, packed = TRUE, align = c(1.5,2), width = 8,
                 density = c(-1, 35, 55, 25), mar = c(4.1, 1, 4.1, 1),
                 angle = c(90, 65, 40, 0), keep.par = FALSE, subregion,
                 pconnect= .5, ...)
}
\arguments{
\item{x}{
object created by the function pedigree.
}
\item{id}{
id variable - used for labeling.  
}
\item{status}{
can be missing.  If it exists, 0=alive/missing and 1=death.
}
\item{affected}{
vector, or matrix with up to 4 columns for affected
indicators. Subject's symbol is divided into sections for each
status, shaded if indicator is 1, not-shaded for 0, and symbol "?"
if missing (NA)
}
\item{cex}{
controls text size.  Default=1.
}
\item{col}{
color for each id.  Default assigns the same color to everyone.
}
\item{symbolsize}{
controls symbolsize. Default=1.
}
\item{branch}{
defines how much angle is used to connect various levels of nuclear families.
}
\item{packed}{
default=T.  If T, uniform distance between all individuals at a given level.
}
\item{align}{ these parameters control the extra effort spent trying to
  align children underneath parents, but without making the pedigree too
  wide.  Set to F to speed up plotting.
}
\item{width}{
default=8.  For a packed pedigree, the minimum width allowed in the
realignment of pedigrees.
}
\item{density}{
defines density used in the symbols.  Takes up to 4 different values.
}
\item{mar}{margin parmeters, as in the \code{par} function}
\item{angle}{
defines angle used in the symbols.  Takes up to 4 different values.
}
\item{keep.par}{
Default = F, allows user to keep the parameter settings the same as they
were for plotting (useful for adding extras to the plot)
}
\item{subregion}{4-element vector for (min x, max x, min depth, max depth),
  used to edit away portions of the plot coordinates returned by
  align.pedigree
}
\item{pconnect}{when connecting parent to children the program will try
  to make the connecting line as close to vertical as possible, subject
  to it lying inside the endpoints of the line that connects the
  children by at least \code{pconnect} people.  Setting this option
  to a large number will force the line to connect at the midpoint of
  the children.}

\item{\dots}{
Extra options that feed into the plot function.
}
}
\value{
  an invisible list containing
  \item{plist}{a list that contains all the position information for
    plotting the pedigree.  This will useful for further functions
    (yet unwritten) for manipulating the plot, but likely not to an
    ordinary user.}
  \item{x,y}{the x an and y plot coordinates of each subject in the
    plot. The coordinate is for the top of the plotted symbol.
    These will be in the same order as the input pedigree.  If
    someone in the pedigree does not appear in the plot their
    coordinates will be NA.  If they appear multiple times one of the
    instances is chosen.  (Which one is a function of the order in
    which the pedigree was constructed.)}
  \item{boxh}{the height of the symbol, in user coordinates}
  \item{boxw}{the width of the symbol}
  \item{call}{a copy of the call that generated the plot}
}
\section{Side Effects}{
creates plot on current plotting device.
} 
\details{  
  Two important parameters control the looks of the result.
  One is the user specified
maximum width.  The smallest possible width is the maximum number of subjects
on a line, if the user's suggestion  %'
is too low it is increased to 1+ that
amount (to give just a little wiggle room).
To make a pedigree where all children are centered under parents simply
make the width large enough, however, the symbols may get very small.

The second is \code{align}, a vector of 2 alignment parameters $a$ and $b$.
For each set of siblings at a set of locations
\code{x} and with parents at \code{p=c(p1,p2)} the
alignment penalty is
\deqn{(1/k^a)\sum{i=1}{k} [(x_i - (p1+p2)/2)]^2}
        {sum(x- mean(p))^2/(k^a)}
where k is the number of siblings in the set.
when $a=1$ moving a sibship with $k$ sibs one unit to the left or
right of optimal will incur the same cost as moving one with only 1 or
two sibs out of place.  If $a=0$ then large sibships are harder to move
than small ones, with the default value $a=1.5$ they are slightly easier 
to move than small ones.  The rationale for the default is as long as the
parents are somewhere between the first and last siblings the result looks
fairly good, so we are more flexible with the spacing of a large family.
By tethering all the sibs to a single spot they are kept close to 
each other.
The alignment penalty for spouses is
\eqn{b(x_1 - x_2)^2}{b *(x1-x2)^2}, which tends to keep 
them together.  The size of $b$ controls the relative importance of sib-parent
and spouse-spouse closeness.
}
\seealso{
\code{\link{pedigree}}
}
\examples{
data(sample.ped)

pedAll <- pedigree(sample.ped$id, sample.ped$father, sample.ped$mother, 
       sample.ped$sex,  #affected=sample.ped$affected,
       affected=cbind(sample.ped$affected, sample.ped$avail), 
       famid=sample.ped$ped)

ped2 <- pedAll['2']

print(ped2)


## plot(ped2)

}

\keyword{hplot, genetics}
