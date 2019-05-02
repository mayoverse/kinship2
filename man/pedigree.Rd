\name{pedigree}
\alias{pedigree}
\alias{as.data.frame.pedigree}
\alias{print.pedigree}
\title{
  Create pedigree structure 
}
\description{
Create pedigree structure in the format needed for plotting.
}
\usage{
pedigree(id, dadid, momid, sex, affected, status, relation, famid,
missid)
}
\arguments{
\item{id}{
Identification variable for individual
}
\item{dadid}{
  Identification variable for father. Founders' parents should be coded
  to NA, or another value specified by missid.
}
\item{momid}{
  Identification variable for mother. Founders' parents should be coded
  to NA, or another value specified by missid. 
}
\item{sex}{
Gender of individual noted in
`id'. Either character ("male","female","unknown","terminated")
or numeric (1="male", 2="female", 3="unknown", 4="terminated")
data is allowed.  For character data the string may be truncated,
and of arbitrary case.
}
\item{affected}{
  A variable indicating affection status.  A multi-column matrix
  can be used to give the status with respect to multiple traits.
  Logical, factor, and integer types are converted to 0/1 representing
  unaffected and affected, respectively. NAs are considered missing.
}
\item{status}{
Status (0="censored", 1="dead")
}
\item{relation}{
A matrix with 3 required columns (id1, id2, code) specifying special relationship
between pairs of individuals. Codes: 1=Monozygotic
twin,  2=Dizygotic twin, 3=Twin of unknown zygosity, 4=Spouse.
(The last is necessary in order to place a marriage with no children
into the plot.) If famid is given in the call to create pedigrees, then
famid needs to be in the last column of the relation matrix. Note for
tuples of >= 3 with a mixture of zygosities,
the plotting is limited to showing pairwise zygosity of adjacent subjects,
so it is only necessary to specify the pairwise zygosity, in the order
the subjects are given or appear on the plot.
}

\item{famid}{
An optional vector of family identifiers.  If it is present the
result will contain individual pedigrees for each family in the set,
which can be extacted using subscripts.
Individuals need to have a unique id \emph{within} family.
}

\item{missid}{
  The founders are those with no father or mother in the pedigree.  The
  \code{dadid} and \code{momid} values for these subjects will either be
  NA or the value of this variable.  The default for \code{missid} is 0
  if the \code{id} variable is numeric, and "" (the empty string)
  otherwise.}
}

\value{
  An object of class \code{pedigree} or \code{pedigreeList}
  Containing the following items:
  famid id findex mindex sex  affected status relation
}
\seealso{
\code{\link{plot.pedigree}}, \code{\link{autohint}}
}
\examples{
data(minnbreast)

bpeds <- with(minnbreast,
    pedigree(id, fatherid, motherid, sex, affected=proband, famid=famid))

## pedigree with id=8
bped.id8 <- bpeds['8']
print(bped.id8)
plot(bped.id8)

## show this pedigree with mixed zygosity quadruplets
rel8 <- data.frame(id1=c(137,138,139), id2=c(138,139,140), code=c(1,2,2))
bped.id8 <- with(minnbreast[minnbreast$famid==8,],
     pedigree(id, fatherid, motherid, sex, affected=proband, relation=rel8))
plot(bped.id8)


## the 8th pedigree
bped8 <- bpeds[8]

print(bped8)
plot(bped8, cex=.5)

}

\keyword{genetics}
