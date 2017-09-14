\name{kinship}
\alias{kinship}
\alias{kinship.default}
\alias{kinship.pedigree}
\alias{kinship.pedigreeList}
\title{
  Compute a kinship matrix
}
\description{
  Compute the kinship matrix for a set of related autosomal subjects.
  The function is generic, and can accept a pedigree, pedigreeList,
  or vector as the first argument.
}
\usage{
kinship(id, \dots)
\method{kinship}{pedigree}(id, chrtype="autosome", \dots)
\method{kinship}{pedigreeList}(id, chrtype="autosome", \dots)
\method{kinship}{default}(id, dadid, momid, sex, chrtype="autosome", \dots)
}
\arguments{
\item{id}{
  either a pedigree object, pedigreeList object, or a vector of
  subject identifiers.  Subject identifiers may be numeric or
  character.  
}
\item{dadid }{
  for each subject, the identifier of the biological father.
  This is only used if \code{id} is a vector.
}
\item{momid }{
  for each subject, the identifier of the biological mother.
  This is only used if \code{id} is a vector.
}
\item{sex}{vector of sex values coded as 1=male, 2=female}

\item{chrtype}{chromosome type.  The currently supported types
  are "autosome" and "X" or "x".  }
\item{\dots}{Any number of optional arguments} 
}
\value{
a matrix of kinship coefficients.
}
\details{
  The function will usually be called with a pedigree or pedigreeList;
  the third form is provided for backwards compatability with an earlier
  release of the library that was less capable.
  The first argument is named \code{id}
  for the same reason.  Note that when using the third form any
  information on twins is not available to the function.

  When called with a pedigreeList, i.e., with multiple families, the
  routine will create a block-diagonal-symmetric sparse matrix object
  of class \code{dsCMatrix}.
  Since the [i,j] value of the result is 0 for any two
  unrelated individuals i and j and a \code{Matrix} utilizes sparse
  representation, the resulting object is often orders of magnitude
  smaller than an ordinary matrix.
  When \code{kinship} is called with a single pedigree
  an ordinary matrix is returned.
  
Two genes G1 and G2 are identical by descent (IBD) if they are both physical
copies of the same ancestral gene; two genes are identical by state if
they represent the same allele.  
So the brown eye gene that I inherited from my mother is IBD with hers;
the same gene in an unrelated individual is not.

The kinship coefficient between two subjects is the probability that a
randomly selected allele from a locus will be IBD between them.
It is obviously 0 between unrelated individuals.
For an autosomal site and no inbreeding
it will be 0.5 for an individual
with themselves, .25 between mother and child, .125 between an uncle and
neice, etc.

The computation is based on a recursive algorithm described in Lange,
which assumes that the founder alleles are all independent.
}
\section{References}{
K Lange, Mathematical and Statistical Methods for Genetic Analysis,
Springer-Verlag, New York, 1997.
}
\seealso{
\code{\link{pedigree}}, \code{\link{makekinship}},\code{\link{makefamid}}
}
\examples{
test1 <- data.frame(id  =c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14),
                    mom =c(0, 0, 0, 0, 2, 2, 4, 4, 6,  2,  0,  0, 12, 13),
                    dad =c(0, 0, 0, 0, 1, 1, 3, 3, 3,  7,  0,  0, 11, 10),
                    sex =c(0, 1, 0, 1, 0, 1, 0, 1, 0,  0,  0,  1,  1,  1))
tped <- with(test1, pedigree(id, dad, mom, sex))
round(8*kinship(tped))

}
\keyword{genetics}
% docclass is function

