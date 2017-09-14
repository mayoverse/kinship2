\name{fixParents}
\alias{fixParents}
\title{
  Fix details on the parents for children of the pedigree
}
\description{
  Fix the sex of parents, add parents that are missing from the pedigree
}
\usage{
fixParents(id, dadid, momid, sex, missid = 0)
}
%- maybe also 'usage' for other objects documented here.
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
Gender of individual noted in `id'. Either character ("male","female","unknown","terminated")
or numeric (1="male", 2="female", 3="unknown", 4="terminated")
data is allowed.  For character data the string may be truncated,
and of arbitrary case.
}
\item{missid}{
  The founders are those with no father or mother in the pedigree.  The
  \code{dadid} and \code{momid} values for these subjects will either be
  NA or the value of this variable.  The default for \code{missid} is 0
  if the \code{id} variable is numeric, and "" (the empty string)
  otherwise.
}
}
\details{
First look to add parents whose ids are given in momid/dadid. Second,
fix sex of parents. Last look to add second parent for children for whom
only one parent id is given.
}
\value{
A data.frame with id, dadid, momid, sex as columns
}

\author{
Jason Sinnwell
}
\examples{
  test1char <- data.frame(id=paste("fam", 101:111, sep=""),
       sex=c("male","female")[c(1,2,1,2,1, 1,2, 2,1,2, 1)],
       father=c(0,0,"fam101","fam101","fam101", 0,0,"fam106","fam106","fam106", "fam109"),
       mother=c(0,0,"fam102","fam102","fam102", 0,0,"fam107","fam107","fam107", "fam112"))
  test1newmom <- with(test1char, fixParents(id, father, mother, sex, missid="0"))
  ## adds fam112 to the pedigree as a founder
  newped <- with(test1newmom, pedigree(id, dadid, momid, sex, missid="0"))
  as.data.frame(newped)
}