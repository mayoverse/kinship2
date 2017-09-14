\name{makekinship}
\alias{makekinship}
\title{
  Create a sparse kinship matrix
}
\description{
Compute the overall kinship matrix for a collection of families, and
store it efficiently.
}
\usage{
makekinship(famid, id, father.id, mother.id, unrelated=0)
}
\arguments{
\item{famid}{
a vector of family identifiers
}
\item{id}{
a vector of unique subject identifiers
}
\item{father.id }{
for each subject, the identifier of their biolgical father
}
\item{mother.id }{
for each subject, the identifier of thier biological mother
}
\item{unrelated}{
subjects with this family id are considered to be unrelated singletons,
i.e., not related to each other or to anyone else.
}
}
\value{
a sparse kinship matrix of class \code{bdsmatrix}
}
\details{
  This command is depricated.  The kinship command now can be applied
  directly to pedigreeList objects.
  }

\seealso{
 \code{\link{kinship}}, \code{\link{makefamid}}
}
\examples{
# Data set from a large family study of breast cancer
#  there are 26050 subjects in the file, from 426 families
\dontrun{
> table(cdata$sex)
     F     M 
 12699 13351
> length(unique(cdata$famid))
[1] 426

> kin1 <- makekinship(cdata$famid, cdata$gid, cdata$dadid, cdata$momid)
> dim(kin1)
[1] 26050 26050
> class(kin1)
[1] "bdsmatrix"
# The next line shows that few of the elements of the full matrix are >0
> length(kin1@blocks)/ prod(dim(kin1))
[1] 0.00164925

# kinship matrix for the females only
> femid <- cdata$gid[cdata$sex=='F']
> femindex <- !is.na(match(dimnames(kin1)[[1]], femid))
> kin2 <- kin1[femindex, femindex]
#
# Note that "femindex <- match(femid, dimnames(kin1)[[1]])" is wrong, since
#  then kin1[femindex, femindex] might improperly reorder the rows/cols 
#  (if families were not contiguous in cdata).  
# However sort(match(femid, dimnames(kin1)[[1]])) would be okay.
}}
\keyword{genetics}

