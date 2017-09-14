\name{makefamid}
\alias{makefamid}
\title{Construct a family id from pedigree information}
\description{
Create a vector of length n, giving the family "tree" number of
 each subject.  If the pedigree is totally connected, then everyone will
 end up in tree 1, otherwise the tree numbers represent the disconnected
 subfamilies.  Singleton subjects give a zero for family number.
}
\usage{
makefamid(id, father.id, mother.id)
}
\arguments{
  \item{id}{Identifier for each subject in the set of pedigrees}
  \item{father.id}{
    Identifier for the father.  This will be 0 or "" for a founder.
  }
  \item{mother.id}{Identifer for the mother.}
}

\value{An integer vector giving family groupings}
\details{
  This command is depricated.  The kinship command now can be applied
  directly to pedigreeList objects.
  }
\author{Terry Therneau}
\seealso{\code{\link{makekinship}}}
\keyword{ genetics }

