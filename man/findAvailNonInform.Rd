\name{findAvailNonInform}
\alias{findAvailNonInform}
\title{
  Find subjects from a pedigree who are available and uninformative 
}
\description{
Identify subjects to remove from a pedigree who are available but
non-informative.  This is the second step to remove subjects in
pedigree.shrink if the pedigree does not meet the desired bit size.
}
\usage{
findAvailNonInform(ped, avail)
}
\arguments{
  \item{ped}{
    A pedigree object
  }
  \item{avail}{
    Vector of availability status (e.g. genotyped) 0/1 or TRUE/FALSE
  }
}
\value{
Vector of subject ids who can be removed by having lowest informativeness.
}
\seealso{
\code{\link{pedigree.shrink}}
}
