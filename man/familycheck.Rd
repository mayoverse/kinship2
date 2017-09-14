\name{familycheck}
\alias{familycheck}
\title{
  Error check for a family classification
}
\description{
Given a family id vector, also compute the familial grouping from
first principles using the parenting data, and compare the results.
}
\usage{
familycheck(famid, id, father.id, mother.id, newfam)
}
\arguments{
\item{famid}{
a vector of family identifiers
}
\item{id}{
a vector of unique subject identifiers
}
\item{father.id }{
vector containing the id of the biological father
}
\item{mother.id }{
vector containing the id of the biological mother
}
\item{newfam}{
the result of a call to \code{makefamid}.
If this has allready been computed by the user,
adding it as an argument shortens the running time somewhat.
}
}
\value{
a data frame with one row for each unique family id in the
\code{famid} argument.
Components of the output are
\item{famid}{
the family id, as entered into the data set
}
\item{n}{
number of subjects in the family
}
\item{unrelated}{
number of them that appear to be unrelated to anyone else 
in the entire pedigree set.  
This is usually marry-ins with no 
children (in the pedigree), and if so are not a problem.
}
\item{split}{
number of unique "new" family ids.
If this is 0, it means that no one in this "family" is related to
anyone else (not good); 
1 = everythings is fine;
2+= the family appears to be a set of disjoint trees.  Are you
                missing some of the people?
}
\item{join}{
number of other families that had a unique famid, but are actually
joined to this one.  0 is the hope.
If there are any joins, then an attribute "join" is attached.  It will be
a matrix with famid as row labels, new-family-id as the columns, and
the number of subjects as entries.  
}
}
\details{
The \code{makefamid} function is used to create a
de novo family id from the parentage data, and this is compared to the
family id given in the data.
}
\seealso{
\code{\link{makefamid}}, \code{\link{makekinship}}
}
\examples{
# use 2 sample peds
data(sample.ped)
pedAll <- with(sample.ped, pedigree(id, father, mother, sex,
                    affected=cbind(affected, avail), famid=ped))

## check them giving separate ped ids
fcheck.sep <- with(sample.ped, familycheck(ped, id, father, mother))
fcheck.sep

## check assigning them same ped id
fcheck.combined <- with(sample.ped, familycheck(rep(1,nrow(sample.ped)), id, father, mother))
fcheck.combined

#make person 120's father be her son.
sample.ped[20,3] <- 131
fcheck1.bad <- try({with(sample.ped, familycheck(ped, id, father, mother))}, silent=FALSE)

## fcheck1.bad is a try-error
}
\keyword{genetics}
