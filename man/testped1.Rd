 
\name{testped1}
\alias{testped1}
\docType{data}
\title{ testped1}
\description{
  Sample pedigree 
}
\usage{data(testped1)}
\format{
  A data frame with 79 subjects in one family with the following variables.
  \describe{
    \item{\code{id}}{ subject id, unique to each ped}
    \item{\code{father}}{id of the subject's father}
    \item{\code{mother}}{id of the subject's mother}
    \item{\code{sex}}{ 1=male, 2=female}   
  }
}

\examples{
data(testped1)
testped1[1:20,]

}
\keyword{datasets}
