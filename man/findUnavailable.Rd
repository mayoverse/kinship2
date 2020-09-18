\name{findUnavailable}
\alias{findUnavailable}
\alias{pedigree.trim}
\alias{excludeUnavailFounders}
\alias{excludeStrayMarryin}
\title{
  Find or trim unavailable subjects in a pedigree
}
\description{
  Find the ID of subjects in a pedigree iteratively, as anyone who is
  not available and does not have an available descendant by
  successively removing unavailable terminal nodes. pedigree.trim
  carries out the removal of the subjects identified by findUnavailable.
}
\usage{
findUnavailable(ped, avail)
pedigree.trim(removeID, ped)
}
\arguments{
\item{ped}{
A pedigree object with an id, findex, mindex, sex, plus other optional items
}
\item{avail}{
  Logical vector of availability status (e.g., genotyped) 0/1 or TRUE/FALSE
}
\item{removeID}{
vector of subject ids of persons to trim from a pedigree
}
}
\value{
  findUnavailable returns a vector of subject ids for who can be
  removed. pedigree.trim returns a trimmed pedigree object.  
}
\section{Side Effects}{
  relation matrix from pedigree.trim is trimmed of any special relations
  that include the subjects to trim.
}
\details{
  Originally written as pedTrim by Steve Iturria, modified by Dan Schaid
  2007, and now split into the two separate functions: \code{findUnavailable()},
  and \code{pedigree.trim()} to do the tasks separately.  \code{findUnavailable()}
  calls \code{excludeStrayMarryin} to find stray
  available marry-ins who are isolated after trimming their unavailable
  offspring, and excludeUnavailFounders.
  If the subject ids are character, make sure none of the characters
  in the ids is a colon (":"), which is a special character used to
  concatenate and split subjects within the utlity.
  
}
\seealso{
\code{\link{pedigree.shrink}},
}
% docclass is function
% Converted by Sd2Rd version 43268.
