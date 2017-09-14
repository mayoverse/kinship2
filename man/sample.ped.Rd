%-$Author: sinnwell $
%- $Date: 2009/11/13 21:33:47 $
%- $Header: /people/biostat3/sinnwell/genet/rpack/PedShrink/RCS/sample.ped.Rd,v 1.1 2009/11/13 21:33:47 sinnwell Exp $
%- $Locker:  $
% 
% $Log: sample.ped.Rd,v $
% Revision 1.1  2009/11/13 21:33:47  sinnwell
% Initial revision
%
% 
\name{sample.ped}
\alias{sample.ped}
\docType{data}
\title{ Two example pedigrees }
\description{
  Two pedigrees of different size for testing pedigree operations
}
\usage{data(sample.ped)}
\format{
  A data frame with 55 observations on the following 7 variables.
  \describe{
    \item{\code{ped}}{ pedigree id}
    \item{\code{id}}{ subject id, unique to each ped}
    \item{\code{father}}{id of the subject's father}
    \item{\code{mother}}{id of the subject's mother}
    \item{\code{sex}}{ 1=male, 2=female, 3=unknown}
    \item{\code{affected}}{affection status; 0=unaffected, 1=affected, NA=unknown}
    \item{\code{avail}}{ is DNA data available; 0=unavailable, 1=available}
  }
}

\examples{
data(sample.ped)
}
\keyword{datasets}
