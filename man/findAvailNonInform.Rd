% $Author: sinnwell $ 
% $Date: 2009/11/19 14:32:52 $ 
% $Header: /people/biostat3/sinnwell/genet/rpack/PedShrink/RCS/findAvailNonInform.Rd,v 1.2 2009/11/19 14:32:52 sinnwell Exp $ 
% $Locker:  $ 
% $Log: findAvailNonInform.Rd,v $
% Revision 1.2  2009/11/19 14:32:52  sinnwell
% *** empty log message ***
%
% Revision 1.1  2008/07/16 19:59:42  sinnwell
% Initial revision
%
% 
%

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
Vector of subject ids
}
\seealso{
\code{\link{pedigree.shrink}}
}
