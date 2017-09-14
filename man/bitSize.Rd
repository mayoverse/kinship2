% $Author: sinnwell $ 
% $Date: 2009/11/19 14:31:33 $ 
% $Header: /people/biostat3/sinnwell/genet/rpack/PedShrink/RCS/bitSize.Rd,v 1.2 2009/11/19 14:31:33 sinnwell Exp $ 
% $Locker:  $ 
% $Log: bitSize.Rd,v $
% Revision 1.2  2009/11/19 14:31:33  sinnwell
% *** empty log message ***
%
% Revision 1.1  2008/07/16 19:59:11  sinnwell
% Initial revision
%
% 
%
\name{bitSize}
\alias{bitSize}
\title{
  Calculate pedigree bitsize, defined as 2 * # NonFounders - # Founders
}
\description{
This is a utility function used in pedigree.shrink()
}
\usage{
bitSize(ped)
}
\arguments{
\item{ped}{
A pedigree object
}
}
\value{
A list with the following components: 
\item{bitSize}{
The bitSize of input pedigree
}
\item{nFounder}{
The number of founders in the pedigree
}
\item{nNonFounder}{
The number of nonfounders in the pedgiree
}
}
\seealso{
\code{\link{pedigree.shrink}}
}
% docclass is function
% Converted by Sd2Rd version 43268.
