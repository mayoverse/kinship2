% $Author: sinnwell $ 
% $Date: 2009/11/19 18:52:33 $ 
% $Header: /people/biostat3/sinnwell/genet/rpack/PedShrink/RCS/pedTrimOneSubj.Rd,v 1.6 2009/11/19 18:52:33 sinnwell Exp $ 
% $Locker:  $ 
% $Log: pedTrimOneSubj.Rd,v $
% Revision 1.6  2009/11/19 18:52:33  sinnwell
% *** empty log message ***
%
% Revision 1.5  2009/11/19 14:47:40  sinnwell
% *** empty log message ***
%
% Revision 1.4  2009/11/19 14:43:22  sinnwell
% *** empty log message ***
%
% Revision 1.3  2009/11/19 14:31:41  sinnwell
% *** empty log message ***
%
% Revision 1.2  2009/11/18 14:54:27  sinnwell
% add removeSubj
%
% Revision 1.1  2008/07/16 20:00:19  sinnwell
% Initial revision
%
% 
%
\name{findAvailAffected}
\alias{findAvailAffected}
\title{
  Find a single person to trim from a pedigree whose is available
}
\description{
Finds one subject from among available non-parents with indicated
affection status
}
\usage{
findAvailAffected(ped, avail, affstatus)
}
\arguments{
\item{ped}{
A pedigree objects, with id (subject ID),
findex (father index), mindex (mother index)
}
\item{avail}{
 Vector of availability status (e.g., genotyped) 0/1 or TRUE/FALSE
}
\item{affstatus}{
  Vector of affection status 0/1 or TRUE/FALSE.
}
}
\value{
A list is returned with the following components
  
\item{ped}{
Dataframe with trimmed subject removed
}
\item{idTrimmed}{
Vector of IDs of trimmed individuals
}
\item{isTrimmed}{
logical value indicating whether pedigree has been trimmed
}
\item{bitSize}{
Bit size of the trimmed pedigree
}
}
\details{
  When used within pedigree.shrink, this function is called with the
  first affected indicator, if the affected item in the pedigree is a
  matrix of multiple affected indicators.  
}
\seealso{
\code{\link{pedigree.shrink}}
}
% docclass is function
% Converted by Sd2Rd version 43268.
