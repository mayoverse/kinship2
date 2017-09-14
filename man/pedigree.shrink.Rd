% $Author: sinnwell $ 
% $Date: 2010/08/25 20:03:43 $ 
% $Header: /people/biostat3/sinnwell/genet/rpack/PedShrink/RCS/pedigree.shrink.Rd,v 1.6 2010/08/25 20:03:43 sinnwell Exp sinnwell $ 
% $Locker: sinnwell $ 
% $Log: pedigree.shrink.Rd,v $
% Revision 1.6  2010/08/25 20:03:43  sinnwell
% clear up availability description, and fix example
%
% Revision 1.5  2010/07/28 21:45:15  sinnwell
% update examples
%
% Revision 1.4  2010/07/16 21:02:31  sinnwell
% add avail, fix items in ped
%
% Revision 1.3  2009/11/19 18:47:30  sinnwell
% args update
%
% Revision 1.2  2009/11/18 14:30:15  sinnwell
% fix example run-on line
%
% Revision 1.1  2009/11/13 22:04:56  sinnwell
% Initial revision

% 

\name{pedigree.shrink}
\alias{pedigree.shrink}
\alias{print.pedigree.shrink}
\title{
  Trim a pedigree to specified bit size with priority placed on trimming
uninformative subjects.
}
\description{
Iteratively remove subjects from the pedigree.  
First remove uninformative subjects, i.e., unavailable (not genotyped)
with no available descendants.  Next, available terminal subjects with
unknown phenotype if both parents available.
Last, iteratively shrinks pedigrees by preferentially removing:  
  1. Subjects with unknown affected status
  2. Subjects with unaffected affected status
  3. Affected subjects.
}
\usage{
pedigree.shrink(ped, avail, affected=NULL, seed=NULL, maxBits=16)
}
\arguments{
\item{ped}{
  A pedigree object with id, dadid, momid, sex, affected
}
\item{avail}{
  Vector of availability status (genotyped) 0/1 or TRUE/FALSE
}
\item{affected}{
  Vector of affected status, 0/1 or TRUE/FALSE. If not given, use the
  first column of the affected matrix of the pedigree object.
}
\item{seed}{
An integer or a saved copy of .Random.seed.  This allows simulations
to be reproduced by using the same initial seed.
}
\item{maxBits}{
Target bit size.  Pedigree will be trimmed until bit size is <= maxBits.
}
}
\value{
An object of class pedigree.shrink which is a list with the following
components:

\item{pedObj}{
pedigree object for the trimmed pedigree
}
\item{idTrimmed}{
Vector of IDs of trimmed individuals
}
\item{idList}{
List of IDs trimmed each of three stages: unavail, noninform, affect
}
\item{bitSize}{
Bit size of the pedigree at each stage of trimming
}
\item{avail}{
vector of availability for members of the trimmed pedigree
}
\item{pedSizeOriginal}{
The number of individuals in the input pedigree
}
\item{pedSizeIntermed}{
The number of individuals in the pedigree after removing unavailable
subjects and before beginning the itertive shrinking.
}
\item{pedSizeFinal}{
The number of individuals in the final pedigree
}
\item{seed}{
The random seed used
}
}
\details{
  If the subject ids are character, make sure none of the characters
  in the ids is a colon (":"), which is a special character used to
  concatenate and split subjects within the findUnavailable utility
  within the pedigree.shrink function. 
}
\seealso{
\code{\link{findUnavailable}},
\code{\link{findAvailNonInform}},
\code{\link{plot.pedigree.shrink}}
}
\examples{
data(sample.ped)

fam1 <- sample.ped[sample.ped$ped==1,]
ped1 <- pedigree(fam1$id, fam1$father, fam1$mother, fam1$sex,
                 fam1$affected)

shrink1 <- pedigree.shrink(ped=ped1, avail=fam1$avail, maxBits=25)

print(shrink1)
## plot(ped1)
## plot(shrink1, title="Sample Pedigree 1")


fam2 <- sample.ped[sample.ped$ped==2,]
ped2 <- pedigree(fam2$id, fam2$father, fam2$mother, fam2$sex,
                 fam2$affected)

shrink2 <- pedigree.shrink(ped2, avail=fam2$avail)

## plot(ped2)
## plot(shrink2, title="Sample Pedigree 2")
print(shrink2)


}
\keyword{pedigree}
% docclass is function
% Converted by Sd2Rd version 43268.
