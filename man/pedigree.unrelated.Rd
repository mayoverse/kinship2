% $Author: sinnwell $ 
% $Date: 2009/11/10 19:22:04 $ 
% $Header: /people/biostat3/sinnwell/genet/rpack/MGenet/RCS/pedigree.unrelated.Rd,v 1.1 2009/11/10 19:22:04 sinnwell Exp $
% $Locker:  $ 
% $Log: pedigree.unrelated.Rd,v $
% Revision 1.1  2009/11/10 19:22:04  sinnwell
% Initial revision
%
% Revision 1.1  2009/11/03 16:41:53  sinnwell
% Initial revision
%
%
%

\name{pedigree.unrelated}
\alias{pedigree.unrelated}
\title{Determine set of maximum number of unrelated available subjects
from a pedigree
}
\description{Determine set of maximum number of unrelated available subjects
from a pedigree, given vectors id, father, and mother for a pedigree structure,
and status vector of T/F for whether each subject is available (e.g. has DNA)
}
\usage{
pedigree.unrelated(ped, avail)
}
\arguments{
\item{ped}{
A pedigree objects with unique id, father index, mother index
}
\item{avail}{
  Vector of availability status (e.g., genotyped) 0/1 or TRUE/FALSE
}
}

\value{
A vector of the ids of subjects that are unrelated.
}
\details{
  This is a greedy algorithm that uses the kinship matrix, sequentially
  removing rows/cols that are non-zero for subjects that have the most
  number of zero kinship coefficients (greedy by choosing a row of
  kinship matrix that has the most number of zeros, and then remove any
  cols and their corresponding rows that are non-zero. To account for
  ties of the count of zeros for rows, a random choice is made. Hence,
  running this function multiple times can return different sets of
  unrelated subjects.
}
\seealso{
\code{\link{kinship}},
\code{\link{pedigree}}
}
\examples{
data(sample.ped)
fam1 <- sample.ped[sample.ped$ped==1,]


ped1 <- pedigree(fam1$id, fam1$father, fam1$mother,
                 fam1$sex, fam1$affected, fam1$avail)

## to see plot:
## plot.pedigree(ped1, align=FALSE)
id1 <- pedigree.unrelated(ped1, avail=fam1$avail)

id1
## some possible vectors
##[1] "110" "113" "133" "109"
##[1] "113" "118" "141" "109" 
##[1] "113" "118" "140" "109" 
##[1] "110" "113" "116" "109" 
##[1] "113" "133" "141" "109" 

 
fam2 <- sample.ped[sample.ped$ped==2,]

ped2 <- pedigree(fam2$id, fam2$father, fam2$mother,
               fam2$sex, fam2$affected, fam2$avail)

## to see plot:
## plot.pedigree(ped2, align=FALSE)

id2 <- pedigree.unrelated(ped2, avail=fam2$avail)

## some possible vectors
##[1] "203" "207"
##[1] "203" "204"
##[1] "201" "203"
##[1] "214" "203"
id2

}
% docclass is function

