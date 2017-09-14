% $Author: sinnwell $
% $Date: 2010/02/08 18:22:58 $
% $Header: /people/biostat3/sinnwell/genet/rpack/PedShrink/RCS/printBanner.Rd,v 1.4 2010/02/08 18:22:58 sinnwell Exp $
% $Id: printBanner.Rd,v 1.4 2010/02/08 18:22:58 sinnwell Exp $
% $Locker:  $
% $Log: printBanner.Rd,v $
% Revision 1.4  2010/02/08 18:22:58  sinnwell
% rm empty sections
%
% Revision 1.3  2008/05/09 15:03:01  sinnwell
% add to description
%
% Revision 1.2  2008/01/08 20:29:41  sinnwell
% fix comments
%
% Revision 1.1  2008/01/08 18:26:06  sinnwell
% Initial revision
%
%Revision 1.4  2007/12/21 17:21:07  sinnwell
%add description
%
%Revision 1.3  2005/02/04 20:57:46  sinnwell
%banner.width and char.perline comply to options()$width
%
%Revision 1.2  2004/06/29 19:54:56  sinnwell
%now works in both R and Splus
%
%Revision 1.1  2004/02/26 21:35:08  sinnwell
%Initial revision
%
%Revision 1.2  2003/01/20 23:21:06  sinnwell
%fixed RCS keywords

\name{printBanner}
\alias{printBanner}
\title{
  Print a nice banner
}
\description{
Print a nice banner with a border above and below the text.  It centers
the text, and adjusts to the width system option by breaking into
multiple lines when needed.
}
\usage{
printBanner(str, banner.width=options()$width, char.perline=.75*banner.width, border="=")
}
\arguments{
\item{str}{
character string - a title within the banner
}
\item{banner.width }{
width of banner, the default is set to fit current options
}
\item{char.perline }{
number of characters per line for the title, the default is 75\% of 
the banner.width parameter 
}
\item{border}{
type of character for the border
}
}
\value{
  nothing is returned
}
\details{
This function prints a nice banner in both R and S-PLUS
}
\seealso{
options
}
\examples{
printBanner("This is a pretty banner", banner.width=40, char.perline=30)

# the output looks like this:
# ========================================
#         This is a pretty banner
# ========================================
}
% docclass is function
% Converted by Sd2Rd version 37351.
