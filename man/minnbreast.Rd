\name{minnbreast}
\alias{minnbreast}
\alias{minnbreast2}
\docType{data}
\title{Minnesota Breast Cancer Study}
\description{
  Data from the Minnesota Breast Cancer Family Study.  This contains
  extended pedigrees from 426 families, each identified by a single
  proband in  1945-52, with follow up for incident breast cancer.
}
\usage{data(minnbreast)}
\format{
  minnbreast: A data frame with 28081 observations, one line per subject,
  on the following 14 variables. 
  \describe{
    \item{\code{id}}{subject identifier}
    \item{\code{proband}}{if 1, this subject is one of the original 426 probands}
    \item{\code{fatherid}}{identifier of the father, if the father is
      part of the data set; zero otherwise}
    \item{\code{motherid}}{identifier of the mother, if the mother is
      part of the data set; zero otherwise}
    \item{\code{famid}}{family identifier}
    \item{\code{endage}}{age at last follow-up or incident cancer}
    \item{\code{cancer}}{1= breast cancer (females) or prostate cancer
      (males), 0=censored}
    \item{\code{yob}}{year of birth}
    \item{\code{education}}{amount of education: 1-8 years,
      9-12 years, high school graduate,  vocational education beyond high
      school, some college but did not graduate,  college graduate,
      post-graduate education, refused to answer on the questionairre}
    \item{\code{marstat}}{marital status: married, living with someone
      in a marriage-like relationship, separated or divorced,  widowed,
      never married, refused to answer the questionaiire}
    \item{\code{everpreg}}{ever pregnant: never pregnant at the time of
      baseline survey, ever pregnant at the time of baseline survey}
    \item{\code{parity}}{number of births}
    \item{\code{nbreast}}{number of breast biopsies}
    \item{sex}{M or F}
    \item{bcpc}{part of one of the families in the breast/prostate
      cancer substudy: 0=no, 1=yes.  Note that subjects who were recruited
      to the overall study after the date of the BP substudy are coded as zero.}
  }
}

\details{
  The original study was conducted by Dr. Elving Anderson at the Dight
  Institute for Human Genetics at the University of Minnesota.  From 1944
  to 1952, 544 sequential breast cancer cases seen at the University
  Hospital were enrolled, and information gathered on parents, siblings,
  offspring, aunts/uncles, and grandparents with the goal of
  understanding possible familial aspects of brest cancer.  In 1991 the
  study was resurrected by Dr. Tom Sellers.  Of the original 544 he
  excluded 58 prevalent cases, along with another 19 who had less than 2
  living relatives at the time of Dr Anderson's survey.  Of the
  remaining 462 families 10 had no living members, 23 could not be
  located and 8 refused, leaving 426 families on whom updated pedigrees
  were obtained.  This gave a study with 13351 males and 12699 females
  (5183 marry-ins).  Primary questions were the relationship of early
  life exposures, breast density, and pharmacogenomics on incident
  breast cancer risk.

  For a subset of the families data was gathered on prostate cancer risk
  for male subjects via questionairres sent to men over 40.  Other than
  this, data items other than parentage are limited to the female subjects.

  In ___ a second phase of the study was instituted.  The pedigrees were
  further extended to the numbers found in this data set, and further
  data gathered by questionairre.
}
\source{Authors of the study}

\references{
  Epidemiologic and genetic follow-up study of 544 Minnesota breast
  cancer families: design and methods.  Sellers TA, Anderson VE, Potter
  JD, Bartow SA, Chen PL, Everson L, King RA, Kuni CC, Kushi LH,
  McGovern PG, et al.  Genetic Epidemiology, 1995; 12(4):417-29.

  Evaluation of familial clustering of breast and prostate cancer in the
  Minnesota Breast Cancer Family Study.
  Grabrick DM, Cerhan JR, Vierkant RA, Therneau TM, Cheville JC, Tindall
  DJ, Sellers TA. Cancer Detect Prev. 2003; 27(1):30-6.

  Risk of breast cancer with oral contraceptive use in women with a
  family history of breast cancer.  Grabrick DM, Hartmann LC, Cerhan JR,
  Vierkant RA, Therneau TM, Vachon CM, Olson JE, Couch FJ, Anderson KE,
  Pankratz VS, Sellers TA.  JAMA. 2000; 284(14):1791-8.
}
\examples{
data(minnbreast)
breastped <- with(minnbreast, pedigree(id, fatherid, motherid, sex,
                  status=(cancer& !is.na(cancer)), affected=proband,
                   famid=famid))
print(breastped["8"])
print(breastped[8])
#plot(breastped["8"])  #plot family 8, proband is solid, slash for cancers
#Note that breastped[8] is a different family, since ids are not 1,2,3,...
}
\keyword{datasets}
