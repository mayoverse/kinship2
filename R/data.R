#' Minnesota Breast Cancer Study
#'
#' @description Data from the Minnesota Breast Cancer Family Study.
#' This contains extended pedigrees from 426 families, each identified by
#' a single proband in 1945-1952, with follow up for incident breast cancer.
#'
#' @details The original study was conducted by Dr. Elving Anderson at the
#' Dight Institute for Human Genetics at the University of Minnesota.
#' From 1944 to 1952, 544 sequential breast cancer cases seen at the
#' University Hospital were enrolled, and information gathered on parents,
#' siblings, offspring, aunts / uncles, and grandparents with the goal of
#' understanding possible familial aspects of brest cancer. In 1991 the
#' study was resurrected by Dr Tom Sellers.
#'
#' Of the original 544 he excluded 58 prevalent cases, along with another 19
#' who had less than 2 living relatives at the time of Dr Anderson's survey.
#' Of the remaining 462 families 10 had no living members, 23 could not be
#' located and 8 refused, leaving 426 families on whom updated pedigrees
#' were obtained.
#'
#' This gave a study with 13351 males and 12699 females (5183 marry-ins).
#' Primary questions were the relationship of early life exposures, breast
#' density, and pharmacogenomics on incident breast cancer risk.
#' For a subset of the families data was gathered on prostate cancer risk for
#' male subjects via questionnaires sent to men over 40. Other than this, data
#' items other than parentage are limited to the female subjects.
#' In 2003 a second phase of the study was instituted. The pedigrees were
#' further extended to the numbers found in this data set, and further data
#' gathered by questionnaire.
#'
#' @format A data frame with 28081 observations, one line per subject, on the
#' following 14 variables.
#'
#' - `id` : Subject identifier
#' - `proband` : If 1, this subject is one of the original
#' 426 probands
#' - `fatherid` : Identifier of the father, if the father is part of
#' the data set; zero otherwise
#' - `motherid` : Identifier of the mother, if the mother is part of
#' the data set; zero otherwise
#' - `famid` : Family identifier
#' - `endage` : Age at last follow-up or incident cancer
#' - `cancer` : `1` = breast cancer (females) or prostate cancer (males),
#' `0` = censored
#' - `yob` : Year of birth
#' - `education` : Amount of education: 1-8 years, 9-12 years, high
#' school graduate, vocational education beyond high school,
#' some college but did not graduate, college graduate,
#' post-graduate education, refused to answer on the questionnaire
#' - `marstat` : Marital status: married, living with someone in a
#' marriage-like relationship, separated
#' or divorced, widowed, never married, refused to answer the questionaire
#' - `everpreg` : Ever pregnant at the time of baseline survey
#' - `parity` : Number of births
#' - `nbreast` : Number of breast biopsies
#' - `sex` : `M` or `F`
#' - `bcpc` : Part of one of the families in the breast / prostate
#' cancer substudy: `0` = no, `1` = yes.
#' Note that subjects who were recruited to the overall study after the date of
#' the BP substudy are coded as zero.
#'
#'
#' @usage
#' data(minnbreast)
#'
#' @references
#' Epidemiologic and genetic follow-up study of 544 Minnesota breast cancer
#' families: design and methods.
#' Sellers TA, Anderson VE, Potter JD, Bartow SA, Chen PL, Everson L, King RA,
#' Kuni CC, Kushi LH, McGovern PG, et al.
#' Genetic Epidemiology, 1995; 12(4):417-29.
#'
#' Evaluation of familial clustering of breast and prostate cancer in the
#' Minnesota Breast Cancer Family Study.
#' Grabrick DM, Cerhan JR, Vierkant RA, Therneau TM, Cheville JC, Tindall DJ,
#' Sellers TA.
#' Cancer Detect Prev. 2003; 27(1):30-6.
#'
#' Risk of breast cancer with oral contraceptive use in women with a family
#' history of breast cancer.
#' Grabrick DM, Hartmann LC, Cerhan JR, Vierkant RA, Therneau TM, Vachon CM,
#' Olson JE, Couch FJ, Anderson KE, Pankratz VS, Sellers TA.
#' JAMA. 2000; 284(14):1791-8.
#'
#' @examples
#' data(minnbreast)
#' breastped <- Pedigree(minnbreast,
#'    cols_ren_ped = list(
#'        "indId" = "id", "fatherId" = "fatherid",
#'        "motherId" = "motherid", "gender" = "sex", "family" = "famid"
#'    ), missid = "0", col_aff = "cancer"
#' )
#' summary(breastped)
#' scales(breastped)
#' #plot family 8, proband is solid, slash for cancers
#' #plot(breastped[famid(breastped) == "8"])
"minnbreast"

#' Sampleped data
#'
#' @description Small sample pedigree data set for testing purposes.
#'
#' @details This is a small fictive pedigree data set, with 55
#' individuals in 2 families.
#' The aim was to create a data set with a variety of pedigree structures.
#'
#' @format A data frame with 55 observations, one line per subject, on the
#' following 7 variables.
#' - `famid` : Family identifier
#' - `id` : Subject identifier
#' - `dadid` : Identifier of the father, if the father is part of the
#' data set; zero otherwise
#' - `momid` : Identifier of the mother, if the mother is part of the
#' data set; zero otherwise
#' - `sex` : `1` for male or `2` for female
#' - `affected` : `1` or `0`
#' - `avail` : `1` or `0`
#'
#' @usage
#' data("sampleped")
#'
#' @examples
#' data("sampleped")
#' ped <- Pedigree(sampleped)
#' summary(ped)
#' #plot(ped)
"sampleped"
