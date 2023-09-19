## Created: 2/24/2020 Author: Jason Sinnwell

#' The kinship2 package for pedigree data
#'
#' The kinship2 package for pedigree data
#'
#' The package download, NEWS, and README are available on CRAN:
#' \\url{https://cran.r-project.org/package=kinship2}
#'
#' @section Functions:
#' Below are listed some of the most widely used functions available
#' in arsenal:
#'
#' [pedigree()]: Contstructor of the pedigree class,
#' given identifiers, sex, affection status(es), and special relationships
#'
#' [kinship()]: Calculates the kinship matrix, the
#' probability having an allele sampled from two individuals
#' be the same via IBD.
#'
#' [ped_to_plotdf()] : Method to transform a pedigree
#' object into a dataframe of graphical elements.
#' Allows extra information to be included in the id under the plot symbol
#'
#' [plot_fromdf()] : Method to plot a pedigree from a
#' dataframe of graphical elements.
#'
#' [shrink()]: Shrink a pedigree to a specific bit size,
#' removing non-informative members first.
#'
#' [bit_size()]: Approximate the output from SAS's
#' `PROC FREQ` procedure when using the `/list`
#' option of the `TABLE` statement.
#'
#' @section Data:
#' - [sampleped()]: Pedigree example data sets
#' with two pedigrees
#' - [minnbreast()]: Larger cohort of pedigrees
#' from MN breast cancer study
#'
#' @examples
#' library(kinship2)
#'
#' @docType package
#' @rdname kinship2
#' @keywords internal
"_PACKAGE"
