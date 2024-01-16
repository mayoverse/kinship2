## Created: 2/24/2020 Author: Jason Sinnwell

#' The Pedixplorer package for pedigree data
#'
#' The Pedixplorer package for pedigree data an updated package of the
#' \code{kinship2} package.
#' The \code{kinship2} package was originally
#' written by Terry Therneau and Jason Sinnwell.
#' The \code{Pedixplorer} package is a fork of the
#' \code{kinship2} package with
#' additional functionality and bug fixes.
#'
#' The package download, NEWS, and README are available on CRAN:
#' \\url{https://cran.r-project.org/package=kinship2} for the
#' previous version of the package.
#'
#' @section Functions:
#' Below are listed some of the most widely used functions available
#' in arsenal:
#'
#' [Pedigree()]: Contstructor of the Pedigree class,
#' given identifiers, sex, affection status(es), and special relationships
#'
#' [kinship()]: Calculates the kinship matrix, the
#' probability having an allele sampled from two individuals
#' be the same via IBD.
#'
#' [Pedixplorer::plot()] : Method to transform a Pedigree
#' object into a graphical plot.
#' Allows extra information to be included in the id under the
#' plot symbol.
#' This method use the [plot_fromdf()] function to transform the Pedigree
#' object into a data frame of graphical elements, the same is done for the
#' legend with the [ped_to_legdf()] function.
#' When done, the data frames are plotted with the [plot_fromdf()] function.
#'
#' [shrink()]: Shrink a Pedigree to a specific bit size,
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
#' library(Pedixplorer)
#'
#' @docType package
#' @rdname Pedixplorer_package
"_PACKAGE"
