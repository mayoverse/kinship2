## Created: 2/24/2020 Author: Jason Sinnwell

#' The kinship2 package for pedigree data
#'
#' The kinship2 package for pedigree data
#'
#' The package download, NEWS, and README are available on CRAN:
#' \\url{https://cran.r-project.org/package=kinship2}
#'
#' @section Functions:
#' Below are listed some of the most widely used functions available in \\code{arsenal}:
#'
#' \\code{\\link{pedigree}}: Contstructor of the pedigree class,
#' given identifiers, sex, affection status(es), and special relationships
#'
#' \\code{\\link{kinship}}: Calculates the kinship matrix, the probability having
#' an allele sampled from two individuals be the same via IBD.
#'
#' \\code{\\link{plot.pedigree}}: Plot method for a pedigree object.
#' Allows extra information to be included in the id under the plot symbol
#'
#' \\code{\\link{legendPlot}}:  Special version of plot.pedigree, with
#' an informativelegend along the bottom of the figure
#'
#' \\code{\\link{pedigree.shrink}}: Shrink a pedigree to a specific bit size,
#' removing non-informative members first.
#'
#' \\code{\\link{bit_size}}: Approximate the output from SAS's \\code{PROC FREQ}
#' procedure when using the \\code{/list} option of the \\code{TABLE} statement.
#'
#' @section Data:
#' \\code{\\link{sample.ped}}: Pedigree example data sets with two pedigrees
#' \\code{\\link{minnbreast}}: Larger cohort of pedigrees from MN breast cancer study
#'
#' @examples
#' library(kinship2)
#'
#' @docType package
#' @name kinship2
#'
TRUE
