
## Created: 2/24/2020
## Author: Jason Sinnwell

#' The kinship2 package for pedigree data
#'
#' The kinship2 package for pedigree data
#'
#' The package download, NEWS, and README are available on CRAN: \url{https://cran.r-project.org/package=kinship2}
#'
#' @section Functions:
#'
#' Below are listed some of the most widely used functions available in \code{arsenal}:
#'
#' \code{\link{pedigree}}: Contstructor of the pedigree class, given identifiers, sex, affection status(es), and special relationships
#'
#' \code{\link{kinship}}: Calculates the kinship matrix, the probability having an allele sampled from two individuals be the same via IBD.
#'
#' \code{\link{plot.pedigree}}: Plot method for a pedigree object. Allows extra information to be included in the id under the plot symbol
#'
#' \code{\link{legendPlot}}:  Special version of plot.pedigree, with an informative legend along the bottom of the figure
#'
#' \code{\link{pedigree.shrink}}: Shrink a pedigree to a specific bit size, removing non-informative members first.
#'
#' \code{\link{bitSize}}: Approximate the output from SAS's \code{PROC FREQ} procedure when using
#'  the \code{/list} option of the \code{TABLE} statement.
#'
#' @section Data:
#'
#' \code{\link{example.ped}}: Pedigree example data sets with two pedigrees
#' 
#' \code{\link{minnbreast}}: Larger cohort of pedigrees from MN breast cancer study
#'
#' @examples
#' library(kinship2)
#'
#' @docType package
#' @name kinship2
#'
NULL

#' @importFrom utils head
#' @seealso \code{\link[utils]{head}}
#' @export
utils::head

#' @importFrom utils tail
#' @seealso \code{\link[utils]{tail}}
#' @export
utils::tail

#### commands to build the package using devtools
# devtools::check_man()
# devtools::test()
# devtools::check()
# withr::with_libpaths(c("../testinstalls/", .libPaths()), devtools::install(build_vignettes = TRUE, dependencies = FALSE))
# devtools::build("../kinship2/")
## < restart R >
## library(kinship2, lib.loc = "../testinstalls/")

#### to upload to CRAN
## Update DESCRIPTION, README.md, NEWS.md, and cran-comments.md
# devtools::revdep_check()
# devtools::release()