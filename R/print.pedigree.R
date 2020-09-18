# Automatically generated from all.nw using noweb
#' Print method for a pedigree object
#' 
#' Print the high-level details of a pedigree S3-class object
#'
#'@param obj pedigree object with id, dadid, momid, sex
#'@param ... optional parameters for print methods
#'
#' @details
#'
#'@return 
#'@author Terry Therneau
#' @seealso \code{\link{pedigree}}
#' @name print.pedigree
NULL
#> NULL

#' @rdname print.pedigree
#' @export

print.pedigree <- function(x, ...) {
    cat("Pedigree object with", length(x$id), "subjects")
    if (!is.null(x$famid)) cat(", family id=", x$famid[1], "\n")
    else cat("\n")
    cat("Bit size=", bitSize(x)$bitSize, "\n")
    }


print.pedigreeList <- function(x, ...) {
    cat("Pedigree list with", length(x$id), "total subjects in",
        length(unique(x$famid)), "families\n")
    }
