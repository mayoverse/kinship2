# Automatically generated from all.nw using noweb
#' Print method for a pedigree.shrink object
#' 
#' Print the high-level details of a pedigree.shrink S3-class object
#'
#' @param x pedigree.shrink object 
#' @param ... optional parameters for print methods
#'
#' @details
#'
#' @return 
#' @author Jason Sinnwell, Daniel Schaid
#' @seealso \code{\link{pedigree.shrink}}
#' @name print.pedigree.shrink
NULL
#> NULL

#' @rdname print.pedigree.shrink
#' @export

print.pedigree.shrink <- function(x, ...){

    cat("Pedigree Size:\n")

    if(length(x$idTrimmed) > 2)
    {
        n <- c(x$pedSizeOriginal, x$pedSizeIntermed, x$pedSizeFinal)
        b <- c(x$bitSize[1], x$bitSize[2], x$bitSize[length(x$bitSize)])
        row.nms <- c("Original","Only Informative","Trimmed")
    } else {
        n <- c(x$pedSizeOriginal, x$pedSizeIntermed)
        b <- c(x$bitSize[1], x$bitSize[2])
        row.nms <- c("Original","Trimmed")
    }
    
    df <- data.frame(N.subj = n, Bits = b)
    rownames(df) <- row.nms
    print(df, quote=FALSE)
    
    if(!is.null(x$idList$unavail)) 
        cat("\n Unavailable subjects trimmed:\n", x$idList$unavail, "\n")
    
    if(!is.null(x$idList$noninform)) 
        cat("\n Non-informative subjects trimmed:\n", x$idList$noninform, "\n")
    
    if(!is.null(x$idList$affect)) 
        cat("\n Informative subjects trimmed:\n", x$idList$affect, "\n")
    
    invisible()
}

