# Automatically generated from all.nw using noweb

#' Plot shrinked pedigree
#'
#' @description
#' plot pedigree.shrink object that is a shrunk pedigree object
#'
#' @details
#' Plot the pedigree object that is the trimmed pedigree.shrink object along
#' with colors based on availability and affection status.
#'
#' @param x A pedigree.shrink object, which contains a pedigree object and
#' information about which subject was removed.
#' @param bigped Logical value indicating whether pedigree should be compacted
#' to fit in plotting region.  If T, then packed=F is used in pedigree() along
#' with smaller symbol sizes.
#' @param title Optional plot title
#' @param xlegend The x argument for the legend command, which allows
#' coordinates or, more conveniently, options such as "topright", "right",
#' "left", "bottomleft", etc., which is useful for pedigrees that cover most of
#' the plot region.
#' @param ...  Optional arguments to plot method
#'
#' @return Plot shrinked pedigree
#'
#' @seealso \code{\link{pedigree.shrink}},
#' @examples
#'
#' data(sample.ped)
#'
#' fam2 <- sample.ped[sample.ped$ped == 2, ]
#' ped2 <- pedigree(
#'   fam2$id, fam2$father, fam2$mother, fam2$sex,
#'   fam2$affected, fam2$avail
#' )
#'
#' shrink2 <- pedigree.shrink(ped2, avail = fam2$avail)
#' shrink2
#'
#' plot(ped2)
#'
#' plot.pedigree.shrink(shrink2, title = "Sample Pedigree 2")
#'
#' @export plot.pedigree.shrink
plot.pedigree.shrink <- function(x, bigped = FALSE, title = "", xlegend = "topright", ...) {
  ##  Plot pedigrees, coloring subjects according
  ##   to availability, shaded by affected status used in shrink

  if (bigped == FALSE) {
    tmp <- plot(x$pedObj, col = x$avail + 1, keep.par = T)
  } else {
    tmp <- plot.pedigree(x$pedObj,
      align = FALSE, packed = FALSE,
      col = x$avail + 1, cex = 0.5, symbolsize = 0.5, keep.par = T
    )
  }

  legend(
    x = xlegend, legend = c("DNA Available", "UnAvailable"),
    pch = c(1, 1), col = c(2, 1), bty = "n", cex = .5
  )
  title(paste(title, "\nbits = ", x$bitSize[length(x$bitSize)]), cex.main = .9)
  invisible(tmp)
}
