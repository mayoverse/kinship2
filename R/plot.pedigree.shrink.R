# Automatically generated from all.nw using noweb

plot.pedigree.shrink <- function(x, bigped=FALSE, title="", xlegend="topright", ...){

  ##  Plot pedigrees, coloring subjects according
  ##   to availability, shaded by affected status used in shrink

if (bigped == FALSE) {
tmp <- plot(x$pedObj, col = x$avail + 1,keep.par=T)
}
else {
tmp <- plot.pedigree(x$pedObj, align = FALSE, packed = FALSE, 
col = x$avail + 1, cex = 0.5, symbolsize = 0.5,keep.par=T)
}

legend(x = xlegend, legend = c("DNA Available", "UnAvailable"), 
pch = c(1, 1), col = c(2, 1), bty = "n", cex=.5)
title(paste(title, "\nbits = ", x$bitSize[length(x$bitSize)]),cex.main=.9)
invisible(tmp)
} 

