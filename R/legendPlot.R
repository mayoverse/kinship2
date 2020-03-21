#' Legend Pedigree Plot
#'
#' Pedigree plot with ready-made legend along the bottom of the page to represent colors 
#' and affection statuses
#' 
#' @param x Pedigree data frame with ped (pedigree id), id (id of individual),
#'   father (id of father), mother (id of mother), sex, affected (affection status), 
#'   and avail (DNA availability).
#' 
#' @param id Optional, a character string to replace the correspinding id for persons in the pedigree
#' 
#' @param affected A variable indicating affection status. A multi-column matrix can be used to
#'    give the status with respect to multiple traits. Logical, factor, and integer types
#'    are converted to 0/1 representing unaffected and affected, respectively. NAs are
#'    considered missing.
#' 
#' @param affected.label Set labels for affection statuses
#' 
#' @param col  Colors for the plot symbol for each individual
#' 
#' @param col.label Named vector, with elements matching the unique color codes, the names are the labels used in the legend.
#' 
#' @param symbolsize Size of symbols. Default is 1.0
#'
#' @examples 
#' require(kinship2)
#' data(sample.ped)
#' pedAll <- pedigree(sample.ped$id, sample.ped$father, sample.ped$mother, sample.ped$sex, affected=cbind(sample.ped$affected, sample.ped$avail), famid=sample.ped$ped)
#' ped1 <- pedAll['1']
#' legendPlot(ped1,  affected.label=c("cancer","available")
#' 
#' @author Jason Sinnwell, with code contributed by Sara Achenbach
#' @seealso \code{\link{pedigree}}, \code{\link{plot.pedigree}}
#' @name legendPlot
NULL
#> NULL

#' @rdname legendPlot
#' @export
legendPlot <- function(x, id=x$id, affected=x$affected, affected.label=NULL, col=1, col.label=NULL, symbolsize=.75, cex=.5, ...) {

  ## Need to deal with real char strings. Set stringsAsFactors back at end
  sAF <- options()$stringsAsFactors
  options(stringsAsFactors=FALSE)
    
  ## check colors
  if(any(col %in% 0 | is.na(col))) {
    warning("missing or zero-value colors exist, will show blank symbol")
  }
 
  ##Pedigree plot with dynamic legend of all affecteds on the bottom
  if(length(id) != nrow(as.data.frame(x))) {
    warning("id not equal to number in pedigree; id set to ped$id\n")
    id <- x$id
  }
  ucols <- sort(unique(col)) ## col[!duplicated(col)]
  if(length(ucols) > 1) {
    if(!is.null(col.label) && length(col.label) < length(unique(col))) {
      warning("col.label not equal to unique number of colors; ignoring colors")
      col=1
      col.label=NULL
      legend.col=1
    }
  }
   
  ##  match labels by names
  colorLabels <- names(col.label[match(ucols, col.label)])
  if(!is.null(affected)) {
    if(is.null(affected.label)) {
      affected.label <- colnames(affected)
    }
    if(length(affected.label) != ncol(affected)) {
      stop("affected.label not equal to the number of affected statuses.\n")
    }
  }
 
  ## Legend Configuration, a nuclear family with a sibship of 
  ## all colors/affection statuses. Parents will be plotted  over
  if(!(ncol(affected)<2 & length(col.label) < 2)) {
    legdf <- rbind.data.frame(
      c(1,0,0,1,""), #father, required
      c(2,0,0,2,"")) # mother, required, 
    legend.col=c(1,1)
    for(j in 1:length(affected.label)) {     
      legdf <- rbind.data.frame(legdf, c(2+j,1,2,2,affected.label[j]))
      legend.col=c(legend.col, ucols[1])
    }

    legaff <- rbind(rep(0, ncol(affected)), rep(0, ncol(affected)), diag(1, length(affected.label)))
    if(length(colorLabels)>1) {  ## color labels if more than 1 color
      for(k in 1:length(colorLabels)) {
        legdf <- rbind.data.frame(legdf, c(2+j+k, 1, 2, 2, colorLabels[k]))
        legend.col <- c(legend.col, ucols[k])
        legaff <- rbind(legaff, rep(0, length(affected.label)))
      }
    }
    names(legdf) <- c("id","dadid","momid","sex", "idlabel")
    legped <- with(legdf, pedigree(as.numeric(id),as.numeric(dadid),as.numeric(momid),
                                   as.numeric(sex), affected=legaff))
    
    ## plot the legend nuclear family on the bottom
    par(mar=c(0,2,2,2), oma=c(0,1,0,1))
    plot(legped, id=legdf$idlabel, col=legend.col,
            density=rep(-1,ncol(legaff)), angle=rep(90,ncol(legaff)),
            symbolsize=symbolsize, cex=cex, packed=TRUE,  mar=c(0,2,2,2), ...)
         # fig=c(0,1,0,1/15),new=FALSE,keep.par=TRUE, ...)
 
 ##DELETE PARENTS FROM LEGEND KEY  (or write over)   
    polygon(y=c(-1,-1,1.999,1.999), x=c(-1,8,8,-1), col='white', border=NA)
 ##End Legend Configuration  
    
 ##BREAKING UP AREA TO ADD TRAITS
    par(new=TRUE) # mar=c(4.5,1,1,1))
  }

 ##PLOTTING THE ACTUAL PEDIGREE FOR THIS FAMILY
  plot(x, density=c(-1,-1,-1,-1),angle=c(90,90,90,90),
       col=col, id=id, symbolsize=symbolsize, cex=cex, packed=FALSE, 
       keep.par=TRUE,fig=c(0,1,1/50,1), new=TRUE, mar=c(3.5,1,1.5,1),new=TRUE, ...)
 
  options(stringsAsFactors=sAF)  
}
 
