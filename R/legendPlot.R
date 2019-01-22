

legendPlot <- function(ped, id=ped$id, affected=ped$affected, affected.label=NULL, col=1, col.label=NULL, symbol.cex=.75, ...) {
  
  sAF <- options()$stringsAsFactors
  options(stringsAsFactors=FALSE)
  
  ## pedigree plot with dynamic legend of all affecteds on the bottom

  if(length(id) != nrow(ped)) {
    warning("id not equal to number in pedigree; id set to ped$id\n")
    id <- ped$id
  }
  
  if(length(unique(col)) > 1) {
    if(!is.null(col.label) && length(col.label) != length(unique(col))) {
      warning("col.label not equal to unique number of colors; ignoring colors")
      col=1
      col.label=NULL
      legend.col=1
    }
  }
  
  if(!is.null(affected)) {
    if(is.null(affected.label)) {
      affected.label <- colnames(affected)
    }
    if(length(affected.label) != ncol(affected)) {
      stop("affected.label not equal to the number of affected statuses.\n")
    }
  }
          
  #### Legend Configuration
  if(!(ncol(affected)<2 & length(col.label) < 2)) {
    legdf <- rbind.data.frame(
                      c(1,0,0,1,""), #father, required
                      c(2,0,0,2,"")) # mother, required
    legend.col=c(1,1)
    for(j in 1:length(affected.label)) {     
      legdf <- rbind.data.frame(legdf, c(2+j,1,2,2,affected.label[j]))
      legend.col=c(legend.col, unique(col)[1])
    }
    legaff <- rbind(rep(0, ncol(affected)), rep(0, ncol(affected)), diag(1, length(affected.label)))
    if(length(col.label)>1) {  ## color labels if more than 1 color
      for(k in 1:length(col.label)) {
        legdf <- rbind.data.frame(legdf, c(2+j+k, 1, 2, 2, col.label[k]))
        legend.col <- c(legend.col, unique(col)[k])
        legaff <- rbind(legaff, rep(0, length(affected.label)))
      }
    }
    names(legdf) <- c("id","dadid","momid","sex", "idlabel")
    
    legped <- with(legdf, pedigree(as.numeric(id),as.numeric(dadid),as.numeric(momid),
                                   as.numeric(sex), affected=legaff))
    par(mar=c(0,2,2,2), oma=c(0,1,0,1))
    plot(legped, id=legdf$idlabel, col=legend.col,
         density=rep(-1,ncol(legaff)), angle=rep(90,ncol(legaff)),
         symbolsize=0.6,packed=TRUE,cex=symbol.cex,  
         mar=c(0,2,2,2),fig=c(0,1,0,1/15),new=FALSE,keep.par=TRUE)
     
     ### DELETE PARENTS FROM LEGEND KEY  (or write over)####
     polygon(y=c(-1,-1,1.999,1.999), x=c(-1,8,8,-1), col='white', border=NA)
     ### End Legend Configuration

     ### BREAKING UP AREA TO ADD TRAITS ###
     par(new=TRUE, mar=c(4.5,1,1,1))
  }
  ### PLOTTING THE ACTUAL PEDIGREE FOR THIS FAMILY ###
  plot(ped, density=c(-1,-1,-1,-1),angle=c(90,90,90,90),
       col=col, id=id, symbolsize=symbol.cex, packed=FALSE, cex=symbol.cex,
       keep.par=TRUE,fig=c(0,1,1/50,1), mar=c(4.5,1,1,1),new=TRUE)

  options(stringsAsFactors=sAF)
    
  
}
