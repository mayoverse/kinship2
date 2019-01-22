library(kinship2)
data(sample.ped)
pedAll <- pedigree(sample.ped$id, sample.ped$father, sample.ped$mother, sample.ped$sex,
              affected=cbind(sample.ped$affected, sample.ped$avail), famid=sample.ped$ped)

ped1 <- pedAll['1']
plot(ped1)

##legendPlot(ped1, affected.label=c("cancer","available")
ped <- ped1; affected=ped1$affected; affected.label=c("cancer","blood-avail")
legendPlot <- function(ped, id=ped$id, affected=ped$affected, affected.label=NULL, col=1, col.label=NULL, symbol.cex=.75, ...) {
  sAF <- options()$stringsAsFactors
  options(stringsAsFactors=FALSE)
  ## pedigree plot with dynamic legend of all affecteds on the bottom
  if(!is.null(affected)) {
    if(is.null(affected.label)) {
      affected.label <- colnames(affected)
    }
  
    #### Legend Configuration
    legdf <- rbind.data.frame(
                     c(1,0,0,1,""), #father, required
                     c(2,0,0,2,"")) # mother, required
  
     for(k in 1:length(affected.label)) {     
       legdf <- rbind.data.frame(legdf, c(2+k,1,2,2,affected.label[k]))
     }
     names(legdf) <- c("id","dadid","momid","sex", "idlabel")
     legaff <- rbind(rep(0, ncol(affected)), rep(0, ncol(affected)), diag(1, ncol(affected)))     
 
    #col.subj <- ifelse(leg$lab5 == 1, "red","black") 

    legped <- with(legdf, pedigree(as.numeric(id),as.numeric(dadid),as.numeric(momid),as.numeric(sex), affected=legaff))
    par(mar=c(1,2,1,2), oma=c(0,1,0,1))
    plot(legped, id=legdf$idlabel,
       density=rep(-1,ncol(legaff)), angle=rep(90,ncol(legaff)),
         symbolsize=0.6,packed=T,cex=symbol.cex,
   #col=col.subj,
    mar=c(0,2,0,2),fig=c(0,1,0,1/15),new=FALSE,keep.par=TRUE)
 
    ### DELETE PARENTS FROM LEGEND KEY  (or write over)####
    polygon(y=c(-1,-1,1.999,1.999), x=c(-1.5,8.5,8.5,-1.5), col='white', border=NA)
  ### End Legend Configuration


   ### BREAKING UP AREA TO ADD TRAITS ###
   par(new=TRUE, mar=c(1.5,1.5,2,2))
 
### PLOTTING THE ACTUAL PEDIGREE FOR THIS FAMILY ###
plot(ped, affected=ped$affected[,1:(ncol(ped$affected)-1)],density=c(-1,-1,-1,-1),angle=c(90,90,90,90),
     col=col.subj, id=id, symbolsize=0.7, packed=FALSE, cex=symbol.cex,
     keep.par=TRUE,fig=c(0,1,1/50,1), mar=c(1.5,1.5,2,2),new=TRUE)

    options(stringsAsFactors=sAF)
    
}

