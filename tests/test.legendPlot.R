library(kinship2)
data(sample.ped)
pedAll <- pedigree(sample.ped$id, sample.ped$father, sample.ped$mother, sample.ped$sex,
              affected=cbind(sample.ped$affected, sample.ped$avail), famid=sample.ped$ped)

ped1 <- pedAll['1']
#plot(ped1)
source("../R/legendPlot.R")
## no color
legendPlot(ped1,  affected.label=c("cancer","available"))
##  with color
legendPlot(ped1, col=ped1$affected[,2]+1,  affected.label=c("cancer","available"))
legendPlot(ped1, col=ped1$affected[,2]+1, col.label=c("no dna", "dna"), affected.label=c("cancer","available"))
## more informative example with 2 affected status, consent-to-study, and vital status

## availability could be something like "consented", or having DNA available, so use consent from the availability status
consented <- ped1$affected[,2]

affected1 <- cbind(ped1$affected[,1], (ped1$id %in% c(108, 104, 131, 132, 140)))
aff.label1 <- c("Cancer","TII-Diab")

## add vital status 0=alive, 1=died  (puts a slash through the plot symbol
ped1$status <- ped1$id %in% c(105:108, 135:138, 115, 132)

legendPlot(ped1, col=consented+1, col.label=c("no consent","consent"), affected=affected1, status=vital1, affected.label=aff.label1)

