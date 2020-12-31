
## if all you have is maternal information, fill in for dads
## from user on 2/1/19
require(kinship2)
materdf <- data.frame(id=1:5, momid=c(0,1,1,2,2), sex=2)

materdf$dadid <- materdf$momid * 100
materdf

peddf <- with(materdf, fixParents(id, dadid, momid, sex))
peddf
testped <- with(peddf, pedigree(id, dadid, momid, sex))
as.data.frame(testped)


## chars
test1char <- data.frame(id=paste("fam", 101:111, sep=""),
                sex=c("male","female")[c(1,2,1,2,1, 1,2, 2,1,2, 1)],
          father=c(0,0,"fam101","fam101","fam101", 0,0,"fam106","fam106","fam106", "fam109"),
          mother=c(0,0,"fam102","fam102","fam102", 0,0,"fam107","fam107","fam107", "fam112"))
test1newmom <- with(test1char, fixParents(id, father, mother, sex, missid="0"))
newped <- with(test1newmom, pedigree(id, dadid, momid, sex, missid="0"))
as.data.frame(newped)


data(sample.ped)
datped2 <- sample.ped[sample.ped$ped %in% 2,]
datped2[datped2$id %in% 203, "sex"] <- 2
datped2 <- datped2[-which(datped2$id %in% 209),]
## this gets an error
##ped2 <- with(datped2, pedigree(id, father, mother, sex))
fixped2 <- with(datped2, fixParents(id, father, mother, sex))
fixped2
ped2 <- with(fixped2, pedigree(id, dadid, momid, sex))

