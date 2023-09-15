
## reported on github in 2023
## version 1.9.6 failed to plot subject 3 second marriage and kids
## fix in 9/2023 to revert to some version 1.8.5 version of kindepth
x = pedigree(id = 1:7, dadid = c(0,0,0,1,3,0,3), momid = c(0,0,0,2,4,0,6), sex = c(1,2,1,2,1,2,1))
plot(x)
