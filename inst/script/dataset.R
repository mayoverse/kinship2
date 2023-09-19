## Here is listed the scripts used to generate the data files

#### minnbreast ####


#### sampleped ####
sampleped <- read.delim("inst/extdata/sampleped.tab",
    header = TRUE, sep = " ", stringsAsFactors = FALSE
)
sampleped[c("family", "id", "dadid", "momid")] <- as.data.frame(
    lapply(sampleped[c("family", "id", "dadid", "momid")], as.character)
)
summary(sampleped)
usethis::use_data(sampleped, overwrite = TRUE)
