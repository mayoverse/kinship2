## Here is listed the scripts used to generate the data files

#### minnbreast ####

#### sampleped ####
sampleped <- read.delim("data-raw/sampleped.tab",
    header = TRUE, sep = " ", stringsAsFactors = FALSE
)
usethis::use_data(sampleped, overwrite = TRUE)
