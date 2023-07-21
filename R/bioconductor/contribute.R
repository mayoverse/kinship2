setwd("R/")
BiocManager::install(version = "devel")
BiocManager::valid()
library(devtools)
library(lintr)

# Linting
lintr::use_lintr(type = "tidyverse")
usethis::use_github_action("lint")
lintr::lint_package()

# Must pass
check()
build() # Generate less than 5Mb
BiocCheck::BiocCheckGitClone()
BiocCheck::BiocCheck('new-package'=TRUE)

check(vignettes = FALSE) # Should take less than 10 min

# Vignettes + Man < 3Gb
# Each file < 5Mb


readCitationFile("inst/CITATION")

#### commands to build the package using devtools
devtools::check_man()
devtools::test()
devtools::check()
withr::with_libpaths(c("../testinstalls/", .libPaths()), devtools::install(build_vignettes = TRUE, dependencies = FALSE))
devtools::build("../kinship2/")
## < restart R >
library(kinship2, lib.loc = "../testinstalls/")

#### to upload to CRAN
## Update DESCRIPTION, README.md, NEWS.md, and cran-comments.md
devtools::revdep_check()
devtools::release()
