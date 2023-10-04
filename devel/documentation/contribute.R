setwd("R/")
BiocManager::install(version = "devel")
BiocManager::valid()
library(devtools)

load_all()

library(lintr)
library(formatR)

#formatR::tidy_dir(".", width.cutoff = 80, recursive = TRUE, overwrite = TRUE,
#    wrap = FALSE, indent = 4, arrow = TRUE)

# Linting
lintr::use_lintr(type = "tidyverse")
usethis::use_github_action("lint")
lintr::lint_package()

# Must pass
check()
devtools::build_vignettes()
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
devtools::build("../Pedixplorer/")
## < restart R >
library(Pedixplorer, lib.loc = "../testinstalls/")

#### to upload to CRAN
## Update DESCRIPTION, README.md, NEWS.md, and cran-comments.md
devtools::revdep_check()
devtools::release()
