
Commands to build the package, most from within R
=============================================

All of these can be run from linux command lines as well, with Rscript

```{r, devtoolsCmds, eval=FALSE}
 Rcpp:::compileAttributes()  ## only if Rcpp has changed 
 devtools::load_all() ## update package within session, re-builds help and namespace
 devtools::check_man()  ## build/check man pages, NAMESPACE file (if not already
 made), roxygenize help files

 devtools::test()  # run tests, but this requires testthat format of scripts
 devtools::check()  # check rd, tests (non-testthat), all
 devtools::build()
 
```

Some Older Commands from not using devtools:
========================================

```{shell, notdevtools}
R CMD build kinship2   ## buils tar.gz
R CMD check kinship2_x.y.tar.gz
R CMD check --as-cran kinship2_x.y.tar.gz
## just before submission to cran, need to run on R-devel version
```


## To get PDF version of help files (after they are created by roxygen

```{shell, pdfhelp}
## after devtools:::check_man(), which creates Rd files from .R files
R CMD Rd2pdf kinship2
```
