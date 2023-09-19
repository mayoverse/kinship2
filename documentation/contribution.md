# To contribute to this package

## Organisation

R package organisation is quite straigth forward:

- All R functions are stored in the `R` folder.
- All documentation are stored in the `man` folder.
- Data samples are stored in the `data` folder.
- The `tests` folder contains the unnitest runned during compilation
- Some `vignettes` are also available in their own folder to explain more the
codes.

## R functions convention

### Documentation

Each function are preceded by ROxygen comment to render the documentation of
each function.
To transfer those ROxygen comment to Rd files in the `man` folder, run:

```R
devtools::document()
```

### Writing

One good guidelines that could be followed is the one from the
[**TidyVerse**](https://style.tidyverse.org/).
It explains how to name files, functions, variable, for analyses and packages.
It also add information on how to nicely write the code: space, indentations,
...

All those guidelines can be automatically detected and applied to a package
with [**styler**](https://styler.r-lib.org/) package.

### Testing

One objective would be to add tests for each functions and for each conditions.
For that we use the package [**test_that**](https://testthat.r-lib.org/).
For a given function a linked test file can be created in the `test/testthat/`
folder with

```R
use_test("function_name")
```

When the test is added inside the file it will be ran during compilation or
can be done with:

```R
devtools::test()   
```

To see the coverage of the generated code you can use:

```R
library(covr)

# If run with no arguments implicitly calls `package_coverage()`
report()
```

## To build the backage

### Needs

To build the package you need to have installed:

- `R` and `R.exe` in your path
- [MiKTeX](https://miktex.org/download) to build the pdf
- [Font ts1-zi4r](https://tex.stackexchange.com/questions/125274/error-font-ts1-zi4r-at-540-not-found)
- [Qpdf](https://github.com/qpdf/qpdf)

If you have `R.exe` in your path you can run:

```powershell
set R_CHECK_ENVIRON = ./documentation/check.Renviron
R.exe CMD build .
R.exe CMD check kinship2_2.0.0.tar.gz
```

## Submit to BioConductor

```R
BiocCheck::BiocCheckGitClone()
BiocCheck::BiocCheck('kinship2'=TRUE)
```

## To check up

```R
readCitationFile("inst/CITATION")
```