
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/kinship2)](https://CRAN.R-project.org/package=kinship2)
[![Downloads](http://cranlogs.r-pkg.org/badges/kinship2)](https://CRAN.R-project.org/package=kinship2)
[![Total-Downloads](https://cranlogs.r-pkg.org/badges/grand-total/kinship2)](https://CRAN.R-project.org/package=kinship2)

# The `kinship2` Package <img src="./inst/figures/icon_kinship2.png" align="right" height="139" />

Initially a set of functions to view pedigrees while developing models that use kinship matrices, the functions were useful enough to put into a package of its own, an S4 class for pedigrees, kinship function that calculates the kinship matrix from a pedigree object, and the pedigree plotting routines that adhere to many of the standards for genetics counselors.

## Installation

To install this package you can use the following R script:

```R
if (!require("BiocManager", quietly = TRUE))
    install.packages("BiocManager")

BiocManager::install("kinship2")
```

## Documentation

To view documentation for the version of this package installed in your system, start R and enter:

```R
browseVignettes("kinship2")
```

## News

To see the new functions / modification please use:

```R
utils::news(package="kinship2")
```

## The main functions

### The `pedigree()` Function

`pedigree()` is a function that creates an S4 class Pedigree object. The core slot of the pedigree object is the `ped` slot built from having a row per person, linked by the father id and mother id. Other relationships can be specified, and affection status can be a matrix of multiple categories in the `rel` slot. All the informations about affection and availability are be stored respectively in `scales$fill` and `scales$border` slots and are therefore used to fill and color the border for each elements of the pedigree graph.

### The `generate_colors()` Function

`generate_colors()` is a function that generates a color palette for an affection status. This function is used by the `pedigree()` function to generate the `scales$fill` and `scales$border` slots. The user can also use this function to generate a color palette for a specific affection status that will be added to the pedigree object.

### The `plot.pedigree()` Method

`plot.pedigree()` is a method for pedigree a object that plots as a "family tree", with relatives of the same generation on the same row, and affection statuses divided over the plot symbol for each person. This function is designed in two steps:

1. First the pedigree object is converted into a data frame with all the elements needed to plot the pedigree (i.e. boxes, lines, text, etc.). This is done by the `ped_to_plotdf()` function.
2. Then the data frame is plotted using the `plot_fromdf()` function.

### The `kinship()` Funtion

`kinship()` is a function that creates the kinship matrix from a pedigree object. It is coded for dyplotype organisms, handling all relationships that can be specified for the pedigree object, including inbreeding, monozygotic twins, etc. A recent addition is handling the kinship matrix for the X and Y chromosomes.  

### Other Notable Functions

* `shrink()` will shrink a pedigree to a given size, keeping the most informative individuals for a single affection variable.

* `fix_parents()` will add parents for children who have a mother or dad listed that is not already included. It will also fix the sex status for the parent if it is mis-specified. This is useful to use before creating the **Pedigree** object.

## Diagrams

The diagrams below show the main functions of the package and how they are related.

![kinship2 Diagram](./inst/figures/kinship2_diagram.png)