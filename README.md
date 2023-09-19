# The `kinship2` Package <img src="./inst/figures/icon_kinship2.png" align="right" height="139" />

[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/kinship2)](https://CRAN.R-project.org/package=kinship2)
[![Downloads](http://cranlogs.r-pkg.org/badges/kinship2)](https://CRAN.R-project.org/package=kinship2)
[![Total-Downloads](https://cranlogs.r-pkg.org/badges/grand-total/kinship2)](https://CRAN.R-project.org/package=kinship2)

Initially a set of functions to view pedigrees while developing models that use
kinship matrices, the functions were useful enough to put into a package of its
own, an S4 class for pedigrees, kinship function that calculates the kinship
matrix from a pedigree object, and the pedigree plotting routines that adhere
to many of the standards for genetics counselors.

## Installation

### From Github

To install this package from Github you can use the following R script:

```R
if (!require("remotes", quietly = TRUE))
    install.packages("remotes")

remotes::install_github("louislenezet/kinship2@dev_newclass",
    build_vignettes=TRUE
)
````

### From Bioconductor (not yet available)

To install this package you can use the following R script:

```R
if (!require("BiocManager", quietly = TRUE))
    install.packages("BiocManager")

BiocManager::install("kinship2")
```

## Documentation

To view documentation for the version of this package installed in your system,
start R and enter:

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

`pedigree()` is a function that creates an S4 class Pedigree object.
The core slot of the pedigree object is the `ped` slot built from having a row
per person, linked by the father id and mother id. Other relationships can be
specified, and affection status can be a matrix of multiple categories in the
`rel` slot. All the informations about affection and availability are be stored
respectively in `scales$fill` and `scales$border` slots and are therefore used
to fill and color the border for each elements of the pedigree graph.

### The `generate_colors()` Function

`generate_colors()` is a function that generates a color palette for an
affection status. This function is used by the `pedigree()` function to
generate the `scales$fill` and `scales$border` slots. The user can also
use this function to generate a color palette for a specific affection
status that will be added to the pedigree object.

### The `plot.pedigree()` Method

`plot.pedigree()` is a method for pedigree a object that plots as a
"family tree", with relatives of the same generation on the same row,
and affection statuses divided over the plot symbol for each person.
This function is designed in two steps:

1. First the pedigree object is converted into a data frame with all the
elements needed to plot the pedigree (i.e. boxes, lines, text, etc.).
This is done by the `ped_to_plotdf()` function.
2. Then the data frame is plotted using the `plot_fromdf()` function.

### The `kinship()` Funtion

`kinship()` is a function that creates the kinship matrix from a pedigree
object. It is coded for dyplotype organisms, handling all relationships that
can be specified for the pedigree object, including inbreeding, monozygotic
twins, etc. A recent addition is handling the kinship matrix for the X and Y
chromosomes.

### Other Notable Functions

* `shrink()` will shrink a pedigree to a given size, keeping the most
informative individuals for a single affection variable.

* `fix_parents()` will add parents for children who have a mother or dad listed
that is not already included. It will also fix the sex status for the parent if
it is mis-specified. This is useful to use before creating the **Pedigree**
object.

## Diagrams

The diagrams below show the main functions of the package and how they are
related.

![kinship2 Diagram](./inst/figures/kinship2_diagram.png)

### Details

Here is the details of the different parts of the diagram.

|       Process        |        Flow chart          |     Description      |
|:---------------------|:--------------------------:|----------------------|
| **S4 Pedigree creation** | <img src="inst/figures/pedigreeobj.png" alt="pedigreeobj" width="800"/> | The *pedigree S4 object* creation is done by the **pedigree()** constructor function. It mainly normalise a *ped_df* dataframe containing the information of each individuals with the **norm_ped()** function and the *rel_df* dataframe containing the special relationship (i.e. twins, spouse with no child) with the **norm_rel()** function. Thereafter It calls the **generate_colors()** function to create the colors scales (i.e. filling and border) before validating the object with **is_valid()** |
| **Alignment**            | <img src="inst/figures/alignment.png" alt="alignment" width="800"/>     | The alignment process is used to create a **plist** stroing the graphical disposition of the different individuals and their relation between them. The *hints* information used by **auto_hint()**, **align()** can be used by the user to force the ordering of some indiviuals |
| **Plotting**             | <img src="inst/figures/plotting.png" alt="plotting" width="800"/>       | The plotting process is now separated in three distinct steps: <br> - **ped_to_plotdf()** is first called to convert the *S4 pedigree object* into a dataframe listing all the graphical elements and their caracteristic. <br> - **ped_to_legdf()** does the same but for the legend informations. <br> - **plot_fromdf()** take as input such resulting dataframe and iteratively plot the elements based on their given characteristics. <br> All those steps are merge in one step with the **plot.pedigree()** method. |
| **Shrinking**            | <img src="inst/figures/shrinking.png" alt="shrinking" width="800"/>     | This process is usefull when you want to reduce a huge pedigree into a more simple version. The **shrink()** method will remove iteratively the less informative individuals from the pedigree until it reach the *max_bits* size awaited |
| **Informations**         | <img src="inst/figures/infos.png" alt="infos" width="800"/>             | From a *S4 pedigree object* it is possible to extract a lot of informations about the individuals, such as their shared relatedness **kinship()**, the number of direct and indirect childs **num_child()**, their informativness based on a set variable **is_informative()**, ... |
| **Checking columns**     | <img src="inst/figures/checkingcol.png" alt="checkingcol" height="200"/> | The **check_col()** function is used to check the presence absence of columns in a designated *data.frame* |
| **Legend**               | <img src="inst/figures/legend.png" alt="legend" height="200"/>           | The diagrams listed here follow this rules |
