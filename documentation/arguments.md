# List of all arguments to inherit

```R
#' @param ... Additional arguments passed to methods
#' @param id A vector of individuals ids
#' @param idlist List of individuals id to be considered
#' @param famid A vector of family identifiers
#' @param dadid A vector of fathers ids
#' @param momid A vector of mothers ids
#' @param sex A vector with the gender of all the individuals.
#' Either character ('male','female','unknown','terminated') or
#' numeric (1='male', 2='female',#' 3='unknown', 4='terminated')
#' data is allowed.
#' @param avail Vector of availability status (e.g., genotyped) 0/1 or
#' TRUE/FALSE
#' @param missid Character defining the missing ids
#' @param idx Index of the subject
#' @param dadx Index of the father
#' @param momx Index of the mother
#' @param level Vector of the level of each subject
#' @param horder Vector of the horizontal order of each subject
#' @param spouselist Matrix of the spouses
#' @param rval A list with components `n`, `nid`, `pos`, and `fam`.
#' @param spouse A matrix with one row per level, giving the index of the
#' spouse for each subject. `0` means no spouse.
#' @param level A vector giving the level of each subject.
#' @param reset If `TRUE`, reset the hints to the initial values
#' @param ped A pedigree object
#' @param packed Should the pedigree be compressed, i.e., allow diagonal
#' lines connecting parents to children in order to have a smaller overall
#' width for the plot.
#' @param width for a packed output, the minimum width of the plot, in
#' inches.
#' @param align for a packed pedigree, align children under parents `TRUE`, to
#' the extent possible given the page width, or align to to the left margin
#' `FALSE`. This argument can be a two element vector, giving the alignment
#' parameters, or a logical value.  If `TRUE`, the default is `c(1.5, 2)`, or
#' numeric the routine `alignped4` will be called.
#' @param hints Plotting hints for the pedigree.
#' This is a list with components `order` and `spouse`, the second one is
#' optional.
#' The order component is a numeric vector with one element per subject in the
#' pedigree.  It determines the relative order of subjects within a sibship, as
#' well as the relative order of processing for the founder couples. (For this
#' latter, the female founders are ordered as though they were sisters). The
#' spouse component is a matrix with one row per hinted marriage, usually only
#' a few marriages in a pedigree will need an added hint, for instance reverse
#' the plot order of a husband/wife pair. Each row contains the index of the
#' left spouse, the right hand spouse, and the anchor
#' (i.e : `1` = left, `2` = right, `0` = either).
#' Children will preferentially appear under the parents of the anchored spouse.
#' @param missid The missing id code in the pedigree (default is  `0`)
#' @param affstatus Affection status to search for.
#' @param boxh Height of the legend boxes
#' @param boxw Width of the legend boxes
#' @param cex Size of the legend text
```

# List of returned values
n
nid
pos
spouselist

# Roxygen S4 methods writing

```{r}
#' My function
#'
#' @param obj An object either a character vector or a pedigree
#' @param ... Arguments to be passed to methods
#'
#' @docType methods
#' @export
setGeneric("myfunction", signature = "obj",
    function(obj, ...) standardGeneric("myfunction")
)

#' @docType methods
#' @aliases myfunction,character
#' @rdname myfunction
#' @param dadid A character vector
#' @param momid A character vector
#' @param missid Character defining the missing ids
#' @usage ## S4 method for signature 'character'
#' @usage myfunction(dadid, momid, missid = "0")
#' @return A character vector with the parents ids
setMethod("myfunction", "character", function(dadid, momid, missid = "0") {
    paste(dadid, momid, sep = missid)
})

#' @docType methods
#' @aliases myfunction,Pedigree
#' @param ped A pedigree object
#' @param missid Character defining the missing ids
#' @usage ## S4 method for signature 'Pedigree'
#' @usage myfunction(dadid, momid, missid = "0")
#' @return A pedigree with the parents ids
setMethod("myfunction", "Pedigree",
    function(ped, missid = "0") {
        ped$par <- myfunction(ped$dadid, ped$momid, missid)
        ped
    }
)
```
