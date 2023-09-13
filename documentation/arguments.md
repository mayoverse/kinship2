# List of all arguments to inherit

```R
#' @param ... Additional arguments passed to methods
#' @param id A vector of individuals ids

## Argu

## Arguments of fix_parents
#' @param sex Gender column or a vector of the sex of the individuals. Either
#' character ('male','female','unknown','terminated') or
#' numeric (1='male', 2='female',#' 3='unknown', 4='terminated')
#' data is allowed.
#' @param missid The founders are those with no father or mother in the
#' pedigree.  The \\code{dadid} and \\code{momid} values for these subjects will
#' either be NA or the value of this variable.  The default for \\code{missid}
#' is 0 if the \\code{id} variable is numeric, and '' (the empty string)
#' otherwise.

## Argument in find_avail_noninform, find_avail_affected
#' @param avail Vector of availability status (e.g., genotyped) 0/1 or
#' TRUE/FALSE 

## Arguments in find_avail_affected
#' @param affstatus Affection status to search for.

## Arguments in descendants, family_check
#' @param obj A pedigree object or a vector of the individuals identifiers
#' @param dadid A vector of the father identifiers
#' @param momid A vector of the mother identifiers

## Arguments in check_hints
#' @param sex An ordered factor vector with the gender of all the individuals.
#' The levels are ordered as follow : 'male', 'female', 'unknown', 'terminated'.

## Argument in bit_size, descendants
#' @param momid A vector of mothers identifiers
#' @param ... Additional arguments passed to methods

## Arguments of bit_size
#' @param obj A pedigree object or a vector of fathers ids

## Argument in align, bitsize, find_avail_noninform, find_avail_affected
#' @param missid The missing id code in the pedigree (default is  `0`)

## Arguments of family_check
#' @param famid A vector of family identifiers
#' @param newfam The result of a call to `make_famid()`. If this has already
#' been computed by the user, adding it as an argument shortens the running
#' time somewhat.

## Arguments of alignped1
#' @param idx Indexes of the subjects
#' @param dadx Indexes of the fathers
#' @param momx Indexes of the mothers
#' @param level Vector of the level of each subject
#' @param horder Vector of the horizontal order of each subject
#' @param spouselist Matrix of the spouses with one row per hinted marriage,
#' usually only a few marriages in a pedigree will need an added hint, for 
#' instance reverse the plot order of a husband/wife pair.
#' Each row contains the index of the left spouse, the right hand spouse
#' and the anchor (i.e : `1` = left, `2` = right, `0` = either).

## Arguments of alignped3
#' @param x1 Alignement of the first tree
#' @param x2 Alignement of the second tree
#' @param space Space between two subjects

## Arguments of alignped4
#' @param rval A list with components `n`, `nid`, `pos`, and `fam`.
#' @param spouse A boolean matrix with one row per level representing if
#' the subject is a spouse or not.

## Arguments of align
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
#' numeric the routine `alignped4()` will be called.
#' @param hints Plotting hints for the pedigree.
#' This is a list with components `order` and `spouse`, the second one is
#' optional.
#' - **order** is a numeric vector with one element per subject in the
#' pedigree.  It determines the relative order of subjects within a sibship, as
#' well as the relative order of processing for the founder couples. (For this
#' latter, the female founders are ordered as though they were sisters).
#' - **spouse** is a matrix with one row per hinted marriage, usually only
#' a few marriages in a pedigree will need an added hint, for instance reverse
#' the plot order of a husband/wife pair. Each row contains the index of the
#' left spouse, the right hand spouse, and the anchor
#' (i.e : `1` = left, `2` = right, `0` = either).
#' Children will preferentially appear under the parents of the anchored spouse.
#' @param missid The missing id code in the pedigree (default is  `0`)

## Arguments of shift()
#' @param id The id of the subject to be shifted
#' @param sibs The ids of the siblings
#' @param goleft If `TRUE`, shift to the left, otherwise to the right
#' @param hint The current hint vector
#' @param twinrel The twin relationship matrix
#' @param twinset The twinset vector

## Arguments of findspouse()
#' @param idpos The position of the subject
#' @param plist The alignment structure representing the pedigree layout.
#' For the differents matrices present in the list, each row represents a
#' level of the pedigree and each column a potential subject.
#' It contains the following components:
#' - n Vector of the number of subjects per level
#' - nid Matrix of the subjects indexes
#' - pos Matrix of the subjects positions
#' - fam Matrix of the siblings family identifiers
#' - spouse Matrix of the left spouses `1` = spouse, `0` = not spouse,
#' `2` = inbred spouse.
#' @param lev The generation level of the subject

## Arguments of duporder()
#' @param idlist List of individuals identifiers to be considered

## Arguments of auto_hint()
#' @param reset If `TRUE`, then even if `ped` object has hints, reset
#' them to the initial values

## Arguments of best_hint()
#' @param wt A vector of three weights for the three error measures
#' - The number of duplicate individuals in the plot
#' - The sum of the absolute values of the differences in the
#'   positions of duplicate individuals
#' - The sum of the absolute values of the differences between
#'   the center of the children and the parents
#' Default is `c(1000, 10, 1)`.
#' @param tolerance The maximum stress level to accept. Default is `0`


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
