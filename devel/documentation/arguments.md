# List of all arguments to inherit

```R
## Arguments in kinship
#' @param obj A pedigree object or a vector of the individuals identifiers
#' @param ... Additional arguments passed to methods
#' + dadid, momid, sex

## Arguments of sex_to_factor
#' @param sex A character, factor or numeric vector corresponding to
#' the gender of the individuals. The following values are recognized:
#' - character() or factor() : "f", "m", "woman", "man", "male", "female",
#' "unknown", "terminated"
#' - numeric() : 1 = "male", 2 = "female", 3 = "unknown", 4 = "terminated"

## Arguments of is_parent
#' @param id A vector of each subjects identifiers
#' @param dadid A vector containing for each subject, the identifiers of the
#' biologicals fathers.
#' @param momid  vector containing for each subject, the identifiers of the
#' biologicals mothers.
#' @param missid The missing identifier value. Founders are the individuals with
#' no father and no mother in the pedigree (i.e. `dadid` and `momid` equal to the
#' value of this variable).  The default for `missid` is `"0"`.

## Argument of is_informative
#' @param avail A numeric vector of availability status of each individual
#' (e.g., genotyped). The values are:
#' - `0`  : unavailable
#' - `1`  : available
#' - `NA` : availability not known
#' @param informative Informative individuals selection can take 5 values:
#' 'AvAf' (available and affected),
#' 'AvOrAf' (available or affected),
#' 'Av' (available only),
#' 'Af' (affected only),
#' 'All' (all individuals)
#' or a numeric/character vector of individuals id
#' or a boolean
#' @param affected A numeric vector of affection status of each individual
#' (e.g., genotyped). The values are:
#' - `0`  : unaffected
#' - `1`  : affected
#' - `NA` : affection status not known
#' @param col_aff A string with the column name to use for the affected status.

## Arguments of useful_inds
#' @param num_child_tot A numeric vector of the number of children of each
#' individuals
#' @param keep_infos Boolean to indicate if individuals with unknown status
#' but available or reverse should be kept

## Arguments in find_avail_affected
#' @param affstatus Affection status to search for.

## Arguments of pedigree
#' @param rel_df
#' @param ped_df

## Arguments of bit_size
#' @param obj A pedigree object or a vector of fathers ids

## Arguments of shrink
#' @param max_bits Optional, the bit size for which to shrink the pedigree

## Argument of kindepth
#' @param align_parents If `align_parents=T`, go one step further and try to
#' make both parents of each child have the same depth.
#' (This is not always possible).
#' It helps the drawing program by lining up pedigrees that 'join in the middle'
#' via a marriage.

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
#' @param horder Vector of the horizontal horder of each subject
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
#' This is a list with components `horder` and `spouse`, the second one is
#' optional.
#' - **horder** is a numeric vector with one element per subject in the
#' pedigree.  It determines the relative horizonal order of subjects within a sibship, as
#' well as the relative order of processing for the founder couples. (For this
#' latter, the female founders are ordered as though they were sisters).
#' - **spouse** is a matrix with one row per hinted marriage, usually only
#' a few marriages in a pedigree will need an added hint, for instance reverse
#' the plot order of a husband/wife pair. Each row contains the index of the
#' left spouse, the right hand spouse, and the anchor
#' (i.e : `1` = left, `2` = right, `0` = either).
#' Children will preferentially appear under the parents of the anchored spouse.

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

## Arguments of ped_to_plotdf
#' @param branch defines how much angle is used to connect various levels of
#' nuclear families.
#' @param mark if TRUE, add a mark to each box corresponding to the value of
#' the affection column for each filling scale.
#' @param label if not NULL, add a label to each box corresponding to the
#' value of the column given.
#' @param pconnect when connecting parent to children the program will try to
#' make the connecting line as close to vertical as possible, subject to it
#' lying inside the endpoints of the line that connects the children by at
#' least `pconnect` people.  Setting this option to a large number will
#' force the line to connect at the midpoint of the children.

## Arguments of set_plot_area
#' @param cex character expansion of the text
#' @param maxlev maximum level
#' @param xrange range of x values
#' @param symbolsize size of the symbols
#' @param ... other arguments passed to [par()]
 
## Arguments of plot_fromdf
#' @param df
#' @param usr The user coordinates of the plot.
#' @param title The title of the plot.
#' @param add_to_existing A logical to know if the plot should be added to an
#' existing plot.
#' @param boxh Height of the legend boxes
#' @param boxw Width of the legend boxes

## Arguments of plot
#' @param fam_to_plot default=1.  If the pedigree contains multiple families,
#' this parameter can be used to select which family to plot.
#' @param legend default=FALSE.  If TRUE, a legend will be added to the plot.
#' @param leg_cex default=0.8.  Controls the size of the legend text.
#' @param leg_symbolsize default=0.5.  Controls the size of the legend symbols.
#' @param leg_loc default=NULL.  If NULL, the legend will be placed in the
#' upper right corner of the plot.  Otherwise, a 4-element vector of the form
#' (x0, x1, y0, y1) can be used to specify the location of the legend.
#' @param ... Extra options that feed into the plot function.
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
