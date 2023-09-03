# List of all arguments to inherit

missid
dadid
id
momid
dadx
momx
idx
packed

# List of returned values
n
nid
pos
spouselist

# Roxygen S4 methods writing

```{r}
#' My function
#'
#' @param obj Vector of fathers ids or a pedigree
#' @param momid Vector of mothers ids
#' @param missid Character defining the missing ids
#'
#' @return A paste of the father and mother ids
#' or a pedigree with the parents ids
#' @docType methods
#' @export
setGeneric("myfunction", signature = "obj",
    function(obj, ...) standardGeneric("myfunction")
)

#' @docType methods
#' @aliases myfunction,character
#' @rdname myfunction
setMethod("myfunction", "character", function(obj, momid, missid = "0") {
    paste(obj, momid, sep = missid)
})

#' @docType methods
#' @aliases myfunction,Pedigree
#' @rdname myfunction
setMethod("myfunction", "Pedigree",
    function(obj, missid = "0") {
        obj$par <- myfunction(obj$dadid, obj$momid, missid)
        obj
    }
)
```
