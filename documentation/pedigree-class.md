# The pedigree S4 object

The pedigree S4 object is a class that contains all the information about a pedigree.
It is composed of 2 slots:

    - `ped` a data.frame containing the pedigree information with at least 4 columns: `id`, `dadid`, `momid`, `sex`.
    - `rel` a data.frame containing the relationship between each individual of the pedigree with at least 3 columns: `id1`, `id2`, `code`.
    - `scales` a data.frame containing the affected scales to use.

The methods available for this class are the following:

    - autoplot()
    - plot()
    - summary()
    - print()
    - show()
    - as.data.frame()
    - setAs()
    - [
    - [<-
    - [[
    - [[<-
    - $
    - $<-
    - c()
    - rbind()
    - cbind()
    - dim()
    - dimnames()
    - names()
    - colnames()
    - rownames()
    - nrow()

## How to define new generic function

setClass(
    "Pedigree",
    slots = c(v1 = "character", v2 = "character", v3 = "character", v4 = "character"),
    prototype = prototype(v1 = "A", v2 = "B", v3 = "C", v4 = character())
)

setGeneric("dosomething", function(obj, ...) {
  standardGeneric("dosomething")
})

setMethod("dosomething", "Pedigree", function(obj) {
  obj@v4 <- dosomething(obj@v1, obj@v2, obj@v3)
  obj
})

setMethod("dosomething", "character", function(obj, v2 = "B", v3="C") {
  paste(obj, v2, v3)
})

dosomething(new("Pedigree"))
dosomething(c("A"))
dosomething(c("A"), c("D"))
