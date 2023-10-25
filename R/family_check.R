# Extracted from checks.Rnw

#' Check family
#'
#' @description
#' Error check for a family classification
#'
#' @details
#' Given a family id vector, also compute the familial grouping from first
#' principles using the parenting data, and compare the results.
#'
#' The `make_famid` function is used to create a de novo family id from the
#' parentage data, and this is compared to the family id given in the data.
#'
#' If there are any joins, then an attribute 'join' is attached.
#' It will be a matrix with family as row labels, new-family-id as the columns,
#' and the number of subjects as entries.
#'
#' @inheritParams kinship
#' @param family A vector of family identifiers
#' @param newfam The result of a call to `make_famid()`. If this has already
#' been computed by the user, adding it as an argument shortens the running
#' time somewhat.
#'
#' @return a data frame with one row for each unique family id in the
#' `family` argument or the one detected in the Pedigree object.
#' Components of the output are:
#' - `family` : The family id, as entered into the data set
#' - `n` : Number of subjects in the family
#' - `unrelated` : Number of them that appear to be unrelated to
#' anyone else in the entire Pedigree.  This is usually marry-ins with no
#' children (in the Pedigree), and if so are not a problem.
#' - `split` : Number of unique 'new' family ids.
#'     - `0` = no one in this 'family' is related to anyone else (not good)
#'     - `1` = everythings is fine
#'     - `2` and + = the family appears to be a set of disjoint trees.
#'       Are you missing some of the people?
#' - `join` : Number of other families that had a unique
#' family, but are actually joined to this one.  0 is the hope.
#'
#' @examples
#'
#' # use 2 samplepeds
#' data(sampleped)
#' pedAll <- Pedigree(sampleped)
#'
#' ## check them giving separate ped ids
#' fcheck.sep <- family_check(pedAll)
#' fcheck.sep
#'
#' ## check assigning them same ped id
#' fcheck.combined <- with(sampleped, family_check(id, dadid, momid,
#' rep(1, nrow(sampleped))))
#' fcheck.combined
#'
#' # make person 120's father be her son.
#' sampleped[20, 3] <- 131
#' fcheck1.bad <- try(
#'   {
#'     with(sampleped, family_check(id, father, mother, family))
#'   },
#'   silent = FALSE
#' )
#'
#' ## fcheck1.bad is a try-error
#'
#' @seealso [make_famid()], [kinship()]
#' @include AllClass.R
#' @keywords internal
#' @docType methods
#' @export
setGeneric("family_check", signature = "obj",
    function(obj, ...) standardGeneric("family_check")
)

#' @rdname family_check
#' @include make_famid.R
#' @aliases family_check,character
#' @export
setMethod("family_check", "character",
    function(obj, dadid, momid, family, newfam) {
        id <- obj
        if (is.numeric(family) && any(is.na(family))) {
            stop("Family id of missing not allowed")
        }
        nfam <- length(unique(family))

        if (missing(newfam)) {
            newfam <- make_famid(id, dadid, momid)
        } else if (length(newfam) != length(family)) {
            stop("Invalid length for newfam")
        }

        xtab <- table(family, newfam)
        if (any(newfam == 0)) {
            unrelated <- xtab[, 1]
            xtab <- xtab[, -1, drop = FALSE]
            ## bug fix suggested by Amanda Blackford 6/2011
        } else {
            unrelated <- rep(0, nfam)
        }

        splits <- apply(xtab > 0, 1, sum)
        joins <- apply(xtab > 0, 2, sum)

        temp <- apply((xtab > 0) * outer(rep(1, nfam), joins - 1), 1, sum)

        out <- data.frame(family = dimnames(xtab)[[1]],
            n = as.vector(table(family)), unrelated = as.vector(unrelated),
            split = as.vector(splits), join = temp, row.names = seq_len(nfam)
        )
        if (any(joins > 1)) {
            tab1 <- xtab[temp > 0, ]  # families with multiple outcomes
            # only keep non-zero columns
            tab1 <- tab1[, apply(tab1 > 0, 2, sum) > 0]
            dimnames(tab1) <- list(dimnames(tab1)[[1]], NULL)
            attr(out, "join") <- tab1
        }
        out
    }
)

#' @rdname family_check
#' @docType methods
#' @aliases family_check,Pedigree
setMethod("family_check", "Pedigree",
    function(obj) {
        family_check(obj$ped$id, obj$ped$dadid, obj$ped$momid, obj$ped$family)
    }
)
