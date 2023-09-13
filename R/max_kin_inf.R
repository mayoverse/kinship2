#' @importFrom dplyr %>%
NULL

#' @title Kinship computation
#'
#' @description Compute the kinship between the informative individuals and
#' all the others.
#'
#' @param obj A pedigree object, a dataframe or a vector of the individuals
#' @param dadid A vector of individuals dadid
#' @param momid A vector of individuals momid
#' @param sex A vector of the individuals sex
#' @param avail A vector of individuals availability (0, 1 or NA)
#' @param affected A vector of individuals affected status (0, 1 or NA)
#' @param informative Informative individuals selection can take 3 values:
#' 'AvAf' (available and affected),
#' 'AvOrAf' (available or affected),
#' 'Av' (available only),
#' 'Af' (affected only),
#' 'All' (all individuals)
#' or a numeric vector of individuals id
#' or a boolean
#' @param ... Other parameters passed to [is_informative()]
#' when using a pedigree object
#'
#' @return
#' ## When obj is a vector or a dataframe
#' A vector of kinship degree
#'
#' ## When obj is a Pedigree
#' The Pedigree object with a new column named 'kin' containing the kinship
#' degree.
#'
#' @examples
#' data(sampleped)
#' ped <- pedigree(sampleped)
#' max_kin_inf(ped, column = "affected_aff")$ped
#'
#' @include is_informative.R
#' @include kinship.R
#' @docType methods
#' @export
setGeneric("max_kin_inf", signature = "obj",
    function(obj, ...) standardGeneric("max_kin_inf")
)

#' @export
#' @rdname max_kin_inf
#' @aliases max_kin_inf,character
#' @docType methods
setMethod("max_kin_inf", "character", function(
    obj, dadid, momid, sex, avail, affected, informative = "AvAf"
) {
    id <- obj
    # Selection of all informative individuals depending of the informative
    # parameter
    id_inf <- is_informative(id, avail, affected, informative)
    if (any(is.na(id_inf)) || length(id_inf) == 0) {
        stop("No informative individuals detected")
    }
    # For all individuals, compute kinship degree
    mat <- as.matrix(kinship(id, dadid, momid, sex))
    sub <- mat[, colnames(mat) %in% id_inf] %>%
        as.data.frame()

    kin <- log2(1 / apply(sub, 1, max))
    kin[is.infinite(kin)] <- NA

    checked <- length(kin[!is.na(kin)])

    kin
})

#' @export
#' @rdname max_kin_inf
#' @aliases max_kin_inf,data.frame
#' @docType methods
setMethod("max_kin_inf", "data.frame",
    function(obj, informative = "AvAf") {
        cols_needed <- c("id", "dadid", "momid", "sex", "avail", "affected")

        df <- check_columns(obj, cols_needed, "", "", others_cols = TRUE)

        max_kin_inf(
            df$id, informative = informative,
            df$dadid, df$momid, df$sex, df$avail, df$affected
        )
    }
)

#' @export
#' @rdname max_kin_inf
#' @aliases max_kin_inf,Pedigree
#' @docType methods
#' @param reset If TRUE, the `kin` column is reset
setMethod("max_kin_inf", "Pedigree",
    function(obj, informative = "AvAf", reset = FALSE, ...) {
        ped <- is_informative(obj, informative = informative, ...)$ped
        kin <- max_kin_inf(obj$ped, informative = informative)

        if (!reset) {
            check_columns(ped$ped, NULL, "kin", NULL)
        }
        ped$ped$kin <- kin
        ped
    }
)
