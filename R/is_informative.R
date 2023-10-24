#' @title Is informative
#'
#' @description Select the ids of the informative individuals.
#'
#' @details Depending on the informative parameter, the function will extract
#' the ids of the informative individuals. In the case of a numeric vector,
#' the function will return the same vector. In the case of a boolean, the
#' function will return the ids of the individuals if TRUE, NA otherwise.
#' In the case of a string, the function will return the ids of the
#' corresponding informative individuals based on the avail and affected
#' columns.
#'
#' @param avail A numeric vector of availability status of each individual
#' (e.g., genotyped). The values are:
#' - `0`  : unavailable
#' - `1`  : available
#' - `NA` : availability not known
#' @param affected A numeric vector of affection status of each individual
#' (e.g., genotyped). The values are:
#' - `0`  : unaffected
#' - `1`  : affected
#' - `NA` : affection status not known
#' @param col_aff A string with the column name to use for the affection status.
#' @param informative Informative individuals selection can take 5 values:
#' - 'AvAf' (available and affected),
#' - 'AvOrAf' (available or affected),
#' - 'Av' (available only),
#' - 'Af' (affected only),
#' - 'All' (all individuals)
#' - A numeric/character vector of individuals id
#' - A boolean
#' @inheritParams kinship
#' @inheritParams is_parent
#' @return
#'
#' ## When obj is a vector
#' A vector of individuals informative identifiers
#'
#' ## When obj is a Pedigree
#' A list containing the Pedigree object and the vector of individuals
#' identifiers.
#' The Pedigree object will have a new column named 'id_inf' containing 1 for
#' informative individuals and 0 otherwise.
#'
#' @examples
#' data("sampleped")
#' ped <- Pedigree(sampleped)
#' is_informative(ped, col_aff = "affection_aff")
#'
#' @export
#' @docType methods
setGeneric("is_informative", signature = "obj",
    function(obj, ...) standardGeneric("is_informative")
)

#' @export
#' @rdname is_informative
#' @aliases is_informative,character
#' @docType methods
setMethod("is_informative", "character", function(
    obj, avail, affected, informative = "AvAf", missid = "0"
) {
    id <- obj
    # Selection of all informative individuals depending of the informative
    # parameter
    if (is.numeric(informative)) {
        id_inf <- id[match(id, informative, nomatch = 0) != 0]
    } else if (is.logical(informative)) {
        id_inf <- ifelse(informative, id, NA)
        id_inf <- id_inf[!is.na(id_inf)]
    } else {
        if (informative == "AvOrAf") {
            id_inf <- id[(avail == 1 & !is.na(avail)) |
                    (affected == 1 & !is.na(affected))
            ]
        } else if (informative == "Av") {
            id_inf <- id[avail == 1 & !is.na(avail)]
        } else if (informative == "Af") {
            id_inf <- id[affected == 1 & !is.na(affected)]
        } else if (informative == "AvAf") {
            id_inf <- id[(avail == 1 & !is.na(avail)) &
                    (affected == 1 & !is.na(affected))
            ]
        } else if (informative == "All") {
            id_inf <- id
        } else {
            id_inf <- id[match(id, informative, nomatch = 0) != 0]
        }
    }
    unique(id_inf)
})

#' @export
#' @rdname is_informative
#' @aliases is_informative,Pedigree
#' @docType methods
#' @param reset If `TRUE`, the `id_inf` column is reset
setMethod("is_informative", "Pedigree", function(
    obj, col_aff = NULL, informative = "AvAf", missid = "0", reset = FALSE
) {
    ped <- obj
    deriv(ped, "affected") <- NA
    aff_scl <- fill(ped)
    if (is.null(col_aff)) {
        stop("The col_aff argument is required")
    }
    if (col_aff %in% aff_scl$column_mods) {
        aff <- aff_scl$mods[aff_scl$affected == TRUE &
                aff_scl$column_mods == col_aff
        ]
        unaff <- aff_scl$mods[aff_scl$affected == FALSE &
                aff_scl$column_mods == col_aff
        ]
        obj$ped$affected[obj$ped[, col_aff] %in% aff] <- 1
        obj$ped$affected[obj$ped[, col_aff] %in% unaff] <- 0
    } else {
        stop("The column ", col_aff, " is not in the scales fill")
    }

    cols_needed <- c("id", "avail", "affected")
    obj$ped <- check_columns(obj$ped, cols_needed, "", "", others_cols = TRUE)
    id_inf <- is_informative(obj$ped$id, obj$ped$avail, obj$ped$affected,
        informative, missid
    )

    if (!reset) {
        check_columns(obj$ped, NULL, "id_inf", NULL)
    }

    obj$ped$id_inf <- ifelse(obj$ped$id %in% id_inf, 1, 0)
    obj
})
