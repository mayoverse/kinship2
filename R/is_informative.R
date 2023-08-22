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
#' @param id A vector of individuals id
#' @param avail A vector of individuals availability (0, 1 or NA)
#' @param affected A vector of individuals affected status (0, 1 or NA)
#' @param column A string with the column name to use for the affected status.
#' @param informative Informative individuals selection can take 5 values:
#' 'AvAf' (available and affected),
#' 'AvOrAf' (available or affected),
#' 'Av' (available only),
#' 'Af' (affected only),
#' 'All' (all individuals)
#' or a numeric/character vector of individuals id
#' or a boolean
#'
#' @export
setGeneric("is_informative", signature = "obj",
    function(obj, ...) standardGeneric("is_informative")
)

setMethod("is_informative", "character",
    function(
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
    }
)

setMethod("is_informative", "data.frame",
    function(obj, informative = "AvAf", missid = "0") {
        cols_needed <- c("id", "avail", "affected")
        obj <- check_columns(obj, cols_needed, "", "", others_cols = TRUE)
        is_informative(obj$id, obj$avail, obj$affected, informative, missid)
    }
)

setMethod("is_informative", "Pedigree", function(
    obj, column = "affected", informative = "AvAf", missid = "0"
) {
    obj$ped$affected <- NA
    aff_scl <- obj$scales$fill
    if (column %in% aff_scl$column) {
        aff <- aff_scl$mods[aff_scl$affected == TRUE &
                aff_scl$column == column
        ]
        unaff <- aff_scl$mods[aff_scl$affected == FALSE &
                aff_scl$column == column
        ]
        obj$ped$affected[obj$ped[, column] %in% aff] <- 1
        obj$ped$affected[obj$ped[, column] %in% unaff] <- 0
    } else {
        stop("The column ", column, " is not in the scales fill")
    }
    id_inf <- is_informative(obj$ped, informative = informative, missid)

    check_columns(obj$ped, NULL, NULL, "id_inf")

    obj$ped$inf <- ifelse(obj$ped$id %in% id_inf, 1, 0)
    list(ped = obj, inf = id_inf)
})
