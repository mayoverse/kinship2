#' Find informative individuals
#'
#' @description Select the ids of the informative individuals.
#'
#' @details Depending on the **informative** parameter, the function will
#' extract the ids of the informative individuals. In the case of a
#' numeric vector, the function will return the same vector.
#' In the case of a boolean, the function will return the ids of the
#' individuals if TRUE, NA otherwise.
#' In the case of a string, the function will return the ids of the
#' corresponding informative individuals based on the avail and affected
#' columns.
#'
#' @inheritParams Ped
#' @param informative Informative individuals selection can take 5 values:
#' - 'AvAf' (available and affected),
#' - 'AvOrAf' (available or affected),
#' - 'Av' (available only),
#' - 'Af' (affected only),
#' - 'All' (all individuals)
#' - A numeric/character vector of individuals id
#' - A boolean
#' @inheritParams generate_colors
#' @return
#'
#' ## When obj is a vector
#' A vector of individuals informative identifiers.
#'
#' ## When obj is a Pedigree
#' The Pedigree object with its `isinf` slot updated.
#'
#' @export
#' @docType methods
#' @usage NULL
setGeneric("is_informative", signature = "obj",
    function(obj, ...) standardGeneric("is_informative")
)

#' @rdname is_informative
#' @examples
#'
#' is_informative(c("A", "B", "C", "D", "E"), informative = c("A", "B"))
#' is_informative(c("A", "B", "C", "D", "E"), informative = c(1, 2))
#' is_informative(c("A", "B", "C", "D", "E"), informative = c("A", "B"))
#' is_informative(c("A", "B", "C", "D", "E"), avail = c(1, 0, 0, 1, 1),
#'   affected = c(0, 1, 0, 1, 1), informative = "AvAf")
#' is_informative(c("A", "B", "C", "D", "E"), avail = c(1, 0, 0, 1, 1),
#'   affected = c(0, 1, 0, 1, 1), informative = "AvOrAf")
#' is_informative(c("A", "B", "C", "D", "E"),
#'      informative = c(TRUE, FALSE, TRUE, FALSE, TRUE))
#' @export
setMethod("is_informative", "character_OR_integer", function(
    obj, avail, affected, informative = "AvAf"
) {
    id <- obj
    # Selection of all informative individuals depending of the informative
    # parameter
    if (length(informative) > 1) {
        if (is.character(informative)) {
            id_inf <- id[match(id, informative, nomatch = 0) != 0]
        } else if (is.numeric(informative)) {
            id_inf <- id[informative]
        } else if (is.logical(informative)) {
            if (length(informative) != length(id)) {
                stop("The length of a logical informative parameter must be",
                    "equal to the length of the id vector"
                )
            }
            id_inf <- id[informative]
        } else {
            stop("The informative parameter must be a character, ",
                "logical or numeric"
            )
        }
    } else if (is.numeric(informative)) {
        id_inf <- id[informative]
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

#' @rdname is_informative
#' @param reset If `TRUE`, the `isinf` slot is reset
#' @examples
#'
#' data("sampleped")
#' ped <- Pedigree(sampleped)
#' ped <- is_informative(ped, col_aff = "affection_mods")
#' isinf(ped(ped))
#' @export
setMethod("is_informative", "Pedigree", function(
    obj, col_aff = NULL, informative = "AvAf", reset = FALSE
) {
    if (!reset & any(!is.na(isinf(ped(obj))))) {
        warning(
            "The isinf slot already has values in the Ped object",
            " and reset is set to FALSE"
        )
        return(obj)
    }
    affected(ped(obj)) <- NA
    aff_scl <- fill(obj)
    ped_df <- as.data.frame(ped(obj))
    if (is.null(col_aff)) {
        stop("The col_aff argument is required")
    }
    # TODO use the affected columns
    if (col_aff %in% aff_scl$column_mods) {
        aff <- aff_scl$mods[aff_scl$affected == TRUE &
                aff_scl$column_mods == col_aff
        ]
        unaff <- aff_scl$mods[aff_scl$affected == FALSE &
                aff_scl$column_mods == col_aff
        ]
        ped_df$affected[ped_df[, col_aff] %in% aff] <- 1
        ped_df$affected[ped_df[, col_aff] %in% unaff] <- 0
    } else {
        stop("The column ", col_aff, " is not in the scales fill")
    }

    cols_needed <- c("id", "avail", "affected")
    check_columns(ped_df, cols_needed, "", "", others_cols = TRUE)
    id_inf <- is_informative(ped_df$id, ped_df$avail, ped_df$affected,
        informative = informative
    )

    isinf(ped(obj)) <- vect_to_binary(
        ifelse(ped_df$id %in% id_inf, 1, 0), logical = TRUE
    )
    obj
})
