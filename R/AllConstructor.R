#' NA to specific length
#'
#' @param x The vector to check.
#' @param temp A template vector to use to determine the length.
#' @param value The value to use to fill the vector.
#'
#' @return A vector with the same length as temp.
#' @keywords internal
na_to_length <- function(x, temp, value) {
    if (length(x) == 1 & all(is.na(x))) {
        rep(value, length(temp))
    } else {
        if (length(x) != length(temp)) {
            stop("The length of the new value should be: ",
                "equal to the length of the template vector"
            )
        }
        x
    }
}

#### S4 Ped constructor ####
#' Constructor for the Ped class
#'
#' @description Constructor for the Ped class
#' If a `data.frame` is provided, the metadata will correspond to the columns
#' that do not correspond to the Ped slots.
#'
#' @param obj A character vector with the id of the individuals or a
#' `data.frame` with all the informations in corresponding columns.
#' @param dadid A character vector with the id of the father of the individuals.
#' @param momid A character vector with the id of the mother of the individuals.
#' @param family A character vector with the family of the individuals.
#' @param sex A factor vector with the sex of the individuals (i.e. `male`,
#' `female`, `unknown` or `terminated`).
#' @param steril A numeric vector with the sterilisation status of the
#' individuals (i.e. `0` = not sterilised, `1` = sterilised, `NA` = unknown).
#' @param status A numeric vector with the affection status of the
#' individuals (i.e. `0` = alive, `1` = dead, `NA` = unknown).
#' @param avail A numeric vector with the availability status of the
#' individuals (i.e. `0` = not available, `1` = available, `NA` = unknown).
#'
#' @return A Ped object.
#' @seealso [Pedigree()]
#' @rdname Ped
#' @export
setGeneric("Ped", signature = "obj", function(obj, ...) {
    standardGeneric("Ped")
})

#' @docType methods
#' @aliases Ped,data.frame
#' @rdname Ped
setMethod("Ped", "data.frame",
    function(obj, cols_used_init = FALSE, cols_used_del = FALSE) {
        col_need <- c("id", "sex", "dadid", "momid")
        col_to_use <- c("family", "steril", "status", "avail", "affected")
        col_used <- c(
            "num_child_total", "num_child_direct", "num_child_indirect",
            "elementMetadata"
        )
        df <- check_columns(
            obj, col_need, col_used, col_to_use,
            others_cols = TRUE, cols_to_use_init = TRUE,
            cols_used_init = cols_used_init, cols_used_del = cols_used_del
        )
        df$steril <- vect_to_binary(df$steril)
        df$status <- vect_to_binary(df$status)
        df$avail <- vect_to_binary(df$avail)
        df$affected <- vect_to_binary(df$affected)

        myped <- with(df, Ped(
            obj = id, sex = sex, dadid = dadid, momid = momid,
            family = family,
            steril = steril, status = status, avail = avail,
            affected = affected
        ))
        mcols(myped) <- df[,
            colnames(df)[!colnames(df) %in% slotNames(myped)]
        ]
        rownames(mcols(myped)) <- df$id
        myped
    }
)

#' @docType methods
#' @aliases Ped,character
#' @rdname Ped
setMethod("Ped", "character",
    function(
        obj, sex, dadid, momid, family = NA,
        steril = NA, status = NA, avail = NA,
        affected = NA
    ) {
        family <- na_to_length(family, obj, NA_character_)
        id <- as.character(obj)
        dadid <- as.character(dadid)
        momid <- as.character(momid)

        sex <- sex_to_factor(sex)

        steril <- na_to_length(steril, id, NA_real_)
        status <- na_to_length(status, id, NA_real_)
        avail <- na_to_length(avail, id, NA_real_)
        affected <- na_to_length(affected, id, NA_real_)

        df_child <- num_child(id, dadid, momid, rel_df = NULL)


        new(
            "Ped",
            id = id, dadid = dadid, momid = momid, family = family,
            sex = sex, steril = steril, status = status, avail = avail,
            affected = affected,
            num_child_total = df_child$num_child_tot,
            num_child_direct = df_child$num_child_dir,
            num_child_indirect = df_child$num_child_ind
        )
    }
)

#### S4 Rel constructor ####
#' Constructor for the Rel class
#'
#' @description Constructor for the Rel class.
#'
#' @param obj A character vector with the id of the first individuals of each
#' pairs or a `data.frame` with all the informations in corresponding columns.
#' @param id2 A character vector with the id of the second individuals of each
#' pairs
#' @param code An ordered factor vector with the code of the special
#' relationship (i.e. `MZ twin` < `DZ twin` < `UZ twin` < `Spouse`).
#' @param family A character vector with the family of the individuals.
#'
#' @return A Rel object.
#' @seealso [Pedigree()]
#' @rdname Rel
#' @export
setGeneric("Rel", signature = "obj", function(obj, ...) {
    standardGeneric("Rel")
})

#' @docType methods
#' @aliases Rel,data.frame
#' @rdname Rel
#' @export
setMethod("Rel", "data.frame",
    function(obj) {
        col_need <- c("id1", "id2", "code")
        col_to_use <- c("family")
        df <- check_columns(
            obj, col_need, NULL, col_to_use,
            cols_to_use_init = TRUE
        )

        with(df, Rel(
            obj = id1, id2 = id2, code = code, family = family
        ))
    }
)

#' @docType methods
#' @aliases Rel,character
#' @rdname Rel
#' @export
setMethod("Rel", "character",
    function(
        obj, id2, code, family = NA
    ) {
        family <- na_to_length(family, obj, NA_character_)
        id1 <- as.character(obj)
        id2 <- as.character(id2)
        code <- rel_code_to_factor(code)

        rel <- new(
            "Rel",
            id1 = id1, id2 = id2, code = code, family = family
        )
        rel
    }
)