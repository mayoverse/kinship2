#' Create a Pedigree object
#'
#' This constructor help to create a `Pedigree` object from
#' different `data.frame` or a set of vectors.
#'
#' If any errors are found in the data, the function will return
#' the data.frame with the errors for the pedigree and the relationship
#' data.frame.
#'
#' @inheritParams align
#' @param obj A vector of the individuals identifiers or a data.frame
#' with the individuals informations.
#' The minimum columns required are `id`, `dadid`, `momid` and `sex`.
#' The `family` column can also be used to specify the family of the
#' individuals and will be merge to the `id` field separated by an
#' underscore.
#' The following columns are also recognize `steril`, `avail`, `status`,
#' `affected`. They respectively correspond to the sterilisation status,
#' the availability status, the death status and the affection status
#' of the individuals. The values recognized for those columns are `1` or
#' `0`.
#' @param relation A matrix or a data.frame with 3 required columns
#' (i.e. id1, id2, code) specifying special relationship between pairs
#' of individuals.
#' #' The code values are:
#' - `1` = Monozygotic twin
#' - `2` = Dizygotic twin
#' - `3` = twin of unknown zygosity
#' - `4` = Spouse
#'
#' If `famid` is given in the call to create pedigrees, then
#' `famid` needs to be in the last column of `relation`.
#' @param cols_ren_ped A named list with the columns to rename for the
#' pedigree dataframe.
#' @param cols_ren_rel A named list with the columns to rename for the
#' relationship matrix.
#' @param scales A list of two data.frame with the scales to use for the
#' affection status and the other one for the border color (e.g availability).
#' @param normalize A logical to know if the data should be normalised.
#' @param ... Other arguments to pass to the function `generate_colors`.
#' @return A Pedigree object.
#' @export
setGeneric("pedigree", signature = "obj",
    function(obj, ...) standardGeneric("pedigree")
)

#' @export
#' @rdname pedigree
#' @aliases pedigree,numeric
#' @docType methods
setMethod("pedigree", "numeric", function(obj, ...
) {
    pedigree(as.character(obj), ...)
})

#' @export
#' @rdname pedigree
#' @aliases pedigree,character
#' @docType methods
#' @inheritParams is_parent
#' @inheritParams sex_to_factor
#' @inheritParams family_check
#' @inheritParams is_informative
#' @param status A numeric vector of vital status of each individual
#' (e.g., genotyped). The values are:
#' - `0`  : alive
#' - `1`  : dead
#' - `NA` : vital status not known
setMethod("pedigree", "character", function(obj, dadid, momid,
    sex, family = NA, avail = NA, affected = NA, status = NA,
    relation =  NULL,
    missid = "0", col_aff = "affected", ...
) {
    n <- length(obj)
    ## Code transferred from noweb to markdown vignette.
    ## Sections from the noweb/vignettes are noted here with
    ## Doc: Error and Data Checks
    ## Doc: Errors1
    if (length(momid) != n) stop("Mismatched lengths, id and momid")
    if (length(dadid) != n) stop("Mismatched lengths, id and momid")
    if (length(sex) != n) stop("Mismatched lengths, id and sex")

    ped_df <- data.frame(
        family = family,
        id = obj,
        dadid = dadid,
        momid = momid,
        sex = sex
    )
    if (any(!is.na(affected))) {
        if (is.vector(affected)) {
            ped_df$affected <- affected
        } else {
            ped_df <- cbind(ped_df, affected)
            col_aff <- colnames(affected)
        }
    }
    if (any(!is.na(avail))) {
        ped_df$available <- avail
    }
    if (any(!is.na(status))) {
        ped_df$status <- status
    }
    if (is.null(relation)) {
        relation <- data.frame(
            id1 = character(),
            id2 = character(),
            code = numeric(),
            family = character()
        )
    }

    pedigree(ped_df, relation = relation,
        missid = missid, col_aff = col_aff, ...
    )
})

#' @export
#' @rdname pedigree
#' @aliases pedigree,data.frame
#' @docType methods
setMethod("pedigree", "data.frame",  function(
    obj = data.frame(
        id = character(),
        dadid = character(),
        momid = character(),
        sex = numeric(),
        family = character(),
        available = numeric(),
        affected = numeric()
    ),
    relation = data.frame(
        id1 = character(),
        id2 = character(),
        code = numeric(),
        family = character()
    ),
    cols_ren_ped = list(
        "indId" = "id",
        "fatherId" = "dadid",
        "motherId" = "momid",
        "gender" = "sex"
    ),
    cols_ren_rel = list(
        "indId1" = "id1",
        "indId2" = "id2"
    ),
    scales = list(
        fill = data.frame(
            order = numeric(),
            column_values = character(),
            column_mods = character(),
            mods = numeric(),
            labels = character(),
            affected = logical(),
            fill = character(),
            density = numeric(),
            angle = numeric()
        ),
        border = data.frame(
            column = character(),
            mods = numeric(),
            labels = character(),
            border = character()
        )
    ),
    hints = list(
        order = NULL,
        spouse = NULL
    ),
    normalize = TRUE,
    missid = "0",
    col_aff = "affected",
    ...
) {
    ped_df <- obj
    if (!is.data.frame(ped_df)) {
        stop("ped_df must be a data.frame")
    }

    if (is.matrix(relation)) {
        print(dim(relation)[2])
        rel_df <- data.frame(
            id1 = relation[, 1],
            id2 = relation[, 2],
            code = relation[, 3]
        )
        if (dim(relation)[2] > 3) {
            rel_df$family <- relation[, 4]
        }
    } else if (is.data.frame(relation)) {
        rel_df <- relation
    } else {
        stop("relation must be a matrix or a data.frame")
    }

    ## Rename columns ped
    old_cols <- as.vector(unlist(cols_ren_ped))
    new_cols <- names(cols_ren_ped)
    cols_to_ren <- match(old_cols, names(ped_df))
    names(ped_df)[cols_to_ren[!is.na(cols_to_ren)]] <-
        new_cols[!is.na(cols_to_ren)]

    ## Rename columns rel
    old_cols <- as.vector(unlist(cols_ren_rel))
    new_cols <- names(cols_ren_rel)
    cols_to_ren <- match(old_cols, names(rel_df))
    names(rel_df)[cols_to_ren[!is.na(cols_to_ren)]] <-
        new_cols[!is.na(cols_to_ren)]

    ## Set family, id, dadid and momid to character
    to_char <- c("family", "indId", "fatherId", "motherId")
    to_char <- colnames(ped_df)[colnames(ped_df) %in% to_char]
    ped_df[to_char] <- lapply(ped_df[to_char], as.character)

    ## Normalise the data before creating the object
    if (normalize) {
        ped_df <- norm_ped(ped_df, missid = missid)
        rel_df <- norm_rel(rel_df, missid = missid)
    } else {
        cols_need <- c("id", "dadid", "momid", "sex")
        cols_to_use <- c("steril", "avail", "family", "status", "affected")
        ped_df <- check_columns(
            ped_df, cols_need, "", cols_to_use,
            others_cols = TRUE, cols_to_use_init = TRUE
        )
        cols_need <- c("id1", "id2", "code")
        cols_to_use <- c("family")
        rel_df <- check_columns(
            rel_df, cols_need, "", cols_to_use, cols_to_use_init = TRUE
        )
    }
    if (any(!is.na(ped_df$error))) {
        warning("The pedigree informations are not valid.")
        message("Here is the normalised pedigree informations with the errors")
        return(ped_df)
    }

    if (any(!is.na(rel_df$error))) {
        warning("The relationship informations are not valid.")
        message("Here is the normalised relationship informations",
            "with the errors"
        )
        return(rel_df)
    }

    rownames(ped_df) <- ped_df$id
    ## Create the object
    ped <- new("Pedigree", ped = ped_df, rel = rel_df,
        scales = scales, hints = hints
    )

    generate_colors(ped, col_aff = col_aff, ...)
}
)
