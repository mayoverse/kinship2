#' Create a Pedigree object
#'
#' This constructor help to create a `Pedigree` object from
#' different `data.frame` or a set of vectors.
#'
#' If any errors are found in the data, the function will return
#' the data.frame with the errors for the Pedigree and the relationship
#' data.frame.
#'
#' @inheritParams align
#' @param obj A vector of the individuals identifiers or a data.frame
#' with the individuals informations.
#' The minimum columns required are `indID`, `fatherId`, `motherId` and
#' `gender`.
#' The `family` column can also be used to specify the family of the
#' individuals and will be merge to the `id` field separated by an
#' underscore.
#' The following columns are also recognize `sterilisation`, `available`,
#' `vitalStatus`, `affection`. The four of them will be transformed with the
#' [vect_to_binary()] function when the normalisation is selected and will
#' be set respectively to `steril`, `avail`,
#' `status` and `affected`.
#' If you do not use the normalisation, the columns will be checked to
#' be `0` or `1`.
#' They respectively correspond to the sterilisation status,
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
#' If `famid` is given in the call to create Pedigrees, then
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
#' @examples
#' data(sampleped)
#' ped1 <- Pedigree(sampleped[sampleped$family == "1",])
#' @export
setGeneric("Pedigree", signature = "obj",
    function(obj, ...) standardGeneric("Pedigree")
)

#' @export
#' @rdname Pedigree
#' @aliases Pedigree,numeric
#' @docType methods
setMethod("Pedigree", "numeric", function(obj, ...
) {
    Pedigree(as.character(obj), ...)
})

#' @export
#' @rdname Pedigree
#' @aliases Pedigree,character
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
#' @param steril A numeric vector of sterilisation status of each individual
#' (e.g., genotyped). The values are:
#' - `0`  : not sterilised
#' - `1`  : sterilised
#' - `NA` : sterilisation status not known
setMethod("Pedigree", "character", function(obj, dadid, momid,
    sex, family = NA, avail = NULL, affected = NULL, status = NULL,
    steril = NULL, relation =  NULL,
    missid = "0", col_aff = "affection", normalize = TRUE, ...
) {
    n <- length(obj)
    ## Code transferred from noweb to markdown vignette.
    ## Sections from the noweb/vignettes are noted here with
    ## Doc: Error and Data Checks
    ## Doc: Errors1
    if (length(momid) != n) stop("Mismatched lengths, id and momid")
    if (length(dadid) != n) stop("Mismatched lengths, id and momid")
    if (length(sex) != n) stop("Mismatched lengths, id and sex")
    if (length(steril) != n & !is.null(steril)) {
        stop("Mismatched lengths, id and steril")
    }

    if (length(avail) != n & !is.null(avail)) {
        stop("Mismatched lengths, id and avail")
    }
    if (length(status) != n & !is.null(status)) {
        stop("Mismatched lengths, id and status")
    }


    ped_df <- data.frame(
        family = family,
        indId = obj,
        fatherId = dadid,
        motherId = momid,
        gender = sex
    )

    if (is.null(affected)) {
        ped_df$affection <- NA
    } else if (any(!is.na(affected))) {
        if (is.vector(affected)) {
            ped_df$affection <- affected
        } else if (is.data.frame(affected)) {
            ped_df <- cbind(ped_df, affected)
            col_aff <- colnames(affected)
        } else {
            stop("Affected must be a vector or a data.frame")
        }
    }
    if (any(!is.na(avail))) {
        ped_df$available <- avail
    }
    if (any(!is.na(status))) {
        ped_df$vitalstatus <- status
    }
    if (any(!is.na(steril))) {
        ped_df$sterilisation <- steril
    }
    if (is.null(relation)) {
        relation <- data.frame(
            id1 = character(),
            id2 = character(),
            code = numeric(),
            family = character()
        )
    }
    Pedigree(ped_df, relation = relation,
        missid = missid, col_aff = col_aff, ...
    )
})

#' @export
#' @rdname Pedigree
#' @aliases Pedigree,data.frame
#' @docType methods
setMethod("Pedigree", "data.frame",  function(
    obj = data.frame(
        indId = character(),
        fatherId = character(),
        motherId = character(),
        gender = numeric(),
        family = character(),
        available = numeric(),
        vitalstatus = numeric(),
        affection = numeric(),
        sterilisation = numeric()
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
        "gender" = "sex",
        "sterilisation" = "steril",
        "affection" = "affected",
        "available" = "avail",
        "vitalstatus" = "status"
    ),
    cols_ren_rel = list(
        "id1" = "indId1",
        "id2" = "indId2"
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
        horder = NULL,
        spouse = NULL
    ),
    normalize = TRUE,
    missid = "0",
    col_aff = "affection",
    ...
) {
    ped_df <- obj
    if (!is.data.frame(ped_df)) {
        stop("ped_df must be a data.frame")
    }

    if (is.matrix(relation)) {
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
        warning("The Pedigree informations are not valid.",
            "Here is the normalised Pedigree informations",
            "with the identified problems"
        )
        return(ped_df)
    }

    if (any(!is.na(rel_df$error))) {
        warning("The relationship informations are not valid.",
            "Here is the normalised relationship informations",
            "with the identified problems"
        )
        return(rel_df)
    }

    cols <- colnames(ped_df)
    col_ped <- c("id", "dadid", "momid", "sex", "family")
    col_deriv <- c("affected", "kin", "useful", "avail", "steril", "status")
    col_rel <- c("id1", "id2", "code", "family")
    col_meta <- cols[!(cols %in% c(col_ped, col_deriv))]
    ped <- ped_df[, col_ped]
    deriv <- ped_df[, col_deriv]
    meta <- ped_df[, col_meta]
    rel <- rel_df[, col_rel]

    rownames(ped) <- ped_df$id
    rownames(deriv) <- ped_df$id
    rownames(meta) <- ped_df$id

    ## Create the object
    ped <- new("Pedigree",
        ped = ped, deriv = deriv, meta = meta, rel = rel,
        scales = scales, hints = hints
    )

    generate_colors(ped, col_aff = col_aff, ...)
}
)
