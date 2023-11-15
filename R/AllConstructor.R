#' @importFrom methods callNextMethod is new 'slot<-' validObject
NULL

#' NA to specific length
#'
#' Check if all value in a vector is `NA`.
#' If so set all of them to a new value matching the length
#' of the template.
#' If not check that the size of the vector is equal to
#' the template.
#'
#' @param x The vector to check.
#' @param temp A template vector to use to determine the length.
#' @param value The value to use to fill the vector.
#'
#' @return A vector with the same length as temp.
#' @keywords internal
#' @examples
#'
#' na_to_length(NA, rep(0, 4), "NewValue")
#' na_to_length(c(1, 2, 3, NA), rep(0, 4), "NewValue")
#' @export
na_to_length <- function(x, temp, value) {
    if (length(x) == 1 && all(is.na(x))) {
        rep(value, length(temp))
    } else {
        if (length(x) != length(temp)) {
            stop("The length of the vector should be: ",
                "equal to the length of the template vector"
            )
        }
        x
    }
}

#### S4 Ped constructor ####

#' @description
#'
#' ## Constructor :
#'
#' You either need to provide a vector of the same size for each slot
#' or a `data.frame` with the corresponding columns.
#'
#' The metadata will correspond to the columns that do not correspond
#' to the Ped slots.
#'
#' @param obj A character vector with the id of the individuals or a
#' `data.frame` with all the informations in corresponding columns.
#' @param dadid A vector containing for each subject, the identifiers of the
#' biologicals fathers.
#' @param momid A vector containing for each subject, the identifiers of the
#' biologicals mothers.
#' @param famid A character vector with the family identifiers of the
#' individuals. If provide, will be aggregated to the individuals
#' identifiers separated by an underscore.
#' @param sex A character, factor or numeric vector corresponding to
#' the gender of the individuals. This will be transformed to an ordered factor
#' with the following levels: `male` < `female` < `unknown` < `terminated
#' The following values are recognized:
#' - character() or factor() : "f", "m", "woman", "man", "male", "female",
#' "unknown", "terminated"
#' - numeric() : 1 = "male", 2 = "female", 3 = "unknown", 4 = "terminated"
#' @param steril A logical vector with the sterilisation status of the
#' individuals
#' (i.e. `FALSE` = not sterilised, `TRUE` = sterilised, `NA` = unknown).
#' @param status A logical vector with the affection status of the
#' individuals
#' (i.e. `FALSE` = alive, `TRUE` = dead, `NA` = unknown).
#' @param avail A logical vector with the availability status of the
#' individuals
#' (i.e. `FALSE` = not available, `TRUE` = available, `NA` = unknown).
#' @param affected A logical vector with the affection status of the
#' individuals
#' (i.e. `FALSE` = unaffected, `TRUE` = affected, `NA` = unknown).
#' @param missid A character vector with the missing values identifiers.
#' All the id, dadid and momid corresponding to those values will be set
#' to `NA_character_`.
#' @inheritParams check_columns
#' @return A Ped object.
#' @rdname Ped-class
#' @export
#' @include utils.R
#' @usage NULL
setGeneric("Ped", signature = "obj", function(obj, ...) {
    standardGeneric("Ped")
})

#' @rdname Ped-class
#' @examples
#'
#' data(sampleped)
#' Ped(sampleped)
#' @export
setMethod("Ped", "data.frame",
    function(obj, cols_used_init = FALSE, cols_used_del = FALSE) {
        col_need <- c("id", "sex", "dadid", "momid")
        col_to_use <- c(
            "famid", "steril", "status", "avail", "affected",
            "kin", "isinf", "useful"
        )
        col_used <- c(
            "num_child_tot", "num_child_dir", "num_child_ind",
            "elementMetadata"
        )
        df <- check_columns(
            obj, col_need, col_used, col_to_use,
            others_cols = TRUE, cols_to_use_init = TRUE,
            cols_used_init = cols_used_init, cols_used_del = cols_used_del
        )

        df$famid[is.na(df$famid)] <- NA_character_

        df$steril <- vect_to_binary(df$steril, logical = TRUE)
        df$status <- vect_to_binary(df$status, logical = TRUE)
        df$avail <- vect_to_binary(df$avail, logical = TRUE)
        df$affected <- vect_to_binary(df$affected, logical = TRUE)
        myped <- with(df, Ped(
            obj = id, sex = sex, dadid = dadid, momid = momid,
            famid = famid,
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

#' @rdname Ped-class
#' @examples
#'
#' Ped(
#'     obj = c("1", "2", "3", "4", "5", "6"),
#'     dadid = c("4", "4", "6", "0", "0", "0"),
#'     momid = c("5", "5", "5", "0", "0", "0"),
#'     sex = c(1, 2, 3, 1, 2, 1),
#'     missid = "0"
#' )
#' @export
setMethod("Ped", "character_OR_integer",
    function(
        obj, sex, dadid, momid, famid = NA,
        steril = NA, status = NA, avail = NA,
        affected = NA, missid = NA_character_
    ) {
        famid <- na_to_length(famid, obj, NA_character_)
        id <- as.character(obj)
        dadid <- as.character(dadid)
        momid <- as.character(momid)

        id[id %in% missid] <- NA_character_
        dadid[dadid %in% missid] <- NA_character_
        momid[momid %in% missid] <- NA_character_

        sex <- sex_to_factor(sex)

        steril <- na_to_length(steril, id, NA)
        status <- na_to_length(status, id, NA)
        avail <- na_to_length(avail, id, NA)
        affected <- na_to_length(affected, id, NA)
        useful <- na_to_length(NA, id, NA)
        isinf <- na_to_length(NA, id, NA)
        kin <- na_to_length(NA, id, NA_real_)

        df_child <- num_child(id, dadid, momid, rel_df = NULL)

        new(
            "Ped",
            id = id, dadid = dadid, momid = momid, famid = famid,
            sex = sex, steril = steril, status = status, avail = avail,
            affected = affected,
            useful = useful, kin = kin, isinf = isinf,
            num_child_tot = df_child$num_child_tot,
            num_child_dir = df_child$num_child_dir,
            num_child_ind = df_child$num_child_ind
        )
    }
)

#' @rdname Ped-class
#' @usage NULL
#' @export
setMethod("Ped", "missing",
    function(obj) {
        new("Ped")
    }
)

#### S4 Rel constructor ####

#' @description
#'
#' ## Constructor :
#'
#' You either need to provide a vector of the same size for each slot
#' or a `data.frame` with the corresponding columns.
#'
#' @param obj A character vector with the id of the first individuals of each
#' pairs or a `data.frame` with all the informations in corresponding columns.
#' @param id2 A character vector with the id of the second individuals of each
#' pairs
#' @param code A character, factor or numeric vector corresponding to
#' the relation code of the individuals:
#' - MZ twin = Monozygotic twin
#' - DZ twin = Dizygotic twin
#' - UZ twin = twin of unknown zygosity
#' - Spouse = Spouse
#' The following values are recognized:
#' - character() or factor() : "MZ twin", "DZ twin", "UZ twin", "Spouse" with
#' of without space between the words. The case is not important.
#' - numeric() : 1 = "MZ twin", 2 = "DZ twin", 3 = "UZ twin", 4 = "Spouse"
#' @inheritParams Ped
#'
#' @return A Rel object.
#' @rdname Rel-class
#' @export
#' @usage NULL
setGeneric("Rel", signature = "obj", function(obj, ...) {
    standardGeneric("Rel")
})

#' @rdname Rel-class
#' @export
#' @examples
#'
#' rel_df <- data.frame(
#'    id1 = c("1", "2", "3"),
#'    id2 = c("2", "3", "4"),
#'    code = c(1, 2, 3)
#' )
#' Rel(rel_df)
setMethod("Rel", "data.frame",
    function(obj) {
        col_need <- c("id1", "id2", "code")
        col_to_use <- c("famid")
        df <- check_columns(
            obj, col_need, NULL, col_to_use,
            cols_to_use_init = TRUE
        )

        with(df, Rel(
            obj = id1, id2 = id2, code = code, famid = as.character(famid)
        ))
    }
)

#' @rdname Rel-class
#' @export
#' @examples
#'
#' Rel(
#'    obj = c("1", "2", "3"),
#'    id2 = c("2", "3", "4"),
#'    code = c(1, 2, 3)
#' )
setMethod("Rel", "character_OR_integer",
    function(
        obj, id2, code, famid = NA_character_
    ) {
        famid <- na_to_length(famid, obj, NA_character_)
        id1 <- as.character(obj)
        id2 <- as.character(id2)

        ## Reorder id1 and id2
        ## id1 is the first in the alphabetic order
        ## id2 is the second in the alphabetic order
        id1o <- pmin(id1, id2)
        id2o <- pmax(id1, id2)

        code <- rel_code_to_factor(code)

        rel <- new(
            "Rel",
            id1 = id1o, id2 = id2o, code = code, famid = famid
        )
        rel
    }
)

#' @rdname Rel-class
#' @usage NULL
#' @export
setMethod("Rel", "missing",
    function(obj) {
        new("Rel")
    }
)

#### S4 Hints constructor ####

#' @description
#'
#' ## Constructor :
#'
#' You either need to provide **horder** or **spouse** in
#' the dedicated parameters (together or separately), or inside a list.
#'
#' @param horder A named numeric vector with one element per subject in the
#' Pedigree.  It determines the relative horizontal order of subjects within a
#' sibship, as well as the relative order of processing for the founder couples.
#' (For this latter, the female founders are ordered as though
#' they were sisters).
#' The names of the vector should be the individual identifiers.
#' @param spouse A data.frame with one row per hinted marriage, usually only
#' a few marriages in a pedigree will need an added hint, for instance reverse
#' the plot order of a husband/wife pair.
#' Each row contains the id of the left spouse (i.e. `idl`), the id of the
#' right hand spouse (i.e. `idr`), and the anchor (i.e : `anchor` :
#' `1` = left, `2` = right, `0` = either).
#' Children will preferentially appear under the parents of the anchored spouse.
#'
#' @return A Hints object.
#' @rdname Hints-class
#' @export
setGeneric("Hints", function(horder, spouse) {
    standardGeneric("Hints")
})

#' @rdname Hints-class
#' @usage NULL
#' @export
setMethod("Hints",
    signature(horder = "Hints", spouse = "missing_OR_NULL"),
    function(horder, spouse) {
        hints
    }
)

#' @rdname Hints-class
#' @export
#' @examples
#'
#' Hints(
#'   list(
#'       horder = c("1" = 1, "2" = 2, "3" = 3),
#'       spouse = data.frame(
#'           idl = c("1", "2"),
#'           idr = c("2", "3"),
#'           anchor = c(1, 2)
#'      )
#'   )
#' )
setMethod("Hints",
    signature(horder = "list", spouse = "missing_OR_NULL"),
    function(horder, spouse) {
        if (all(!c("horder", "spouse") %in% names(horder))) {
            stop("hints is a list, ",
                "but doesn't contains horder or spouse slot"
            )
        }
        if ("horder" %in% names(horder)) {
            horder <- horder$horder
        } else {
            horder <- NULL
        }
        if ("spouse" %in% names(horder)) {
            spouse <- horder$spouse
        } else {
            spouse <- NULL
        }
        Hints(horder, spouse)
    }
)

#' @rdname Hints-class
#' @export
#' @examples
#'
#' Hints(
#'   horder = c("1" = 1, "2" = 2, "3" = 3),
#'   spouse = data.frame(
#'       idl = c("1", "2"),
#'       idr = c("2", "3"),
#'       anchor = c(1, 2)
#'   )
#' )
setMethod("Hints",
    signature(horder = "numeric", spouse = "data.frame"),
    function(horder, spouse) {
        if (length(horder) > 0 && (
            is.null(names(horder)) || any(!is.numeric(horder))
        )) {
            stop("horder must be a named numeric vector")
        }
        spouse <- check_columns(
            spouse, c("idl", "idr", "anchor"), NULL, NULL,
            cols_to_use_init = TRUE
        )
        spouse$anchor <- anchor_to_factor(spouse$anchor)
        new("Hints", horder = horder, spouse = spouse)
    }
)

#' @rdname Hints-class
#' @export
#' @examples
#'
#' Hints(
#'   horder = c("1" = 1, "2" = 2, "3" = 3)
#' )
setMethod("Hints",
    signature(horder = "numeric", spouse = "missing_OR_NULL"),
    function(horder, spouse) {
        if (length(horder) > 0 && (
            is.null(names(horder)) || any(!is.numeric(horder))
        )) {
            stop("horder must be a named numeric vector")
        }
        dfe <- data.frame("idl" = character(), "idr" = character(),
            "anchor" = factor()
        )
        new("Hints", horder = horder, spouse = dfe)
    }
)

#' @rdname Hints-class
#' @export
#' @usage NULL
setMethod("Hints",
    signature(horder = "missing_OR_NULL", spouse = "missing_OR_NULL"),
    function(horder, spouse) {
        dfe <- data.frame("idl" = character(), "idr" = character(),
            "anchor" = factor()
        )
        new("Hints", horder = numeric(), spouse = dfe)
    }
)

#### S4 Scales constructor ####

#' @description
#'
#' ## Constructor :
#'
#' You need to provide both **fill** and **border** in the dedicated parameters.
#' However this is usually done using the [generate_colors()] function with a
#' Pedigree object.
#'
#' @param fill A data.frame with the informations for the affection status.
#' The columns needed are:
#' - 'order': the order of the affection to be used
#' - 'column_values': name of the column containing the raw values in the
#' Ped object
#' - 'column_mods': name of the column containing the mods of the transformed
#' values in the Ped object
#' - 'mods': all the different mods
#' - 'labels': the corresponding labels of each mods
#' - 'affected': a logical value indicating if the mod correspond to an affected
#' individuals
#' - 'fill': the color to use for this mods
#' - 'density': the density of the shading
#' - 'angle': the angle of the shading
#' @param border A data.frame with the informations for the availability status.
#' The columns needed are:
#' - 'column_values': name of the column containing the raw values in the
#' Ped object
#' - 'column_mods': name of the column containing the mods of the transformed
#' values in the Ped object
#' - 'mods': all the different mods
#' - 'labels': the corresponding labels of each mods
#' - 'border': the color to use for this mods
#'
#' @return A Scales object.
#' @seealso [generate_colors()]
#' @rdname Scales-class
#' @export
setGeneric("Scales", function(fill, border) {
    standardGeneric("Scales")
})

#' @rdname Scales-class
#' @export
#' @examples
#'
#' Scales(
#'    fill = data.frame(
#'       order = 1,
#'       column_values = "affected",
#'       column_mods = "affected_mods",
#'       mods = c(0, 1),
#'       labels = c("unaffected", "affected"),
#'       affected = c(FALSE, TRUE),
#'       fill = c("white", "red"),
#'       density = c(NA, 20),
#'       angle = c(NA, 45)
#'    ),
#'    border = data.frame(
#'       column_values = "avail",
#'       column_mods = "avail_mods",
#'       mods = c(0, 1),
#'       labels = c("not available", "available"),
#'       border = c("black", "blue")
#'    )
#' )
setMethod("Scales",
    signature(fill = "data.frame", border = "data.frame"),
    function(fill, border) {
        fill <- check_columns(
            fill, c(
                "order", "column_values", "column_mods", "mods",
                "labels", "affected", "fill", "density", "angle"
            ), NULL, NULL
        )
        border <- check_columns(
            border,
            c("column_values", "column_mods", "mods", "labels", "border"),
            NULL, NULL
        )
        new("Scales", fill = fill, border = border)
    }
)

#' @rdname Scales-class
#' @export
#' @usage NULL
setMethod("Scales",
    signature(fill = "missing", border = "missing"),
    function(fill, border) {
        fill <- data.frame(
            order = numeric(),
            column_values = character(),
            column_mods = character(),
            mods = numeric(),
            labels = character(),
            affected = logical(),
            fill = character(),
            density = numeric(),
            angle = numeric()
        )
        border <- data.frame(
            column_values = character(),
            column_mods = character(),
            mods = numeric(),
            labels = character(),
            border = character()
        )
        new("Scales", fill = fill, border = border)
    }
)

#### S4 Pedigree constructor ####

#' @description
#'
#' ## Constructor :
#'
#' Main constructor of the package.
#' This constructor help to create a `Pedigree` object from
#' different `data.frame` or a set of vectors.
#'
#' If any errors are found in the data, the function will return
#' the data.frame with the errors of the Ped object and the
#' Rel object.
#'
#' @details
#' If the normalization is set to `TRUE`, then the data will be
#' standardized using the function `norm_ped()` and `norm_rel()`.
#'
#' If a data.frame is given, the columns names needed will depend if
#' the normalization is selected or not. If the normalization is selected,
#' the columns names needed are as follow and if not the columns names
#' needed are in parenthesis:
#' - `indID`: the individual identifier (`id`)
#' - `fatherId`: the identifier of the biological father (`dadid`)
#' - `motherId`: the identifier of the biological mother (`momid`)
#' - `gender`: the sex of the individual (`sex`)
#' - `family`: the family identifier of the individual (`famid`)
#' - `sterilisation`: the sterilisation status of the individual (`steril`)
#' - `available`: the availability status of the individual (`avail`)
#' - `vitalStatus`: the death status of the individual (`status`)
#' - `affection`: the affection status of the individual (`affected`)
#' - `...`: other columns that will be stored in the `elementMetadata` slot
#'
#' The minimum columns required are :
#' - `indID` / `id`
#' - `fatherId` / `dadid`
#' - `motherId` / `momid`
#' - `gender` / `sex`
#'
#' The `family` / `famid` column can also be used to specify the family of the
#' individuals and will be merge to the `indId` / `id` field separated by an
#' underscore.
#' The columns `sterilisation`, `available`, `vitalStatus`, `affection`
#' will be transformed with the [vect_to_binary()] function when the
#' normalisation is selected.
#' If you do not use the normalisation, the columns will be checked to
#' be `0` or `1`.
#'
#' @param obj A vector of the individuals identifiers or a data.frame
#' with the individuals informations. See [Ped()] for more informations.
#' @param rel_df A data.frame with the special relationships between
#' individuals. See [Rel()] for more informations.
#' The minimum columns required are `id1`, `id2` and `code`.
#' The `famid` column can also be used to specify the family
#' of the individuals.
#' If a matrix is given, the columns needs to be ordered as
#' `id1`, `id2`, `code` and `famid`.
#' The code values are:
#' - `1` = Monozygotic twin
#' - `2` = Dizygotic twin
#' - `3` = twin of unknown zygosity
#' - `4` = Spouse
#'
#' The value relation code recognized by the function are the one defined
#' by the [rel_code_to_factor()] function.
#'
#' @param hints A Hints object or a named list containing `horder` and
#' `spouse`.
#' @param cols_ren_ped A named list with the columns to rename for the
#' pedigree dataframe. This is useful if you want to use a dataframe with
#' different column names. The names of the list should be the new column
#' names and the values should be the old column names. The default values
#' are to be used with `normalize = TRUE`.
#' @param cols_ren_rel A named list with the columns to rename for the
#' relationship matrix. This is useful if you want to use a dataframe with
#' different column names. The names of the list should be the new column
#' names and the values should be the old column names.
#' @param normalize A logical to know if the data should be normalised.
#' @inheritDotParams generate_colors
#' @inheritParams Ped
#' @inheritParams is_informative
#'
#' @return A Pedigree object.
#' @export
#' @rdname Pedigree-class
#' @seealso [Ped()], [Rel()], [Scales()]
setGeneric("Pedigree", signature = "obj",
    function(obj, ...) standardGeneric("Pedigree")
)

#' @export
#' @rdname Pedigree-class
#' @param affected A logical vector with the affection status of the
#' individuals
#' (i.e. `FALSE` = unaffected, `TRUE` = affected, `NA` = unknown).
#' Can also be a data.frame with the same length as `obj`. If it is a
#' matrix, it will be converted to a data.frame and the columns will be
#' named after the `col_aff` argument.
#' @details
#' If `affected` is a data.frame, **col_aff** will be overwritten by the column
#' names of the data.frame.
#' @inheritParams generate_colors
#' @examples
#'
#' Pedigree(
#'    obj = c("1", "2", "3", "4", "5", "6"),
#'    dadid = c("4", "4", "6", "0", "0", "0"),
#'    momid = c("5", "5", "5", "0", "0", "0"),
#'    sex = c(1, 2, 3, 1, 2, 1),
#'    avail = c(0, 1, 0, 1, 0, 1),
#'    affected = matrix(c(
#'        0, 1, 0, 1, 0, 1,
#'        1, 1, 1, 1, 1, 1
#'    ), ncol = 2),
#'    col_aff = c("aff1", "aff2"),
#'    missid = "0",
#'    rel_df = matrix(c(
#'       "1", "2", 2
#'    ), ncol = 3, byrow = TRUE),
#' )
setMethod("Pedigree", "character_OR_integer", function(obj, dadid, momid,
    sex, famid = NA, avail = NULL, affected = NULL, status = NULL,
    steril = NULL, rel_df =  NULL,
    missid = NA_character_, col_aff = "affection", normalize = TRUE, ...
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
        family = famid,
        indId = obj,
        fatherId = dadid,
        motherId = momid,
        gender = sex
    )

    if (is.null(affected)) {
        ped_df[col_aff] <- NA
    } else if (any(!is.na(affected))) {
        if (is.vector(affected)) {
            ped_df[col_aff] <- affected
        } else if (is.data.frame(affected)) {
            ped_df <- cbind(ped_df, affected)
            col_aff <- colnames(affected)
        } else if (is.matrix(affected)) {
            affected <- as.data.frame(affected)
            if (is.null(colnames(affected))) {
                if (length(col_aff) != ncol(affected)) {
                    stop("The length of col_aff should be equal to the number",
                        "of columns of affected"
                    )
                }
                colnames(affected) <- col_aff
            }
            ped_df <- cbind(ped_df, affected)
            col_aff <- colnames(affected)
        } else {
            stop("Affected must be a vector or a data.frame, got:",
                class(affected)
            )
        }
    }
    if (any(!is.na(avail))) {
        ped_df$available <- avail
    }
    if (any(!is.na(status))) {
        ped_df$vitalStatus <- status
    }
    if (any(!is.na(steril))) {
        ped_df$sterilisation <- steril
    }
    if (is.null(rel_df)) {
        rel_df <- data.frame(
            id1 = character(),
            id2 = character(),
            code = numeric(),
            family = character()
        )
    }
    Pedigree(ped_df, rel_df = rel_df,
        missid = missid, col_aff = col_aff, ...
    )
})

#' @export
#' @rdname Pedigree-class
#' @examples
#'
#' data(sampleped)
#' Pedigree(sampleped)
setMethod("Pedigree", "data.frame",  function(
    obj = data.frame(
        indId = character(),
        fatherId = character(),
        motherId = character(),
        gender = numeric(),
        family = character(),
        available = numeric(),
        vitalStatus = numeric(),
        affection = numeric(),
        sterilisation = numeric()
    ),
    rel_df = data.frame(
        id1 = character(),
        id2 = character(),
        code = numeric(),
        famid = character()
    ),
    cols_ren_ped = list(
        indId = "id",
        fatherId = "dadid",
        motherId = "momid",
        family = "famid",
        gender = "sex",
        sterilisation = "steril",
        affection = "affected",
        available = "avail",
        vitalStatus = "status"
    ),
    cols_ren_rel = list(
        id1 = "indId1",
        id2 = "indId2",
        famid = "family"
    ),
    hints = list(
        horder = NULL,
        spouse = NULL
    ),
    normalize = TRUE,
    missid = NA_character_,
    col_aff = "affection",
    ...
) {
    ped_df <- obj
    if (!is.data.frame(ped_df)) {
        stop("ped_df must be a data.frame")
    }

    if (is.matrix(rel_df)) {
        rel_mat <- rel_df
        rel_df <- data.frame(
            id1 = rel_mat[, 1],
            id2 = rel_mat[, 2],
            code = rel_mat[, 3]
        )
        if (dim(rel_mat)[2] > 3) {
            rel_df$family <- rel_mat[, 4]
        }
    }

    if (!is.data.frame(rel_df)) {
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
        cols_to_use <- c("steril", "avail", "famid", "status", "affected")
        ped_df <- check_columns(
            ped_df, cols_need, "", cols_to_use,
            others_cols = TRUE, cols_to_use_init = TRUE
        )
        cols_need <- c("id1", "id2", "code")
        cols_to_use <- c("famid")
        rel_df <- check_columns(
            rel_df, cols_need, "", cols_to_use, cols_to_use_init = TRUE
        )
    }
    if (any(!is.na(ped_df$error))) {
        warning("The Pedigree informations are not valid. ",
            "Here is the normalised Pedigree informations ",
            "with the identified problems"
        )
        return(ped_df)
    }

    if (any(!is.na(rel_df$error))) {
        warning("The relationship informations are not valid. ",
            "Here is the normalised relationship informations ",
            "with the identified problems"
        )
        return(rel_df)
    }

    ped <- Ped(ped_df)
    rel <- Rel(rel_df)
    hints <- Hints(hints)
    scales <- Scales()

    ## Create the object
    ped <- new("Pedigree",
        ped = ped, rel = rel,
        hints = hints, scales = scales
    )
    generate_colors(ped, col_aff = col_aff, ...)
}
)

#' @export
#' @rdname Pedigree-class
#' @usage NULL
setMethod("Pedigree", "missing", function(obj) {
    ped <- new("Pedigree",
        ped = Ped(), rel = Rel(),
        hints = Hints(), scales = Scales()
    )
    ped
})
