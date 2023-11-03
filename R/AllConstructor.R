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
#' @param famid A character vector with the family identifiers of the
#' individuals.
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
        col_to_use <- c(
            "famid", "steril", "status", "avail", "affected",
            "kin", "id_inf", "useful"
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
        df$steril <- vect_to_binary(df$steril)
        df$status <- vect_to_binary(df$status)
        df$avail <- vect_to_binary(df$avail)
        df$affected <- vect_to_binary(df$affected)
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

#' @docType methods
#' @aliases Ped,character
#' @rdname Ped
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

        steril <- na_to_length(steril, id, NA_real_)
        status <- na_to_length(status, id, NA_real_)
        avail <- na_to_length(avail, id, NA_real_)
        affected <- na_to_length(affected, id, NA_real_)

        useful <- na_to_length(NA, id, NA_real_)
        kin <- na_to_length(NA, id, NA_real_)
        id_inf <- na_to_length(NA, id, NA_real_)

        df_child <- num_child(id, dadid, momid, rel_df = NULL)

        new(
            "Ped",
            id = id, dadid = dadid, momid = momid, famid = famid,
            sex = sex, steril = steril, status = status, avail = avail,
            affected = affected,
            useful = useful, kin = kin, id_inf = id_inf,
            num_child_tot = df_child$num_child_tot,
            num_child_dir = df_child$num_child_dir,
            num_child_ind = df_child$num_child_ind
        )
    }
)

#' @docType methods
#' @aliases Ped,missing
#' @rdname Ped
setMethod("Ped", "missing",
    function(obj) {
        new("Ped")
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
#' @param famid A character vector with the family identifiers of the
#' individuals.
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
        col_to_use <- c("famid")
        df <- check_columns(
            obj, col_need, NULL, col_to_use,
            cols_to_use_init = TRUE
        )

        with(df, Rel(
            obj = id1, id2 = id2, code = code, famid = famid
        ))
    }
)

#' @docType methods
#' @aliases Rel,character
#' @rdname Rel
#' @export
setMethod("Rel", "character_OR_integer",
    function(
        obj, id2, code, famid = NA
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

#' @docType methods
#' @aliases Rel,missing
#' @rdname Rel
setMethod("Rel", "missing",
    function(obj) {
        new("Rel")
    }
)
#### S4 Hints constructor ####
#' Create a Hints object
#'
#' @description Create a Hints object
#'
#' @param horder A vector with the order of the individuals in the pedigree.
#' @param spouse A data.frame with one row per hinted marriage, usually only
#' a few marriages in a pedigree will need an added hint, for instance reverse
#' the plot order of a husband/wife pair.
#' Each row contains the id of the left spouse (i.e. `idl`), the id of the
#' right hand spouse (i.e. `idr`), and the anchor (i.e : `anchor` :
#' `1` = left, `2` = right, `0` = either).
#' Children will preferentially appear under the parents of the anchored spouse.
#'
#' @return A Hints object.
#' @seealso [Pedigree()]
#' @rdname Hints
#' @export
setGeneric("Hints", function(horder, spouse) {
    standardGeneric("Hints")
})

#' @docType methods
#' @rdname Hints
#' @export
setMethod("Hints",
    signature(horder = "numeric", spouse = "data.frame"),
    function(horder, spouse) {
        if (length(horder) > 0 && is.null(names(horder))) {
            stop("The horder vector must be named")
        }
        new("Hints", horder = horder, spouse = spouse)
    }
)

#' @docType methods
#' @rdname Hints
#' @export
setMethod("Hints",
    signature(horder = "numeric", spouse = "missing_OR_NULL"),
    function(horder, spouse) {
        if (length(horder) > 0 && is.null(names(horder))) {
            stop("The horder vector must be named")
        }
        dfe <- data.frame("idl" = character(), "idr" = character(),
            "anchor" = factor()
        )
        new("Hints", horder = horder, spouse = dfe)
    }
)

#' @docType methods
#' @rdname Hints
#' @export
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
#' Create a Scales object
#'
#' @description Create a Scales object
#'
#' @param fill A data.frame with the informations for the affection status.
#' The columns needed are: `column_values`, `column_mods`, `mods`, `labels`,
#' `affected`, `fill`, `density` and `angle`.
#' @param border A data.frame with the informations for the availability status.
#' The columns needed are: `column`, `mods`, `labels` and `border`.
#'
#' @return A Scales object.
#' @seealso [Pedigree()]
#' @rdname Scales
#' @export
setGeneric("Scales", function(fill, border) {
    standardGeneric("Scales")
})

#' @docType methods
#' @rdname Scales
#' @export
setMethod("Scales",
    signature(fill = "data.frame", border = "data.frame"),
    function(fill, border) {
        new("Scales", fill = fill, border = border)
    }
)

#' @docType methods
#' @rdname Scales
#' @export
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
            column = character(),
            mods = numeric(),
            labels = character(),
            border = character()
        )
        new("Scales", fill = fill, border = border)
    }
)
#### S4 Pedigree constructor ####
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
#' The `famid` column can also be used to specify the family of the
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
setMethod("Pedigree", "character_OR_integer", function(obj, dadid, momid,
    sex, famid = NA, avail = NULL, affected = NULL, status = NULL,
    steril = NULL, relation =  NULL,
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
        ped_df$affection <- NA
    } else if (any(!is.na(affected))) {
        if (is.vector(affected)) {
            ped_df$affection <- affected
        } else if (is.data.frame(affected) | is.matrix(affected)) {
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
        "family" = "famid",
        "gender" = "sex",
        "sterilisation" = "steril",
        "affection" = "affected",
        "available" = "avail",
        "vitalstatus" = "status"
    ),
    cols_ren_rel = list(
        "id1" = "indId1",
        "id2" = "indId2",
        "famid" = "family"
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

    ped <- Ped(ped_df)
    rel <- Rel(rel_df)
    hints <- Hints(hints$horder, hints$spouse)
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
#' @rdname Pedigree
#' @aliases Pedigree,missing
#' @docType methods
setMethod("Pedigree", "missing", function(obj) {
    ped <- new("Pedigree",
        ped = Ped(), rel = Rel(),
        hints = Hints(), scales = Scales()
    )
    ped
})
