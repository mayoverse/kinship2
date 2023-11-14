#' @importFrom dplyr select one_of %>%
NULL

#' Check columns presence in a dataframe
#'
#' @description Check for presence / absence of columns names
#' depending on their need
#'
#' @details 3 types of columns are here checked:
#' - `cols_needed` : those columns need to be present if any is missing
#' an error will be prompted and the script will stop
#' - `cols_used` : those columns will be used in the script and will be
#' overwritten to NA.
#' - `cols_to_use` : those columns are optional and will be recognise
#' if present.
#' The last two types of columns can be initialised to NA if needed.
#'
#' @param df The dataframe to use
#' @param cols_needed A vector of columns needed
#' @param cols_used A vector of columns that are used by the script and
#' that will be overwritten.
#' @param cols_to_use A vector of optional columns that are authorized.
#' @param others_cols Boolean defining if non defined columns should be allowed.
#' @param cols_to_use_init Boolean defining if the optional columns should be
#' initialised to NA.
#' @param cols_used_init Boolean defining if the columns that will be used
#' should be initialised to NA.
#' @param cols_used_del Boolean defining if the columns that will be used
#' should be deleted.
#' @param verbose Should message be prompted to the user
#'
#' @return Dataframe with only the column allowed and all the column correctly
#' initialised.
#'
#' @examples
#' data.frame
#' df <- data.frame(ColN1 = c(1, 2), ColN2 = 4,
#'          ColU1 = 'B', ColU2 = '1',
#'          ColTU1 = 'A', ColTU2 = 3,
#'          ColNR1 = 4, ColNR2 = 5)
#' tryCatch(
#'      check_columns(df,
#'          c('ColN1', 'ColN2'), c('ColU1', 'ColU2'),
#'          c('ColTU1', 'ColTU2')
#' ), error = function(e) print(e))
#'
#' @keywords internal
check_columns <- function(
    df, cols_needed = NULL, cols_used = NULL, cols_to_use = NULL,
    others_cols = FALSE, cols_used_init = FALSE, cols_to_use_init = FALSE,
    cols_used_del = FALSE, verbose = FALSE
) {
    cols_p <- colnames(df)
    cols_needed_missing <- cols_needed[is.na(match(cols_needed, cols_p))]
    if (length(cols_needed_missing) > 0) {
        stop(paste(
            "Columns :", paste0(cols_needed_missing, collapse = ", "),
            "are missing. Could not continu without.\n"
        ))
    }
    cols_use_by_script <- cols_used[cols_used %in% cols_p]
    if (length(cols_use_by_script) > 0) {
        if (!cols_used_del) {
            stop(paste(
                "Columns :", paste0(cols_use_by_script, collapse = ", "),
                "are used by the script and would be overwritten.\n"
            ))
        }
        warning(paste(
            "Columns :", paste0(cols_use_by_script, collapse = ", "),
            "are used by the script and will disgarded.\n"
        ))
        cols_to_keep <- cols_p[!cols_p %in% cols_use_by_script]
        df <- df[, cols_to_keep]
    }
    if (cols_used_init) {
        if (verbose) {
            message(paste(
                "Columns :", paste0(cols_used, collapse = ", "),
                "are used by the script and will be set to NA.\n"
            ))
        }
        df[cols_used] <- ifelse(nrow(df) > 0, NA, list(character()))
    }
    cols_optional <- cols_to_use[cols_to_use %in% cols_p]
    cols_optional_abs <- cols_to_use[!cols_to_use %in% cols_p]
    if (length(cols_optional) > 0) {
        if (verbose) {
            message(paste(
                "Columns :", paste0(cols_optional, collapse = ", "),
                "where recognize and therefore will be used.\n"
            ))
        }
    }
    if (cols_to_use_init && length(cols_optional_abs) > 0) {
        if (verbose) {
            message(paste(
                "Columns :", paste0(cols_optional_abs, collapse = ", "),
                "where absent and set to NA.\n"
            ))
        }
        df[cols_optional_abs] <- ifelse(nrow(df) > 0, NA, list(character()))
    }

    if (others_cols) {
        all_cols_checked <- colnames(df)
    } else {
        if (!cols_to_use_init) {
            cols_to_use <- cols_optional
        }
        if (!cols_used_init) {
            cols_used <- NULL
        }
        all_cols_checked <- c(cols_needed, cols_to_use, cols_used)
        cols_not_recognize <- cols_p[!cols_p %in% all_cols_checked]
        if (length(cols_not_recognize) > 0 && verbose) {
            message(paste(
                "Columns :",
                paste0(cols_not_recognize, collapse = ", "),
                "not recognize and therefore will be disregarded.\n"
            ))
        }
    }

    df[all_cols_checked]
}

#'@importFrom stringr str_detect
NULL

#' Is numeric or NA
#'
#' @description Check if a variable given is numeric or NA
#'
#' @details Check if the values in **var** are numeric or if they are
#' `NA` in the case that `na_as_num` is set to TRUE.
#'
#' @param var Vector of value to test
#' @param na_as_num Boolean defining if the `NA` string should be
#' considered as numerical values
#'
#' @return A vector of boolean of the same size as **var**
#' @keywords internal
check_num_na <- function(var, na_as_num = TRUE) {
    # Should the NA value considered as numeric values
    is_num <- str_detect(var, "^\\-*[:digit:]+\\.*[:digit:]*$")
    is_na <- FALSE
    if (na_as_num) {
        is_na <- str_detect(as.character(var), "^NA$")
        is_na <- is_na | is.na(var)
    }
    is_num | is_na
}


#' Are individuals parents
#'
#' @description Check which individuals are parents.
#'
#' @param obj A vector of each subjects identifiers or a Ped object
#' @inheritParams Ped
#'
#' @return A vector of boolean of the same size as **obj**
#' with TRUE if the individual is a parent and FALSE otherwise
#' @inheritParams Ped
#' @keywords internal
#' @usage NULL
setGeneric("is_parent", signature = "obj",
    function(obj, ...) standardGeneric("is_parent")
)

#' @rdname is_parent
#' @examples
#'
#' is_parent(c("1", "2", "3", "4"), c("3", "3", NA, NA), c("4", "4", NA, NA))
#' @export
setMethod("is_parent", "character_OR_integer",
    function(obj, dadid, momid, missid = NA_character_) {
        # determine subjects who are parents assume input of
        # dadid/momid indices, not ids

        if (length(obj) != length(dadid) | length(obj) != length(momid)) {
            stop("The length of the vectors are not the same")
        }

        is_father <- !is.na(match(obj, unique(dadid[!dadid %in% missid])))
        is_mother <- !is.na(match(obj, unique(momid[!momid %in% missid])))
        is_father | is_mother
    }
)

#' @rdname is_parent
#' @examples
#'
#' data(sampleped)
#' ped <- Pedigree(sampleped)
#' is_parent(ped(ped))
#' @export
setMethod("is_parent", "Ped",
    function(obj, missid = NA_character_) {
        is_parent(id(obj), dadid(obj), momid(obj), missid)
    }
)

#' Are individuals founders
#'
#' @description Check which individuals are founders.
#'
#' @inheritParams Ped
#'
#' @return A vector of boolean of the same size as **dadid** and **momid**
#' with `TRUE` if the individual has no parents (i.e is a founder) and
#' `FALSE` otherwise.
#'
#' @examples
#' is_founder(c("3", "3", NA, NA), c("4", "4", NA, NA))
#' @keywords internal
#' @export
is_founder <- function(momid, dadid, missid = NA_character_) {
    (dadid %in% missid) & (momid %in% missid)
}

#' Are individuals disconnected
#'
#' @description Check which individuals are disconnected.
#'
#' @details An individuals is considered disconnected if the kinship with
#' all the other individuals is `0`.
#'
#' @inheritParams Ped
#'
#' @return A vector of boolean of the same size as **id**
#' with `TRUE` if the individual is disconnected and
#' `FALSE` otherwise
#'
#' @include kinship.R
#' @keywords internal
#' @examples
#' is_disconnected(
#'      c("1", "2", "3", "4", "5"),
#'      c("3", "3", NA, NA, NA),
#'      c("4", "4", NA, NA, NA)
#' )
#' @export
is_disconnected <- function(id, dadid, momid) {
    # check to see if any subjects are disconnected in Pedigree by checking for
    # kinship = 0 for all subjects excluding self
    kin_mat <- kinship(id, dadid, momid)
    diag(kin_mat) <- 0
    apply(kin_mat == 0, 1, all)
}

#' @importFrom plyr revalue
NULL

#' Gender variable to ordered factor
#'
#' @inheritParams Ped
#'
#' @return an ordered factor vector containing the transformed variable
#' "male" < "female" < "unknown" < "terminated"
#' @examples
#' sex_to_factor(c(1, 2, 3, 4, "f", "m", "man", "female"))
#' @export
sex_to_factor <- function(sex) {
    if (is.factor(sex) || is.numeric(sex)) {
        sex <- as.character(sex)
    }
    ## Normalized difference notations for sex
    sex_equiv <- c(
        f = "female", m = "male", woman = "female", man = "male",
        female = "female", male = "male", `2` = "female", `1` = "male",
        `3` = "unknown", `4` = "terminated"
    )
    sex <- as.character(revalue(as.factor(
        casefold(sex, upper = FALSE)
    ), sex_equiv, warn_missing = FALSE))
    sex_codes <- c("male", "female", "unknown", "terminated")
    sex[!sex %in% sex_codes] <- "unknown"

    sex <- factor(sex, sex_codes, ordered = TRUE)
    sex
}

#' @importFrom stringr str_remove_all
NULL

#' Relationship code variable to ordered factor
#'
#' @inheritParams Rel
#'
#' @return an ordered factor vector containing the transformed variable
#' "MZ twin" < "DZ twin" < "UZ twin" < "Spouse"
#' @examples
#' rel_code_to_factor(c(1, 2, 3, 4, "MZ twin", "DZ twin", "UZ twin", "Spouse"))
#' @export
rel_code_to_factor <- function(code) {
    if (is.factor(code) || is.numeric(code)) {
        code <- as.character(code)
    }
    ## Normalized difference notations for code
    code_equiv <- c(
        mztwin = "MZ twin", dztwin = "DZ twin", uztwin = "UZ twin",
        spouse = "Spouse",
        "1" = "MZ twin", "2" = "DZ twin", "3" = "UZ twin", "4" = "Spouse"
    )
    codes <- c("MZ twin", "DZ twin", "UZ twin", "Spouse")
    code <- as.character(revalue(as.factor(
        str_remove_all(
            casefold(as.character(code), upper = FALSE),
            " "
        )
    ), code_equiv, warn_missing = FALSE))
    code <- factor(code, codes, ordered = TRUE)
    code
}

#' Vector variable to binary vector
#'
#' @description Transform a vector to a binary vector.
#' All values that are not `0`, `1`, `TRUE`, `FALSE`, or `NA`
#' are transformed to `NA`.
#'
#' @param vect A character, factor, logical or numeric vector corresponding to
#' a binary variable (i.e. `0` or `1`).
#' The following values are recognized:
#' - character() or factor() : "TRUE", "FALSE", "0", "1", "NA" will be
#' respectively transformed to `1`, `0`, `0`, `1`, `NA`.
#' Spaces and case are ignored.
#' All other values will be transformed to NA.
#' - numeric() : `0` and `1` are kept, all other values are transformed to NA.
#' - logical() : `TRUE` and `FALSE` are tansformed to `1` and `0`.
#' @param logical Boolean defining if the output should be a logical vector
#' instead of a numeric vector (i.e. `0` and `1` becomes `FALSE` and `TRUE).
#' @return numeric binary vector of the same size as **vect**
#' with `0` and `1`
#' @examples
#' vect_to_binary(
#'    c(0, 1, 2, 3.6, "TRUE", "FALSE", "0", "1", "NA", "B", TRUE, FALSE, NA)
#' )
#' @export
vect_to_binary <- function(vect, logical = FALSE) {
    if (is.factor(vect) || is.numeric(vect) || is.logical(vect)) {
        vect <- as.character(vect)
    }
    ## Normalized difference notations for code
    code_equiv <- c(
        "0" = 0, "1" = 1, "true" = 1, "false" = 0,
        "na" = NA
    )
    vect <- as.numeric(revalue(str_remove_all(
        casefold(vect, upper = FALSE),
        " "
    ), code_equiv, warn_missing = FALSE
    ))
    vect[!vect %in% c(0, 1)] <- NA
    if (logical) {
        as.logical(vect)
    } else {
        vect
    }
}

#' Anchor variable to ordered factor
#'
#' @param anchor A character, factor or numeric vector corresponding to
#' the anchor of the individuals. The following values are recognized:
#' - character() or factor() : "0", "1", "2", "left", "right", "either"
#' - numeric() : 1 = "left", 2 = "right", 0 = "either"
#'
#' @return An ordered factor vector containing the transformed variable
#' "either" < "left" < "right"
#' @examples
#' anchor_to_factor(c(1, 2, 0, "left", "right", "either"))
#' @export
anchor_to_factor <- function(anchor) {
    if (is.factor(anchor) || is.numeric(anchor)) {
        anchor <- as.character(anchor)
    }
    ## Normalized difference notations for anchor
    anchor_equiv <- c(
        "0" = "either", "1" = "left", "2" = "right",
        "left" = "left", "right" = "right", "either" = "either"
    )
    anchor <- as.character(revalue(as.factor(
        casefold(anchor, upper = FALSE)
    ), anchor_equiv, warn_missing = FALSE))
    anchor_codes <- c("left", "right", "either")
    if (any(!anchor %in% anchor_codes)) {
        stop(paste(
            "The following values are not recognized :",
            paste0(unique(anchor[!anchor %in% anchor_codes]), collapse = ", "),
            ".\n"
        ))
    }

    factor(anchor, anchor_codes, ordered = TRUE)
}
