#' @importFrom dplyr select one_of %>%
NULL

#' Check for columns name usage
#'
#' @description Check for presence / absence of columns names
#' depending on their need
#'
#' @details 3 types of columns are here checked:
#' - 1 `cols_needed` those columns need to be present if any is missing
#' an error will be prompted and the script will stop
#' - 2 `cols_used` those columns will be used in the script and will be
#' overwritten to NA.
#' - 3 `cols_to_use` those columns are optional and will be recognise
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
#' tryCatch(check_columns(df, c('ColN1', 'ColN2'), c('ColU1', 'ColU2'),
#'  c('ColTU1', 'ColTU2')), error = function(e) print(e))
#'
#' @export
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

#' Check is numeric
#'
#' @description Check if a variable given is numeric or NA
#'
#' @details Check if the values in `var` are numeric or if they are
#' NA in the case that `na_as_num` is set to TRUE.
#'
#' @param var Vector of value to test
#' @param na_as_num Boolean defining if the `NA` string should be
#' considered as numerical values
#'
#' @return A vector of boolean of the same size as `var`
#'
#' @examples
#' var <- c(45, 'NA', 'Test', '46.2', -2, '-46', '2NA', NA)
#' check_num_na(var)
#' check_num_na(var, na_as_num = FALSE)
#'
#' @export
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


#' Check wich individuals are parents
#'
#' @description Check which individuals are parents.
#'
#' @param id A vector of each subjects identifiers
#' @param dadid A vector containing for each subject, the identifiers of the
#' biologicals fathers.
#' @param momid  vector containing for each subject, the identifiers of the
#' biologicals mothers.
#' @param missid The missing identifier value. Founders are the individuals with
#' no father and no mother in the pedigree (i.e. `dadid` and `momid` equal to
#' the value of this variable).  The default for `missid` is `"0"`.
#'
#' @return A vector of boolean of the same size as `id`
#' with TRUE if the individual is a parent and FALSE otherwise
#'
#' @export
is_parent <- function(id, dadid, momid, missid = "0") {
    # determine subjects who are parents assume input of dadid/momid indices,
    # not ids

    if (length(id) != length(dadid) | length(id) != length(momid)) {
        stop("The length of the vectors are not the same")
    }

    is_father <- !is.na(match(id, unique(dadid[dadid != missid])))
    is_mother <- !is.na(match(id, unique(momid[momid != missid])))
    is_father | is_mother
}

#' Check wich individuals are founders
#'
#' @description Check which individuals are founders.
#'
#' @inheritParams is_parent
#'
#' @return A vector of boolean of the same size as `dadid` and `momid`
#' with TRUE if the individual has no parents (i.e is a founder) and FALSE
#' otherwise.
#'
#' @export
is_founder <- function(momid, dadid, missid = "0") {
    (dadid == missid) & (momid == missid)
}

#' Check wich individuals are disconnected
#'
#' @description Check which individuals are disconnected.
#'
#' @details An individuals is considered disconnected if the kinship with
#' all the other individuals is 0.
#'
#' @inheritParams is_parent
#'
#' @return A vector of boolean of the same size as `id`
#' with TRUE if the individual is disconnected and FALSE otherwise
#'
#' @include kinship.R
#' @export
is_disconnected <- function(id, dadid, momid) {
    # check to see if any subjects are disconnected in pedigree by checking for
    # kinship = 0 for all subjects excluding self
    kin_mat <- kinship(id, dadid, momid)
    diag(kin_mat) <- 0
    apply(kin_mat == 0, 1, all)
}

#' @importFrom plyr revalue
NULL

#' Transform a gender variable to an ordered factor
#'
#' @param sex A character, factor or numeric vector corresponding to
#' the gender of the individuals. The following values are recognized:
#' - character() or factor() : "f", "m", "woman", "man", "male", "female",
#' "unknown", "terminated"
#' - numeric() : 1 = "male", 2 = "female", 3 = "unknown", 4 = "terminated"
#'
#' @return an ordered factor vector containing the transformed variable
#' "male" > "female" > "unknown" > "terminated"
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
    sex <- suppressMessages(as.character(revalue(as.factor(
        casefold(sex, upper = FALSE)
    ), sex_equiv)))
    sex_codes <- c("male", "female", "unknown", "terminated")
    sex[!sex %in% sex_codes] <- "unknown"

    sex <- factor(sex, sex_codes, ordered = TRUE)
    sex
}

#' @importFrom stringr str_remove_all
NULL

#' Transform a relationship code variable to an ordered factor
#'
#' @param rel_code A character, factor or numeric vector corresponding to
#' the realtion code of the individuals:
#' - MZ twin = Monozygotic twin
#' - DZ twin = Dizygotic twin
#' - UZ twin = twin of unknown zygosity
#' - Spouse = Spouse
#' The following values are recognized:
#' - character() or factor() : "MZ twin", "DZ twin", "UZ twin", "Spouse" with
#' of without space between the words. The case is not important.
#' - numeric() : 1 = "MZ twin", 2 = "DZ twin", 3 = "UZ twin", 4 = "Spouse"
#'
#' @return an ordered factor vector containing the transformed variable
#' "MZ twin" < "DZ twin" < "UZ twin" < "Spouse"
#' @export
rel_code_to_factor <- function(rel_code) {
    if (is.factor(rel_code) || is.numeric(rel_code)) {
        rel_code <- as.character(rel_code)
    }
    ## Normalized difference notations for code
    code_equiv <- c(
        mztwin = "MZ twin", dztwin = "DZ twin", uztwin = "UZ twin",
        spouse = "Spouse",
        `1` = "MZ twin", `2` = "DZ twin", `3` = "UZ twin", `4` = "Spouse"
    )
    codes <- c("MZ twin", "DZ twin", "UZ twin", "Spouse")
    rel_code <- suppressMessages(as.character(revalue(as.factor(
        str_remove_all(
            casefold(as.character(rel_code), upper = FALSE),
            " "
        )
    ), code_equiv)))
    rel_code <- factor(rel_code, codes, ordered = TRUE)
    rel_code
}
TRUE
