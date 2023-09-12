#' Variable to factor
#'
#' @description Transform a variable into a factor
#'
#' @details  Transform a numeric or string variable into a factor.
#' In the case of a numerical variable the threshold is used to separate
#' the variable using the `cut` function.
#'
#' @param var The variable to be transformed
#' @param threshold The threshold to be used to separate the variable
#'
#' @return a factor vector containing the transformed variable
#'
#' @examples
#' var <- runif(10)
#' var_to_factor(var, threshold = 0.5)
#'
#' @export
var_to_factor <- function(var, threshold = NULL) {
    if (!is.numeric(var)) {
        var_fact <- addNA(droplevels(as.factor(var)), ifany = TRUE)
    } else {
        if (is.null(threshold)) {
            stop("No threshold given")
        }
        labels <- NULL
        if (length(threshold) == 1) {
            labels <- c(paste("Inf to", threshold), paste("Sup to", threshold))
        }
        var_fact <- cut(
            var, c(min(var, na.rm = TRUE),
                threshold, max(var, na.rm = TRUE)
            ), label = labels, include.lowest = TRUE
        )
        var_fact <- addNA(var_fact, ifany = TRUE)
    }
    var_fact
}

#' Data frame to contingency table
#'
#' @description Summarise two variables in a contingency table
#'
#' @details Proccess two variable from a dataframe in a contingency
#' table depending on the variables selected and the thresholds in case the
#' variable is numerical. To do so the variables are transformed to factors.
#'
#' @param df Dataframe containing the family informations
#' @param var1,var2 First and second column variable to select
#' @param threshold1,threshold2 Thresholds to apply to `var1` and `var2`
#'
#' @examples
#' var1 <- runif(10)
#' var2 <- runif(10)
#' df <- data.frame(var1, var2)
#' df_cont_table(df, 'var1', 0.5, 'var2', c(0.25, 0.5, 0.75))
#' df_cont_table(df, 'var1', 0.5)
#'
#' @export
df_cont_table <- function(
    df, var1, threshold1 = NULL, var2 = NULL, threshold2 = NULL
) {
    if (!var1 %in% colnames(df)) {
        stop(paste0(var1, " is not present in the dataframe", collapse = " "))
    }
    if (!var2 %in% colnames(df) && !is.null(var2)) {
        stop(paste0(var2, " is not present in the dataframe", collapse = " "))
    }
    var1_fact <- var_to_factor(df[[var1]], threshold = threshold1)
    if (is.null(var2)) {
        cont_table <- as.data.frame(table(var1_fact))
        colnames(cont_table) <- c(var1, "Freq")
    } else {
        var2_fact <- var_to_factor(df[[var2]], threshold = threshold2)
        cont_table <- as.data.frame(table(var1_fact, var2_fact))
        colnames(cont_table) <- c(var1, var2, "Freq")
    }
    cont_table
}

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
#'check_columns(df, c('ColN1', 'ColN2'), c('ColU1', 'ColU2'),
#'   c('ColTU1', 'ColTU2'))
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
#' @param id Vector of individual id
#' @param dadid Vector of father index
#' @param momid Vector of mother index
#' @param missid Missing value identifier
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
#' @param dadid Vector of father index
#' @param momid Vector of mother index
#' @param missid Missing value identifier
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
#' @param id Vector of individual id
#' @param dadid Vector of father id
#' @param momid Vector of mother id
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
TRUE
