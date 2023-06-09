#' Data frame to contingency table
#'
#' @description Summarise two variables in a contingency table
#'
#' @details Proccess two variable from a dataframe in a contingency
#' table depending on the variables selected and the thresholds in case the
#' variable is numerical. To do so the variables are transformed to factors.
#'
#' @param df Dataframe containing the family informations
#' @param var1 First column variable to select
#' @param var2 Second column variable to select
#' @param threshold1 Threshold to apply to `var1`
#' @param threshold2 Threshold to apply to `var2`
#'
#' @examples
#' var1 <- runif(10)
#' var2 <- runif(10)
#' df <- data.frame(var1, var2)
#' df_cont_table(df, "var1", 0.5, "var2", c(0.25, 0.5, 0.75))
#' df_cont_table(df, "var1", 0.5)
#'
#' @export get_family_infos
df_cont_table <- function(df,
    var1, threshold1 = NULL,
    var2 = NULL, threshold2 = NULL) {
  if (! var1 %in% colnames(df)) {
    stop(paste0(var1, "is not present in the dataframe", collapse = " "))
  }
  if (! var2 %in% colnames(df) && !is.null(var2)) {
    stop(paste0(var2, "is not present in the dataframe", collapse = " "))
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

#' Variable to factor
#'
#' @description Transform a variable into a factor
#'
#' @details  Transform a numeric or string variable into a factor.
#' In the case of a numerical variable the threshold is used to separate
#' the variable using the `cut` function.
#'
#' @param var The variable to be transformed
#' @param thresholds The thresholds to be used to separate the variable
#'
#' @return a factor vector containing the transformed variable
#'
#' @examples
#' var <- runif(10)
#' var_to_factor(var, threshold = 0.5)
#'
#' @export var_to_factor
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
      var,
      c(min(var, na.rm = TRUE), threshold, max(var, na.rm = TRUE)),
      label = labels,
      include.lowest = TRUE
    )
    var_fact <- addNA(var_fact, ifany = TRUE)
  }
  var_fact
}

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
#'
#' @param df The dataframe to use
#' @param cols_needed A vector of columns needed
#' @param cols_used A vector of columns that are used by the script and
#' that will be overwritten.
#' @param cols_to_use A vector of optional columns that are authorized.
#' @param others_cols Boolean defining if non defined columns should be allowed.
#'
#' @return Dataframe with only the column allowed and all the column
#' to be used by the script initialised to NA.
#'
#' @examples
#' data.frame
#' df <- data.frame(ColN1 = c(1, 2), ColN2 = 4,
#'          ColU1 = "B", ColU2 = "1",
#'          ColTU1 = "A", ColTU2 = 3,
#'          ColNR1 = 4, ColNR2 = 5)
#'check_columns(df, c("ColN1", "ColN2"), c("ColU1", "ColU2"),
#'   c("ColTU1", "ColTU2"))
#'
#' @export check_columns
check_columns <- function(df,
    cols_needed, cols_used, cols_to_use,
    others_cols = FALSE) {
  cols_p <- colnames(df)
  cols_needed_missing <- cols_needed[is.na(match(cols_needed, cols_p))]
  if (length(cols_needed_missing) > 0) {
    stop(paste("Columns :", cols_needed_missing,
      "are missing. Could not continu without.\n"))
  }
  col_use_by_script <- cols_used[cols_used %in% cols_p]
  if (length(col_use_by_script) > 0) {
    warning(paste("Columns :", col_use_by_script,
      "are used by the script and will be overwriten.\n"))
    df[col_use_by_script] <- NA
  }
  cols_optional <- cols_to_use[cols_to_use %in% cols_p]
  if (length(cols_optional) > 0) {
    message(paste("Columns :", cols_optional,
      "where recognize and therefore will be used.\n"))
  }

  if (others_cols) {
    all_cols_checked <- cols_p
  } else {
    all_cols_checked <- c(cols_needed, col_use_by_script, cols_optional)
    cols_not_recognize <- cols_p[!cols_p %in% all_cols_checked]
    if (length(cols_not_recognize) > 0) {
      message(paste("Columns :", cols_not_recognize,
        "not recognize and therefore will be disregarded.\n"))
    }
  }

  df[all_cols_checked]
}

usethis::use_package(stringr)
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
#' var <- c(45, "NA", "Test", "46.2", -2, "-46", "2NA", NA)
#' check_num_na(var)
#' check_num_na(var, na_as_num = FALSE)
#'
#' @export check_num_na
check_num_na <- function(var, na_as_num = TRUE) {
  # Should the NA value considered as numeric values
  is_num <- stringr::str_detect(var, "^\\-*[:digit:]+\\.*[:digit:]*$")
  is_na <- FALSE
  if (na_as_num) {
    is_na <- stringr::str_detect(as.character(var), "^NA$")
    is_na <- is_na | is.na(var)
  }
  is_num | is_na
}
