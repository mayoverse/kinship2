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


test_that("var_to_factor works", {
    var <- c(1, NA, 4, 1.2, -3)
    expect_error(var_to_factor(var))
    expect_snapshot(var_to_factor(var, threshold = 1))
    expect_snapshot(var_to_factor(var, threshold = c(0, 1)))
    expect_snapshot(var_to_factor(c("A", "B", "A", "C")))
})

test_that("df_cont_table", {
    set.seed(10)
    var1 <- runif(10)
    var2 <- runif(10)
    df <- data.frame(var1, var2)
    expect_snapshot(df_cont_table(df, "var1", 0.5, "var2", c(0.25, 0.5, 0.75)))
    expect_snapshot(df_cont_table(df, "var1", 0.5))
    expect_error(df_cont_table(df, "var3"))
    expect_error(df_cont_table(df, "var1", 0.5, "var3"))
})

#' @importFrom gridGraphics grid.echo
#' @importFrom grid grid.grab
NULL

#' Register the plot
#'
#' Use the grid.echo() and grid.grab() functions to register the plot.
#'
#' @export
grab_grob <- function() {
    grid.echo()
    grid.grab()
}