usethis::use_package("plyr")

#' Process the colors based on affection
#'
#' @details Perform transformation uppon a column given as the one
#' containing affection status to compute the filling color.
#'
#' @param values The vector containing the values to process as affection.
#' @param affected The vector containing the affection status.
#' @param labels The vector containing the labels to use for the affection.
#' @param keep_full_scale Boolean defining if the affection values need to
#' be set as a scale. If `values` is numeric the filling scale will be
#' calculated based on the values and the number of breaks given.
#' If `values` isn't numeric then each levels will get it's own color
#' @param break Number of breaks to use when using full scale with numeric
#' values. The same number of breaks will be done for values from affected
#' individuals and unaffected individuals.
#' @param colors_aff Set of increasing colors to use for the filling of the
#' affected individuls.
#' @param colors_unaff Set of increasing colors to use for the filling of the
#' unaffected individuls.
#'
#' @return A list of two elements
#' - The processed values column as a numeric factor
#' - A dataframe containing the description of each modality of the scale
#'
#' @export
generate_fill <- function(
    values, affected, labels,
    keep_full_scale = FALSE, breaks = 3,
    colors_aff = c("yellow2", "red"),
    colors_unaff = c("white", "steelblue4")
) {

    n <- length(values)

    if (length(affected) != n) {
        stop("The length of `affected` need to be the same as `values`")
    }

    if (length(labels) != n) {
        stop("The length of `affected` need to be the same as `values`")
    }

    mods <- fill <- rep(NA, n)

    # Affection modality previously used
    scale <- unique(as.data.frame(
        list(mods_aff = labels, affected = affected, fill = fill)
    ))
    if (! all(scale$affected %in% c(NA, TRUE, FALSE))) {
        stop("Affected status should only contains TRUE, FALSE or NA")
    }

    # Set the filling color
    print("Bal: generate_colors, fill_scale")
    if (!keep_full_scale) {
        print("Bal: generate_colors: keep_full_scale = FALSE")
        # If the scale is binary just keep the first color of unaff and the
        # last of aff
        fill_to_use <- c(colors_unaff[1], colors_aff[-1], "grey")
        names(fill_to_use) <- c("FALSE", "TRUE", NA)
        fill <- plyr::revalue(affected, fill_to_use)
        mods <- plyr::revalue(affected, c("FALSE" = 0, "TRUE" = 1))
    } else {
        print("Bal: generate_colors: keep_full_scale = TRUE")
        fct_scale_unaff <- grDevices::colorRampPalette(colors_unaff)
        fct_scale_aff <- grDevices::colorRampPalette(colors_aff)

        if (!is.numeric(values)) {
            print("Bal: generate_colors: col_aff is not numeric")
            levs_aff <- as.factor(values[affected == TRUE & !is.na(affected)])
            levs_unaff <- as.factor(
                values[affected == FALSE & !is.na(affected)]
            )
            fill_scale_aff <- fct_scale_aff(length(levels(levs_aff)))
            fill_scale_unaff <- fct_scale_unaff(length(levels(levs_unaff)))
            fill_scale <- c(fill_scale_unaff, fill_scale_aff)
            names(fill_scale) <- c(levels(levs_unaff), levels(levs_aff))
        } else {
            print("Bal: generate_colors: col_aff is numeric")
            mean_aff <- mean(values[affected == TRUE], na.rm = TRUE)
            mean_unaff <- mean(values[affected == FALSE], na.rm = TRUE)
            levs_aff <- cut(values[affected == TRUE & !is.na(affected)],
                breaks = breaks, include.lowest = TRUE
            )
            levs_unaff <- cut(values[affected == FALSE & !is.na(affected)],
                breaks = breaks, include.lowest = TRUE
            )
            fill_scale_aff <- fct_scale_aff(breaks)
            fill_scale_unaff <- fct_scale_unaff(breaks)
            if (mean_aff > mean_unaff) {
                fill_scale <- c(fill_scale_unaff, fill_scale_aff)
                names(fill_scale) <- c(levels(levs_unaff), levels(levs_aff))
            } else {
                fill_scale <- c(fill_scale_aff, fill_scale_unaff)
                names(fill_scale) <- c(
                    rev(levels(levs_aff)),
                    rev(levels(levs_unaff))
                )
            }
        }

        if (any(duplicated(fill_scale))) {
            stop("The colors for the scale should be different")
        }

        print("Bal: generate_colors: fill_scale_aff")
        # Set fill depending on the corresponding color for aff and unaff
        fill[affected == TRUE & !is.na(affected)] <-
            as.character(plyr::revalue(levs_aff, fill_scale))

        fill[affected == FALSE & !is.na(affected)] <-
            as.character(plyr::revalue(levs_unaff, fill_scale))

        # Set modalities as factor levels
        mods[affected == TRUE & !is.na(affected)] <- as.character(levs_aff)
        mods[affected == FALSE & !is.na(affected)] <- as.character(levs_unaff)
        mods_to_use <- seq_along(fill_scale)
        names(mods_to_use) <- names(fill_scale)
        mods <- plyr::revalue(mods, mods_to_use)
    }
    # Set to grey color individual with no informations
    fill[is.na(fill)] <- "grey"
    mods <- as.numeric(mods)

    scale <- unique(as.data.frame(
        list(
            mods = mods, labels = labels, affected = affected, fill = fill,
            density = rep(NA, n), angle = rep(NA, n)
        )
    ))

    list(mods = mods, fill_scale = scale)
}

#' Process the colors based on affection and availability
#'
#' @details Perform transformation uppon a column given as the one
#' containing the availability status to compute the border color.
#'
#' @param avail The vector containing the availability status.
#' The values need to be numeric and can only be 0, 1 or NA.
#' @param colors_avail Set of 2 colors to use for the box's border of an
#' individual. The first color will be used for available individual (avail
#' == 1) and the second for the unavailable individual (avail == 0).
#'
#' @return A dataframe containing the scale to use for the availability
#' status.
#'
#' @export
generate_border <- function(avail, colors_avail = c("green", "black")) {
    if (! is.numeric(avail)) {
        stop("Available variable need to be numeric")
    }

    if (!all(levels(as.factor(avail)) %in% c("0", "1", "NA"))) {
        stop("Available variable need to have only 0, 1 or NA")
    }

    # Set border colors
    if (length(colors_avail) != 2) {
        stop("Variable `colors_avail` need to be a vector of 2 colors")
    }

    as.data.frame(list(
        column = "avail",
        mods = c(NA, 1, 0),
        border = c("grey", colors_avail[1], colors_avail[2]),
        labels = c("NA", "Available", "Non Available")
    ))
}

#' Process the colors based on affection and availability
#'
#' @details Perform transformation uppon a dataframe given to compute
#' the colors for the filling and the border of the individuals based
#' on the affection and availability status.
#'
#' @param df The dataframe containing the columns to process.
#' @param col_aff The name of the column containing the affection status.
#' @param mods_aff Vector of modality to consider as affected in the case
#' where the `col_aff` is a factor.
#' @param threshold Numeric value separating the affected and healthy subject
#' in the case where the `col_aff` is numeric.
#' @param sup_thres_aff Boolean defining if the affected individual are above
#' the threshold or not. If TRUE, the individuals will be considered affected
#' if the value of `col_aff` is stricly above the `threshold`. If FALSE, the
#' individuals will be considered affected if the value is stricly under the
#' `threshold`.
#' @param col_avail The name of the column containing the availability status.
#' @param keep_full_scale Boolean defining if the affection values need to
#' be set as a continuous scale or to a boolean.
#' @param breaks Number of breaks to use when using full scale with numeric
#' values. The same number of breaks will be done for values from affected
#' individuals and unaffected individuals.
#' @param colors_aff Set of increasing colors to use for the filling of the
#' affected individuls.
#' @param colors_unaff Set of increasing colors to use for the filling of the
#' unaffected individuls.
#' @param colors_avail Set of 2 colors to use for the box's border of an
#' individual. The first color will be used for available individual (avail
#' == 1) and the second for the unavailable individual (avail == 0).
#'
#' @return A list of two elements
#' - The processed dataframe with the `affected` and `avail` columns
#' processed accordingly
#' - A dataframe containing the description of each modality of the scale
#'
#' @export
setGeneric("generate_colors", function(obj, ...) {
    standardGeneric("generate_colors")
})

#' @export
setMethod("generate_colors", "data.frame",
    function(
        obj, col_aff,
        mods_aff = NULL, threshold = NULL, sup_thres_aff = NULL,
        keep_full_scale = FALSE, breaks = 3,
        colors_aff = c("yellow2", "red"),
        colors_unaff = c("white", "steelblue4"),
        col_avail = "avail", colors_avail = c("green", "black")
    ) {
        new_col <- paste0(col_aff, "_aff")
        df <- check_columns(obj, c(col_aff, col_avail),
            "", new_col, others_cols = TRUE
        )

        affected <- generate_aff_inds(df[[col_aff]],
            mods_aff, threshold, sup_thres_aff
        )
        border <- generate_border(df[[col_avail]], colors_avail)
        fill <- generate_fill(
            df[[col_aff]], affected$affected, affected$labels,
            keep_full_scale, breaks, colors_aff, colors_unaff
        )
        df[new_col] <- fill$mods
        fill$fill_scale$columns <- new_col
        scales <- list(
            fill = fill$fill_scale,
            border = border
        )

        list(df = df, scales = scales)
    }
)

#' @export
setMethod("generate_colors", "Pedigree",
    function(obj, ...) {
        list_aff <- generate_colors(obj$ped, ...)

        obj$ped <- list_aff$df
        obj$scales <- list_aff$scales
        validObject(obj)
        obj
    }
)
