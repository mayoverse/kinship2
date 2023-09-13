#' @importFrom plyr revalue
NULL

#' Process the colors based on affection
#'
#' @details Perform transformation uppon a column given as the one
#' containing affection status to compute the filling color.
#'
#' @param values The vector containing the values to process as affection.
#' @param affected The vector containing the affection status TRUE/FALSE.
#' @param labels The vector containing the labels to use for the affection.
#' @param keep_full_scale Boolean defining if the affection values need to
#' be set as a scale. If `values` is numeric the filling scale will be
#' calculated based on the values and the number of breaks given.
#' If `values` isn't numeric then each levels will get it's own color
#' @param breaks Number of breaks to use when using full scale with numeric
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
#' @examples
#' aff <- generate_aff_inds(seq_len(5), threshold = 3, sup_thres_aff = TRUE)
#' generate_fill(seq_len(5), aff$affected, aff$labels)
#' generate_fill(seq_len(5), aff$affected, aff$labels, keep_full_scale = TRUE)
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
    if (!keep_full_scale) {
        # If the scale is binary just keep the first color of unaff and the
        # last of aff
        fill_to_use <- c(colors_unaff[1], colors_aff[-1], "grey")
        names(fill_to_use) <- c("FALSE", "TRUE", NA)
        fill <- suppressMessages(revalue(affected, fill_to_use))
        mods <- suppressMessages(revalue(affected, c("FALSE" = 0, "TRUE" = 1)))
    } else {
        fct_scale_unaff <- grDevices::colorRampPalette(colors_unaff)
        fct_scale_aff <- grDevices::colorRampPalette(colors_aff)

        if (!is.numeric(values)) {
            levs_aff <- as.factor(values[affected == TRUE & !is.na(affected)])
            levs_unaff <- as.factor(
                values[affected == FALSE & !is.na(affected)]
            )
            fill_scale_aff <- fct_scale_aff(length(levels(levs_aff)))
            fill_scale_unaff <- fct_scale_unaff(length(levels(levs_unaff)))
            fill_scale <- c(fill_scale_unaff, fill_scale_aff)
            names(fill_scale) <- c(levels(levs_unaff), levels(levs_aff))
        } else {
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

        # Set fill depending on the corresponding color for aff and unaff
        fill[affected == TRUE & !is.na(affected)] <-
            suppressMessages(as.character(revalue(levs_aff, fill_scale)))

        fill[affected == FALSE & !is.na(affected)] <-
            suppressMessages(as.character(revalue(levs_unaff, fill_scale)))

        # Set modalities as factor levels
        mods[affected == TRUE & !is.na(affected)] <- as.character(levs_aff)
        mods[affected == FALSE & !is.na(affected)] <- as.character(levs_unaff)
        mods_to_use <- seq_along(fill_scale)
        names(mods_to_use) <- names(fill_scale)
        mods <- suppressMessages(revalue(mods, mods_to_use))
        rev_fill_scale <- names(fill_scale)
        names(rev_fill_scale) <- unlist(fill_scale)
        labels <- paste(labels, ":",
            suppressMessages(revalue(fill, rev_fill_scale))
        )
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
#' @examples
#' generate_border(c(1, 0, 1, 0, NA, 1, 0, 1, 0, NA))
#'
#' @export
generate_border <- function(avail, colors_avail = c("green", "black")) {
    if (length(avail) > 0) {
        if (! is.numeric(avail) && ! all(is.na(avail))) {
            stop("Available variable need to be numeric")
        }

        if (!all(levels(as.factor(avail)) %in% c("0", "1", "NA"))) {
            stop("Available variable need to have only 0, 1 or NA")
        }
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
#' @param obj A dataframe containing the columns to process or a pedigree
#' object.
#' @param col_aff The name of the column containing the affection status.
#' @param col_avail The name of the column containing the availability status.
#' @inheritParams generate_fill
#' @inheritParams generate_border
#' @inheritParams generate_aff_inds
#'
#' @return
#' ## When used with a dataframe :
#' A list of two elements
#' - The processed dataframe with the `affected` and `avail` columns
#' processed accordingly
#' - A dataframe containing the description of each modality of the scale
#'
#' ## When used with a pedigree object :
#' The pedigree object with the `affected` and `avail` columns
#' processed accordingly
#' The pedigree scales slots are updated
#'
#' @examples
#' data("sampleped")
#' ped <- pedigree(sampleped)
#' generate_colors(ped, "affected", add_to_scale=FALSE)$scales
#' @export
setGeneric("generate_colors", signature = "obj",
    function(obj, ...) standardGeneric("generate_colors")
)

#' @export
#' @aliases generate_colors,character
#' @rdname generate_colors
setMethod("generate_colors", "character",
    function(
        obj, avail,
        mods_aff = NULL, threshold = 0.5, sup_thres_aff = TRUE,
        keep_full_scale = FALSE, breaks = 3,
        colors_aff = c("yellow2", "red"),
        colors_unaff = c("white", "steelblue4"),
        colors_avail = c("green", "black")
    ) {
        affected_val <- obj
        affected <- generate_aff_inds(affected_val,
            mods_aff, threshold, sup_thres_aff
        )
        border <- generate_border(avail, colors_avail)
        lst_sc <- generate_fill(
            affected_val, affected$affected, affected$labels,
            keep_full_scale, breaks, colors_aff, colors_unaff
        )

        lst_sc$border_scale <- border
        lst_sc
    }
)

#' @export
#' @aliases generate_colors,data.frame
#' @rdname generate_colors
setMethod("generate_colors", "data.frame",
    function(
        obj, col_aff = "affected", col_avail = "avail",
        mods_aff = NULL, threshold = 0.5, sup_thres_aff = TRUE,
        keep_full_scale = FALSE, breaks = 3,
        colors_aff = c("yellow2", "red"),
        colors_unaff = c("white", "steelblue4"),
        colors_avail = c("green", "black")
    ) {
        new_col <- paste0(col_aff, "_aff")
        df <- check_columns(obj, c(col_aff, col_avail),
            "", new_col, others_cols = TRUE
        )

        lst_sc <- generate_colors(df[[col_aff]], df[[col_avail]],
            mods_aff, threshold, sup_thres_aff,
            keep_full_scale, breaks,
            colors_aff, colors_unaff, colors_avail
        )

        df[new_col] <- lst_sc$mods
        if (nrow(lst_sc$fill_scale) > 0) {
            lst_sc$fill_scale$column_mods <- new_col
            lst_sc$fill_scale$column_values <- col_aff
        }

        scales <- list(
            fill = lst_sc$fill_scale,
            border = lst_sc$border_scale
        )
        list(df = df, scales = scales)
    }
)

#' @importFrom plyr rbind.fill
#' @include pedigreeClass.R
#' @docType methods
#' @aliases generate_colors,Pedigree
#' @param add_to_scale Boolean defining if the scales need to be added to the
#' existing scales or if they need to replace the existing scales.
#' @param reset Boolean defining if the scale of the specified column need to
#' be reset if already present.
#' @param ... Other parameters to pass to the `generate_colors` function
#' @rdname generate_colors
#' @export
setMethod("generate_colors", "Pedigree",
    function(obj, col_aff = "affected", add_to_scale = TRUE,
        reset = TRUE, ...
    ) {
        if (nrow(obj$ped) == 0) {
            return(obj)
        }
        list_aff <- generate_colors(obj$ped, col_aff, ...)
        obj$ped <- list_aff$df

        if (add_to_scale) {
            if (col_aff %in% obj$scales$fill$column_values &
                    !reset) {
                stop("The column ", col_aff, " is already in the scales")
            } else if (col_aff %in% obj$scales$fill$column_values & reset) {
                obj$scales$fill <- obj$scales$fill[
                    obj$scales$fill$column_values != col_aff,
                ]
            }
            new_order <- ifelse(nrow(obj$scales$fill) > 0,
                max(obj$scales$fill$order) + 1, 1
            )
            list_aff$scales$fill$order <- new_order
            list_aff$scales$fill <- rbind.fill(obj$scales$fill,
                list_aff$scales$fill
            )
        } else {
            list_aff$scales$fill$order <- 1
            list_aff$scales$fill <- list_aff$scales$fill
        }

        obj$scales <- list_aff$scales
        validObject(obj)
        obj
    }
)
