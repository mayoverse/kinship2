#' @importFrom plyr revalue
NULL

#' Process the filling colors based on affection
#'
#' @description Perform transformation uppon a column given as the one
#' containing affection status to compute the filling color.
#'
#' @details The colors will be set using the
#' [grDevices::colorRampPalette()] function
#' with the colors given as parameters.
#'
#' The colors will be set as follow:
#'
#' - If **keep_full_scale** is `FALSE`:
#' Then the affected individuals will get the first color of the
#' **colors_aff** vector and the unaffected individuals will get the
#' first color of the **colors_unaff** vector.
#' - If **keep_full_scale** is `TRUE`:
#'   - If **values** isn't numeric:
#'   Each levels of the affected **values** vector will get it's own color from
#'   the **colors_aff** vector using the [grDevices::colorRampPalette()] and
#'   the same will be done for the unaffected individuals using the
#'   **colors_unaff**.
#'   - If **values** is numeric:
#'   The mean of the affected individuals will be compared to the mean of the
#'   unaffected individuals and the colors will be set up such as the color
#'   gradient follow the direction of the affection.
#'
#' @param values The vector containing the values to process as affection.
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
#' @inheritParams Ped
#'
#' @return A list of three elements
#' - `mods` : The processed values column as a numeric factor
#' - `affected` : A logical vector indicating if the individual is affected
#' - `sc_fill` : A dataframe containing the description of each modality of the
#' scale
#'
#' @examples
#' aff <- generate_aff_inds(seq_len(5), threshold = 3, sup_thres_aff = TRUE)
#' generate_fill(seq_len(5), aff$affected, aff$labels)
#' generate_fill(seq_len(5), aff$affected, aff$labels, keep_full_scale = TRUE)
#'
#' @keywords generate_scales
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
        fill <- revalue(
            as.character(affected), fill_to_use, warn_missing = FALSE
        )
        mods <- revalue(
            as.character(affected), c("FALSE" = 0, "TRUE" = 1),
            warn_missing = FALSE
        )
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
            as.character(revalue(levs_aff, fill_scale, warn_missing = FALSE))

        fill[affected == FALSE & !is.na(affected)] <-
            as.character(revalue(levs_unaff, fill_scale, warn_missing = FALSE))

        # Set modalities as factor levels
        mods[affected == TRUE & !is.na(affected)] <- as.character(levs_aff)
        mods[affected == FALSE & !is.na(affected)] <- as.character(levs_unaff)
        mods_to_use <- seq_along(fill_scale)
        names(mods_to_use) <- names(fill_scale)
        mods <- revalue(mods, mods_to_use, warn_missing = FALSE)
        rev_fill_scale <- names(fill_scale)
        names(rev_fill_scale) <- unlist(fill_scale)
        labels <- paste(labels, ":",
            revalue(fill, rev_fill_scale, warn_missing = FALSE)
        )
    }
    # Set to grey color individual with no informations
    fill[is.na(fill)] <- "grey"
    mods <- as.numeric(mods)

    sc_fill <- unique(as.data.frame(
        list(
            mods = mods, labels = labels, affected = affected, fill = fill,
            density = rep(NA_integer_, n), angle = rep(NA_integer_, n)
        )
    ))
    list(mods = mods, affected = affected, sc_fill = sc_fill)
}

#' Process the border colors based on availability
#'
#' @description Perform transformation uppon a vector given as the one
#' containing the availability status to compute the border color.
#' The vector given will be transformed using the [vect_to_binary()]
#' function.
#'
#' @param values The vector containing the values to process as available.
#' @param colors_avail Set of 2 colors to use for the box's border of an
#' individual. The first color will be used for available individual
#' (`avail == 1`) and the second for the unavailable individual
#' (`avail == 0`).
#'
#' @return A list of three elements
#' - `mods` : The processed values column as a numeric factor
#' - `avail` : A logical vector indicating if the individual is available
#' - `sc_bord` : A dataframe containing the description of each modality of the
#' scale
#'
#' @examples
#' generate_border(c(1, 0, 1, 0, NA, 1, 0, 1, 0, NA))
#'
#' @keywords generate_scales
#' @export
generate_border <- function(values, colors_avail = c("green", "black")) {
    # Set border colors
    if (length(colors_avail) != 2) {
        stop("Variable `colors_avail` need to be a vector of 2 colors")
    }

    mods <- vect_to_binary(values)
    avail <- vect_to_binary(values, logical = TRUE)

    sc_bord <- data.frame(
        column = "avail",
        mods = c(NA, 1, 0),
        border = c("grey", colors_avail[1], colors_avail[2]),
        labels = c("NA", "Available", "Non Available")
    )

    list(mods = mods, avail = avail, sc_bord = sc_bord)
}

#' Process the filling and border colors based on affection and availability
#'
#' @description Perform transformation uppon a dataframe given to compute
#' the colors for the filling and the border of the individuals based
#' on the affection and availability status.
#'
#' @details The colors will be set using the [generate_fill()] and the
#' [generate_border()] functions respectively for the filling and the border.
#'
#' @param obj A Pedigree object or a vector containing the affection status for
#' each individuals. The affection status can be numeric or a character.
#' @inheritParams generate_fill
#' @inheritParams generate_border
#' @inheritParams generate_aff_inds
#' @inheritParams Ped
#'
#' @return
#' ## When used with a vector
#'
#' A list of two elements
#' - The list containing the filling colors processed and their description
#' - The list containing the border colors processed and their description
#'
#' ## When used with a Pedigree object
#'
#' The Pedigree object with the `affected` and `avail` columns
#' processed accordingly as well as the `scales` slot updated.
#'
#' @keywords generate_scales
#' @export
#' @usage NULL
setGeneric("generate_colors", signature = "obj",
    function(obj, ...) standardGeneric("generate_colors")
)

#' @rdname generate_colors
#' @examples
#'
#' generate_colors(
#'     c("A", "B", "A", "B", NA, "A", "B", "A", "B", NA),
#'     c(1, 0, 1, 0, NA, 1, 0, 1, 0, NA),
#'     mods_aff = "A",
#' )
#' @export
setMethod("generate_colors", "character",
    function(
        obj, avail,
        mods_aff = NULL,
        keep_full_scale = FALSE,
        colors_aff = c("yellow2", "red"),
        colors_unaff = c("white", "steelblue4"),
        colors_avail = c("green", "black")
    ) {
        affected_val <- obj
        affected <- generate_aff_inds(affected_val,
            mods_aff = mods_aff
        )
        lst_bord <- generate_border(avail, colors_avail)
        lst_aff <- generate_fill(
            affected_val, affected$affected, affected$labels,
            keep_full_scale, NULL, colors_aff, colors_unaff
        )

        list(
            fill = lst_aff,
            bord = lst_bord
        )
    }
)

#' @rdname generate_colors
#' @examples
#'
#' generate_colors(
#'     c(10, 0, 5, 7, NA, 6, 2, 1, 3, NA),
#'     c(1, 0, 1, 0, NA, 1, 0, 1, 0, NA),
#'     threshold = 3, keep_full_scale = TRUE
#' )
#' @export
setMethod("generate_colors", "numeric",
    function(
        obj, avail, threshold = 0.5, sup_thres_aff = TRUE,
        keep_full_scale = FALSE, breaks = 3,
        colors_aff = c("yellow2", "red"),
        colors_unaff = c("white", "steelblue4"),
        colors_avail = c("green", "black")
    ) {
        affected_val <- obj
        affected <- generate_aff_inds(affected_val,
            mods_aff = NULL, threshold, sup_thres_aff
        )

        lst_bord <- generate_border(avail, colors_avail)
        lst_aff <- generate_fill(
            affected_val, affected$affected, affected$labels,
            keep_full_scale, breaks, colors_aff, colors_unaff
        )

        list(
            fill = lst_aff,
            bord = lst_bord
        )
    }
)

#' @importFrom plyr rbind.fill
#' @param add_to_scale Boolean defining if the scales need to be added to the
#' existing scales or if they need to replace the existing scales.
#' @param reset If `TRUE` the scale of the specified column will be reset if
#' already present.
#' @param col_avail A character vector with the name of the column to be used
#' for the availability status.
#' @param col_aff A character vector with the name of the column to be used
#' for the affection status.
#' @examples
#' data("sampleped")
#' ped <- Pedigree(sampleped)
#' ped <- generate_colors(ped, "affected", add_to_scale=FALSE)
#' scales(ped)
#' @rdname generate_colors
#' @include AllClass.R
#' @export
setMethod("generate_colors", "Pedigree",
    function(obj,
        col_aff = "affected", add_to_scale = TRUE,
        col_avail = "avail",
        mods_aff = NULL, threshold = 0.5, sup_thres_aff = TRUE,
        keep_full_scale = FALSE, breaks = 3,
        colors_aff = c("yellow2", "red"),
        colors_unaff = c("white", "steelblue4"),
        colors_avail = c("green", "black"),
        reset = TRUE
    ) {
        if (length(obj) == 0) {
            return(obj)
        }

        if (length(col_aff) > 1) {
            for (col in col_aff) {
                obj <- generate_colors(obj, col, add_to_scale,
                    col_avail, mods_aff, threshold, sup_thres_aff,
                    keep_full_scale, breaks,
                    colors_aff, colors_unaff, colors_avail,
                    reset
                )
            }
            return(obj)
        }

        new_fill <- paste0(col_aff, "_mods")
        new_bord <- paste0(col_avail, "_mods")
        df <- check_columns(as.data.frame(ped(obj)), c(col_aff, col_avail),
            "", c(new_fill, new_bord), others_cols = TRUE
        )

        ## Generate affected individuals
        lst_inds <- generate_aff_inds(df[[col_aff]],
            mods_aff, threshold, sup_thres_aff
        )

        ## Create border and fill scales
        lst_bord <- generate_border(
            df[[col_avail]], colors_avail
        )
        lst_fill <- generate_fill(
            df[[col_aff]], lst_inds$affected, lst_inds$labels,
            keep_full_scale, breaks, colors_aff, colors_unaff
        )

        lst_sc <- list(
            fill = lst_fill$sc_fill,
            border = lst_bord$sc_bord
        )

        ## Add mods to Pedigree object and update slots
        mcols(obj)[new_fill] <- lst_fill$mods
        mcols(obj)[new_bord] <- lst_bord$mods

        affected(ped(obj)) <- lst_fill$affected
        avail(ped(obj)) <- lst_bord$avail

        ## Update scales with correct names
        if (nrow(lst_sc$fill) > 0) {
            lst_sc$fill$column_mods <- new_fill
            lst_sc$fill$column_values <- col_aff
        }

        if (nrow(lst_sc$bord) > 0) {
            lst_sc$border$column_mods <- new_bord
            lst_sc$border$column_values <- col_avail
        }

        ## Should the affected scales be added to existing one
        if (add_to_scale) {
            if (col_aff %in% fill(obj)$column_values) {
                if (!reset) {
                    stop("The column ", col_aff, " is already in the scales")
                } else {
                    new_order <- unique(fill(obj)[
                        fill(obj)$column_values == col_aff,
                        "order"
                    ])
                    fill(obj) <- fill(obj)[
                        fill(obj)$column_values != col_aff,
                    ]
                }
            } else {
                new_order <- ifelse(nrow(fill(obj)) > 0,
                    max(fill(obj)$order) + 1, 1
                )
            }
            lst_sc$fill$order <- new_order
            lst_sc$fill <- rbind.fill(fill(obj), lst_sc$fill)
        } else {
            lst_sc$fill$order <- 1
        }
        scales(obj) <- Scales(lst_sc$fill, lst_sc$border)
        validObject(obj)
        obj
    }
)
