#' Process the information for affection
#'
#' @details Perform transformation uppon a column given as the one
#' containing affection status to get an `affected` column usable for
#' the rest of the script
#'
#' @param df The dataframe to process
#' @param col_aff The name of the column containing the values to process as
#' affection
#' @param keep_full_scale Boolean defining if the affection values need to
#' be set as a scale. If `col_aff` is numeric the filling scale will be
#' calculated based on the value of `col_aff` and the number of breaks given.
#' If `col_aff` isn't numeric then each levels will get it's own color
#' @param break Number of breaks to use when using full scale with numeric
#' values. The same number of breaks will be done for values from affected
#' individuals and unaffected individuals.
#' @param colors_avail Set of 2 colors to use for the box's border of an
#' individual. The first color will be used for available individual (avail
#' == 1) and the second for the unavailable individual (avail == 0).
#' @param colors_aff Set of increasing colors to use for the filling of the
#' affected individuls.
#' @param colors_unaff Set of increasing colors to use for the filling of the
#' unaffected individuls.
#'
#' @return A list of two elements
#' - The dataframe with the columns `fill` and `border` updated
#' - List containing the corresponding scale for `col_aff` and availability
#'
#' @export generate_colors
generate_colors <- function(df, col_aff, keep_full_scale = FALSE, breaks = 3,
  colors_avail = c("green", "black"),
  colors_aff = c("yellow2", "red"),
  colors_unaff = c("white", "steelblue4")) {

  cols_needed <- c("affected", "mods_aff", "avail")
  cols_used <- c("border", "fill")

  df <- check_columns(df, cols_needed, cols_used, "", others_cols = TRUE)

  # Set border colors
  if (length(colors_avail) != 2) {
    stop("Variable `colors_avail` need to be a vector of 2 colors")
  }
  ## Available individual have the first color
  df$border <- colors_avail[1]
  ## Unavailable individual have the second color
  df$border[df$avail == 0] <- colors_avail[2]

  border_scale <- setNames(c(colors_avail[1], colors_avail[2]),
    c("Available", "Non Available"))

  # Set the filling color
  if (!keep_full_scale) {
    # If the scale is binary just keep the first color of unaff
    # and the last of aff
    fill_scale <- c("0" = colors_unaff[1], "1" = colors_aff[-1])
    df$fill <- plyr::revalue(as.character(df$affected),
      fill_scale)
    lev <- na.omit(unique(df[, c("affected", "mods_aff")]))
    mods <- data.frame(lev$mods_aff, lev$affected)
    if (any(dim(mods) != 2)) {
      stop("Error while computing the affected levels filling scale")
    }
    names(fill_scale) <- mods$lev.mods_aff[match(names(fill_scale),
        mods$lev.affected)]
  } else {
    fct_scale_unaff <- grDevices::colorRampPalette(colors_unaff)
    fct_scale_aff <- grDevices::colorRampPalette(colors_aff)

    lev <- na.omit(unique(df[, c("affected", "mods_aff")]))
    mods <- data.frame(lev$mods_aff, lev$affected)

    if (!is.numeric(df[[col_aff]])) {
      levs_aff <- as.factor(df[df$affected == 1 &
          !is.na(df$affected), col_aff])
      levs_unaff <- as.factor(df[df$affected == 0 &
          !is.na(df$affected), col_aff])
      fill_scale_aff <- fct_scale_aff(length(levels(levs_aff)))
      fill_scale_unaff <- fct_scale_unaff(length(levels(levs_unaff)))
      fill_scale_aff <- setNames(fill_scale_aff, levels(levs_aff))
      fill_scale_unaff <- setNames(fill_scale_unaff, levels(levs_unaff))
    } else {
      mean_aff <- mean(df[df$affected == 1, col_aff], na.rm = TRUE)
      mean_unaff <- mean(df[df$affected == 0, col_aff], na.rm = TRUE)
      levs_aff <- cut(df[df$affected == 1 & !is.na(df$affected), col_aff],
        breaks = breaks, include.lowest = TRUE)
      levs_unaff <- cut(df[df$affected == 0 & !is.na(df$affected), col_aff],
        breaks = breaks, include.lowest = TRUE)
      fill_scale_aff <- fct_scale_aff(breaks)
      fill_scale_unaff <- fct_scale_unaff(breaks)
      if (mean_aff > mean_unaff) {
        fill_scale_aff <- setNames(fill_scale_aff, levels(levs_aff))
        fill_scale_unaff <- setNames(fill_scale_unaff, levels(levs_unaff))
      } else {
        fill_scale_aff <- setNames(fill_scale_aff, rev(levels(levs_aff)))
        fill_scale_unaff <- setNames(fill_scale_unaff, rev(levels(levs_unaff)))
      }
    }
    # Set fill depending on the corresponding color for aff and unaff
    df$fill[df$affected == 1 & !is.na(df$affected)] <-
      as.character(plyr::revalue(levs_aff, fill_scale_aff))
    df$fill[df$affected == 0 & !is.na(df$affected)] <-
      as.character(plyr::revalue(levs_unaff, fill_scale_unaff))
    fill_scale <- c(fill_scale_aff, fill_scale_unaff)
  }
  # Set to grey color individual with no informations
  df$fill[is.na(df$fill)] <- "grey"

  fill_border_scale <- list(fill_scale, border_scale)
  names(fill_border_scale) <- c(col_aff, "Availability")

  list(df = df, scales = fill_border_scale)
}