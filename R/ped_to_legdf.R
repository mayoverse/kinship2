#' Convert a Pedigree to a legend data frame of element to plot
#'
#' @description
#' Convert a Pedigree to a legend data frame for it to
#' be plotted with afterwards with [plot_fromdf()].
#'
#' @inheritParams align
#' @inheritParams set_plot_area
#' @inheritParams plot_fromdf
#' @param adjx default=0.  Controls the horizontal text adjustment of
#' the labels in the legend.
#' @param adjy default=0.  Controls the vertical text adjustment
#' of the labels in the legend.
#'
#' @return
#' A list containing the legend data frame and the user coordinates.
#'
#' @examples
#' data("sampleped")
#' ped <- Pedigree(sampleped)
#' leg_df <- ped_to_legdf(ped)
#' summary(leg_df$leg_df)
#' plot_fromdf(leg_df$leg_df)
#' @export
#' @docType methods
ped_to_legdf <- function(ped,
    boxh = 1, boxw = 1, cex = 1,
    adjx = 0, adjy = 0
) {
    par_usr <- list(boxh = boxh, boxw = boxw, cex = cex)
    plot_df <- data.frame(
        id = character(),
        x0 = numeric(), y0 = numeric(), x1 = numeric(), y1 = numeric(),
        type = character(), fill = character(), border = character(),
        angle = numeric(), density = numeric(), cex = numeric(),
        label = character(), tips = character(), adjx = numeric(),
        adjy = numeric()
    )
    sex_equiv <- c("Male", "Female", "Terminated", "Unknown")
    all_lab <- list(sex_equiv, scales(ped)$border$labels)
    all_aff <- lapply(unique(scales(ped)$fill$order), function(x) {
        scales(ped)$fill$labels[scales(ped)$fill$order == x]
    })

    all_lab <- c(all_lab, all_aff)

    max_lab <- lapply(lapply(
        all_lab, strwidth,
        units = "inches", cex = cex
    ), max)

    posx <- unlist(lapply(max_lab, function(x) {
        c(c(boxw, boxw, boxw / 5), x * 3)
    }))
    posx <- cumsum(posx) - boxw
    posx <- c(posx[seq_along(posx) %% 2 == 1], posx[length(posx)])

    n_max <- max(unlist(lapply(all_lab, function(x) {
        length(x)
    })))

    posy <- rep(boxh, n_max * 2)
    posy <- cumsum(posy) - boxh
    posy <- posy[seq_along(posy) %% 2 == 0]

    all_aff <- scales(ped)$fill
    n_aff <- length(unique(all_aff$order))

    lab_title <- c("Sex", "Border", unique(all_aff$column_values))
    titles <- data.frame(
        x0 = posx[seq_along(posx) %% 2 == 0] - boxw, y0 = 0,
        type = "text", label = lab_title,
        fill = "black", cex = cex * 1.5,
        id = "titles"
    )
    plot_df <- rbind.fill(plot_df, titles)

    # Sex
    poly1 <- polygons(1)
    all_sex <- unique(as.numeric(ped(ped)$sex))
    sex <- data.frame(
        x0 = posx[1], y0 = posy[all_sex],
        type = paste(names(poly1)[all_sex], 1, 1, sep = "_"),
        fill = "white",
        border = "black",
        id = "sex"
    )

    sex_label <- data.frame(
        x0 = posx[2] + adjx,
        y0 = posy[all_sex] + boxh / 2 + adjy,
        label = sex_equiv[all_sex], cex = cex,
        type = "text",
        fill = "black",
        id = "sex_label"
    )

    plot_df <- rbind.fill(plot_df, sex, sex_label)

    # Border
    bord_df <- scales(ped)$border
    border_mods <- unique(ped(ped)[, unique(bord_df[["column"]])])
    border <- data.frame(
        x0 = posx[3], y0 = posy[seq_along(border_mods)],
        type = rep("square_1_1", length(border_mods)),
        border = bord_df$border[match(border_mods, bord_df$mods)],
        fill = "white",
        id = "border"
    )
    lab <- bord_df$labels[match(border_mods, bord_df$mods)]
    lab[is.na(lab)] <- "NA"
    border_label <- data.frame(
        x0 = posx[4] + adjx,
        y0 = posy[seq_along(border_mods)] + boxh / 2  + adjy,
        label = lab, cex = cex,
        type = "text",
        fill = "black",
        id = "border_label"
    )

    plot_df <- rbind.fill(plot_df, border, border_label)

    ## Affected
    for (aff in seq_len(n_aff)) {
        aff_df <- all_aff[all_aff$order == aff, ]
        aff_mods <- unique(ped(ped)[, unique(aff_df[["column_mods"]])])
        aff_bkg <- data.frame(
            x0 = posx[3 + aff * 2], y0 = posy[seq_along(aff_mods)],
            type = rep(paste("square", 1, 1, sep = "_"),
                length(aff_mods)
            ),
            border = "black",
            fill = "white",
            id = paste("aff_bkg", aff, aff_mods, sep = "_")
        )

        affected <- data.frame(
            x0 = posx[3 + aff * 2], y0 = posy[seq_along(aff_mods)],
            type = rep(paste("square", n_aff, aff, sep = "_"),
                length(aff_mods)
            ),
            border = "black",
            fill = aff_df$fill[match(aff_mods, aff_df$mods)],
            id = paste("affected", aff, aff_mods, sep = "_")
        )

        lab <- aff_df$labels[match(aff_mods, aff_df$mods)]
        lab[is.na(lab)] <- "NA"
        affected_label <- data.frame(
            x0 = posx[4 + aff * 2] + adjx,
            y0 = posy[seq_along(aff_mods)] + boxh / 2 + adjy,
            label = lab, cex = cex,
            type = "text",
            fill = "black",
            id = paste("affected_label", aff, aff_mods, sep = "_")
        )
        plot_df <- rbind.fill(plot_df, aff_bkg, affected, affected_label)
    }

    ## Max limit
    max_lim <- data.frame(
        x0 = c(0, max(posx)), y0 = c(0, max(posy)),
        type = "text",
        border = "black",
        label = NA,
        id = "max_lim"
    )
    plot_df <- rbind.fill(plot_df, max_lim)

    plot_df[plot_df$type == "text", "adjx"] <- 0
    plot_df[plot_df$type == "text", "adjy"] <- 1
    par_usr$usr <- c(
        min(plot_df$x0), max(plot_df$x0),
        min(plot_df$y0), max(plot_df$y0)
    )
    list(leg_df = plot_df, par_usr = par_usr)
}
