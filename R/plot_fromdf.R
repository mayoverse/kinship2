#' @importFrom ggplot2 ggplot ggtitle theme element_blank element_rect
#' @importFrom stringr str_split_fixed str_split_i
NULL

#' @include plot_fct.R
#' @export
plot_from_df <- function(
    df, par_usr = NULL, title = NULL, ggplot_gen = FALSE, boxw = 1,
    boxh = 1
) {
    frame()
    if (!is.null(par_usr)) {
        par(usr = par_usr)
    }
    p <- ggplot() +
        theme(
            plot.margin = unit(c(0, 0, 0, 0), "cm"),
            panel.background = element_rect(fill = "transparent", color = NA),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.ticks = element_blank(),
            axis.text = element_blank(),
            axis.title = element_blank()
        ) +
        scale_y_reverse()

    ## Add title if exists
    if (!is.null(title)) {
        title(title)
        p <- p + ggtitle(title)
    }

    naff <- max(as.numeric(str_split_i(df$type, "_", 2)), na.rm = TRUE)

    ## Add boxes
    polylist <- polygons(naff)
    all_types <- unlist(lapply(names(polylist),
        paste, seq_len(naff), sep = "_"
    ))

    boxes <- df[df$type %in% all_types, ]
    boxes[c("poly", "naff")] <- str_split_fixed(boxes$type, "_", 2)
    boxes$angle[boxes$angle == "NA"] <- 45

    for (i in seq_len(dim(boxes)[1])){
        poly <- polylist[[boxes$poly[i]]][[as.numeric(boxes$naff[i])]]
        p <- draw_polygon(
            boxes$x0[i] + poly$x * boxw,
            boxes$y0[i] + poly$y * boxh,
            p, ggplot_gen,
            boxes$fill[i], boxes$border[i], boxes$density[i], boxes$angle[i]
        )
    }
    txt <- df[df$type == "text" & !is.na(df$label), ]
    p <- draw_text(
        txt$x0, txt$y0, txt$label,
        p, ggplot_gen, txt$cex, txt$fill
    )

    seg <- df[df$type == "segments", ]
    p <- draw_segment(
        seg$x0, seg$y0, seg$x1, seg$y1,
        p, ggplot_gen, seg$fill, seg$cex
    )

    arcs <- df[df$type == "arc", ]
    for (it in nrow(arcs)){
        arc <- arcs[it, ]
        p <- draw_arc(arc$x0, arc$y0, arc$x1, arc$y1,
            p, ggplot_gen, cex = arc$cex, col = arc$fill)
    }
    p
}
