#' @importFrom ggplot2 ggplot ggtitle
#' @importFrom stringr str_split_fixed
NULL

#' @include plot_fct.R
#' @export
plot_from_df <- function(
    df, par_usr = NULL, naff = 1, title = NULL, ggplot_gen = FALSE, boxw = 1,
    boxh = 1
) {
    frame()
    if (!is.null(par_usr)) {
        par(usr = par_usr)
    }
    polylist <- polygons(naff)
    p <- ggplot()

    ## Add title if exists
    if (!is.null(title)) {
        title(title)
        p <- p + ggtitle(title)
    }

    ## Add boxes
    all_types <- unlist(lapply(names(polylist),
        paste, seq_len(naff), sep = "_"
    ))
    boxes <- df[df$type %in% all_types, ]
    boxes[c("poly", "naff")] <- str_split_fixed(boxes$type, "_", 2)
    boxes$angle[boxes$angle == "NA"] <- 45

    for (i in seq_len(dim(boxes)[1])){
        poly <- polylist[[boxes$poly[i]]][[as.numeric(boxes$naff[i])]]
        draw_polygon(
            boxes$x0[i] + poly$x * boxw,
            boxes$y0[i] + poly$y * boxh,
            p, ggplot_gen, boxes$fill[i], boxes$border[i], boxes$density[i],
            boxes$angle[i]
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
