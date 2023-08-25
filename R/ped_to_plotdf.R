#' @importFrom plyr rbind.fill
NULL

ped_to_plotdf <- function(
    ped, packed = FALSE, width = 10, align = c(1.5, 2),
    subregion = NULL, cex = 1, symbolsize = cex, ...
) {

    famlist <- unique(ped$ped$family)
    if (length(famlist) > 1) {
        nfam <- length(famlist)
        all_df <- vector("list", nfam)
        for (i_fam in famlist) {
            ped_fam <- ped[ped$ped$family == i_fam]
            all_df[[i_fam]] <- ped_to_plotdf(ped_fam, packed, width, align,
                subregion, cex, symbolsize, ...
            )
        }
        return(all_df)
    }

    plot_df <- data.frame(
        x0 = numeric(), y0 = numeric(), x1 = numeric(), y1 = numeric(),
        type = character(), fill = character(), border = character(),
        angle = numeric(), density = numeric(),
        label = character(), tips = character()
    )
    plist <- align(ped, packed = packed, width = width, align = align)

    if (!is.null(subregion)) {
        plist <- subregion2(plist, subregion)
    }
    xrange <- range(plist$pos[plist$nid > 0])
    maxlev <- nrow(plist$pos)

    params_plot <- set_plot_area(
        cex, ped$ped$id, maxlev, xrange, symbolsize, ...
    )

    par_usr <- params_plot$par_usr
    oldpar <- params_plot$oldpar
    boxw <- params_plot$boxw
    boxh <- params_plot$boxh
    labh <- params_plot$labh
    legh <- params_plot$legh

    ## Get all boxes to plot
    # idx is the index of the boxes in the alignement
    idx <- which(plist$nid > 0)
    # index value in the ped of each box
    id <- plist$nid[idx]
    # x position of each box
    pos <- plist$pos[idx]
    # y position of each box
    i <- (idx - 1) %% length(plist$n) + 1
    # sex of each box
    sex <- as.numeric(ped$ped$sex)[id]

    all_aff <- ped$scales$fill
    bord_df <- ped$scales$border
    n_aff <- length(unique(all_aff$order))
    polylist <- polygons(max(1, n_aff))

    # border mods of each box
    border_mods <- ped$ped[id, unique(bord_df[["column"]])]
    border_idx <- match(border_mods, bord_df[["mods"]])

    for (aff in seq_len(n_aff)) {
        aff_df <- all_aff[all_aff$order == aff, ]
        aff_mods <- ped$ped[id, aff_df[["column_mods"]]]
        aff_idx <- match(aff_mods, aff_df[["mods"]])


        # mean range of each box for each polygon for each subregion
        poly_aff <- lapply(polylist, "[[", aff)
        poly_aff_x <- lapply(poly_aff, "[[", "x")
        poly_aff_y <- lapply(poly_aff, "[[", "y")
        poly_aff_x_mr <- sapply(poly_aff_x, function(x) mean(range(x * boxw)))
        poly_aff_y_mr <- sapply(poly_aff_x, function(x) mean(range(x * boxw)))
        p <- data.frame(
            x0 = pos, y0 = i,
            type = paste(names(polylist)[sex], aff, sep = "_"),
            fill = aff_df[aff_idx, "fill"],
            density = aff_df[aff_idx, "density"],
            angle = aff_df[aff_idx, "angle"],
            border = bord_df[border_idx, "border"]
        )

        label <- data.frame(
            x0 = pos + poly_aff_x_mr[sex],
            y0 = i +  poly_aff_y_mr[sex],
            label = ped$ped[id, aff_df[["column_values"]]],
            fill = "black",
            type = "text"
        )
        plot_df <- rbind.fill(plot_df, p, label)
    }
    
    plot_df
}
