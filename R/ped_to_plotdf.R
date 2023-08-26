#' @importFrom plyr rbind.fill
NULL

ped_to_plotdf <- function(
    ped, packed = FALSE, width = 10, align = c(1.5, 2),
    subregion = NULL, cex = 1, symbolsize = cex, pconnect = 0.5, branch = 0.6, ...
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
        angle = numeric(), density = numeric(), cex = numeric(),
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
    id <- plist$nid
    # x position
    pos <- plist$pos
    # y position
    i <- (seq_len(length(plist$nid)) - 1) %% length(plist$n) + 1
    # sex of each box
    sex <- as.numeric(ped$ped$sex)[id[idx]]

    all_aff <- ped$scales$fill
    bord_df <- ped$scales$border
    n_aff <- length(unique(all_aff$order))
    polylist <- polygons(max(1, n_aff))

    # border mods of each box
    border_mods <- ped$ped[id[idx], unique(bord_df[["column"]])]
    border_idx <- match(border_mods, bord_df[["mods"]])

    for (aff in seq_len(n_aff)) {
        aff_df <- all_aff[all_aff$order == aff, ]
        aff_mods <- ped$ped[id[idx], aff_df[["column_mods"]]]
        aff_idx <- match(aff_mods, aff_df[["mods"]])


        # mean range of each box for each polygon for each subregion
        poly_aff <- lapply(polylist, "[[", aff)
        poly_aff_x <- lapply(poly_aff, "[[", "x")
        poly_aff_y <- lapply(poly_aff, "[[", "y")
        poly_aff_x_mr <- sapply(poly_aff_x, function(x) mean(range(x * boxw)))
        poly_aff_y_mr <- sapply(poly_aff_x, function(x) mean(range(x * boxw)))
        p <- data.frame(
            x0 = pos[idx], y0 = i[idx],
            type = paste(names(polylist)[sex], aff, sep = "_"),
            fill = aff_df[aff_idx, "fill"],
            density = aff_df[aff_idx, "density"],
            angle = aff_df[aff_idx, "angle"],
            border = bord_df[border_idx, "border"]
        )

        label <- data.frame(
            x0 = pos[idx] + poly_aff_x_mr[sex],
            y0 = i[idx] +  poly_aff_y_mr[sex],
            label = ped$ped[id[idx], aff_df[["column_values"]]],
            fill = "black",
            type = "text", cex = cex
        )
        plot_df <- rbind.fill(plot_df, p, label)
    }

    ## Add status
    status <- ped$ped[id[idx], "status"]
    idx_dead <- idx[status == 1]

    dead_df <- data.frame(
        x0 = pos[idx_dead] - 0.6 * boxw, y0 = i[idx_dead] + 1.1 * boxh,
        x1 = pos[idx_dead] + 0.6 * boxw, y1 = i[idx_dead] - 0.1 * boxh,
        type = "segments", fill = "black", cex = cex
    )

    plot_df <- rbind.fill(plot_df, dead_df)

    ## Add ids
    id_df <- data.frame(
        x0 = pos[idx], y0 = i[idx] + boxh + labh * 0.7,
        label = id[idx], fill = "black",
        type = "text", cex = cex
    )
    plot_df <- rbind.fill(plot_df, id_df)

    ## Add lines between spouses
    spouses <- which(plist$spouse > 0)
    l_spouses_i <- i[spouses] + boxh / 2
    pos_sp1 <- pos[spouses] + boxw / 2
    pos_sp2 <- pos[spouses + maxlev] - boxw / 2
    l_spouses <- data.frame(
        x0 = pos_sp1, y0 = l_spouses_i,
        x1 = pos_sp2, y1 = l_spouses_i,
        type = "segments", fill = "black", cex = cex
    )
    plot_df <- rbind.fill(plot_df, l_spouses)

    ## Add doubles mariage
    spouses2 <- which(plist$spouse == 2)

    l_spouses2_i <- i[spouses2] + boxh / 2 + boxh / 10
    pos_sp21 <- pos[spouses2]
    pos_sp22 <- pos[spouses2 + maxlev]
    l_spouses2 <- data.frame(
        x0 = pos_sp21 + boxw / 2,
        y0 = l_spouses2_i,
        x1 = pos_sp22 - boxw / 2,
        y1 = l_spouses2_i,
        type = "segments", fill = "black", cex = cex
    )
    plot_df <- rbind.fill(plot_df, l_spouses2)

    ## Children to parents lines
    for (gen in 1:maxlev) {
        zed <- unique(plist$fam[gen, ])
        zed <- zed[zed > 0]  # list of family ids
        for (fam in zed) {
            xx <- pos[gen - 1, fam + 0:1]
            parentx <- mean(xx)  # midpoint of parents

            # Get the horizontal end points of the childrens
            who <- (plist$fam[gen, ] == fam)  # The kids of interest
            if (is.null(plist$twins)) {
                # If no twins, just use the position of the children
                target <- plist$pos[gen, who]
            } else {
                # If twins, use the midpoint of the twins
                twin_to_left <- (c(0, plist$twins[gen, who])[1:sum(who)])
                # increment if no twin to the left
                temp <- cumsum(twin_to_left == 0)
                # 5 sibs, middle 3 are triplets gives 1,2,2,2,3 twin, twin,
                # singleton gives 1,1,2,2,3
                tcount <- table(temp)
                target <- rep(tapply(plist$pos[gen, who], temp, mean), tcount)
            }
            yy <- rep(gen, sum(who)) # Vertical start height

            ## Add the vertical lines from children to midline
            vert <- data.frame(
                x0 = pos[gen, who], y0 = yy,
                x1 = target, y1 = yy - legh,
                type = "segments", fill = "black", cex = cex
            )
            plot_df <- rbind.fill(plot_df, vert)

            ## Draw horizontal MZ twin line
            if (any(plist$twins[gen, who] == 1)) {
                who2 <- which(plist$twins[gen, who] == 1)
                temp1 <- (pos[gen, who][who2] + target[who2]) / 2
                temp2 <- (pos[gen, who][who2 + 1] + target[who2]) / 2
                # Horizontal line at mid point of leg height
                yy <- rep(gen, length(who2)) - legh / 2
                twin_l <- data.frame(
                    x0 = temp1, y0 = yy,
                    x1 = temp2, y1 = yy,
                    type = "segments", fill = "black", cex = cex
                )
                plot_df <- rbind.fill(plot_df, twin_l)
            }

            # Add a question mark for those of unknown zygosity
            if (any(plist$twins[gen, who] == 3)) {
                who2 <- which(plist$twins[gen, who] == 3)
                temp1 <- (pos[gen, who][who2] + target[who2]) / 2
                temp2 <- (pos[gen, who][who2 + 1] + target[who2]) / 2
                yy <- rep(gen, length(who2)) - legh / 2
                twin_lab <- data.frame(
                    x0 = (temp1 + temp2) / 2, y0 = yy,
                    label = "?", fill = "black",
                    type = "text", cex = cex
                )
                plot_df <- rbind.fill(plot_df, twin_lab)
            }

            # Add the horizontal line
            hori <- data.frame(
                x0 = min(target), y0 = gen - legh,
                x1 = max(target), y1 = gen - legh,
                type = "segments", fill = "black", cex = cex
            )
            plot_df <- rbind.fill(plot_df, hori)

            # Draw line to parents.  The original rule corresponded to
            # pconnect a large number, forcing the bottom of each
            # parent-child line to be at the center of the bar uniting
            # the children.
            if (diff(range(target)) < 2 * pconnect) {
                x1 <- mean(range(target))
            } else {
                x1 <- pmax(min(target) + pconnect,
                    pmin(max(target) - pconnect, parentx)
                )
            }
            y1 <- gen - legh
            ## Add the parent to mid line
            if (branch == 0) {
                l_child_par <- data.frame(
                    x0 = x1, y0 = y1,
                    x1 = parentx, y1 = (gen - 1) + boxh / 2,
                    type = "segments", fill = "black", cex = cex
                )
            } else {
                y2 <- (gen - 1) + boxh / 2
                x2 <- parentx
                ydelta <- ((y2 - y1) * branch) / 2
                l_child_par <- data.frame(
                    x0 = c(x1, x1, x2), y0 = c(y1, y1 + ydelta, y2 - ydelta),
                    x1 = c(x1, x2, x2), y1 = c(y1 + ydelta, y2 - ydelta, y2),
                    type = "segments", fill = "black", cex = cex
                )
            }
            plot_df <- rbind.fill(plot_df, l_child_par)
        }
    }  ## end of parent-child lines

    uid_all <- unique(plist$nid[plist$nid > 0])
    ## JPS 4/27/17: unique above only applies to rows unique added to
    ## for loop iterator
    uid <- 8
    for (uid in uid_all) {
        indx <- which(plist$nid == uid)
        if (length(indx) > 1) {
            # subject is a multiple
            tx <- plist$pos[indx]
            ty <- ((row(plist$pos))[indx])[order(tx)]
            tx <- sort(tx)
            for (j in seq_len(length(indx) - 1)) {
                arc <- data.frame(
                    x0 = tx[j + 0], y0 = ty[j + 0],
                    x1 = tx[j + 1], y1 = ty[j + 1],
                    type = "arc", fill = "black", cex = cex
                )
                p <- draw_arc(tx[j + 0:1], ty[j + 0:1])
            }
        }
    }
    list(df = plot_df, par = params_plot)
}
