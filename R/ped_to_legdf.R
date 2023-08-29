setGeneric("ped_to_legdf", function(obj, ...) {
    standardGeneric("ped_to_legdf")
})

setMethod("ped_to_legdf", "Pedigree", function(obj,
    boxh = 1, boxw = 1, cex = 1, ...
) {
    par_usr <- list(boxh = boxh, boxw = boxw, cex = cex)
    ped <- obj
    plot_df <- data.frame(
        id = character(),
        x0 = numeric(), y0 = numeric(), x1 = numeric(), y1 = numeric(),
        type = character(), fill = character(), border = character(),
        angle = numeric(), density = numeric(), cex = numeric(),
        label = character(), tips = character(), adjx = numeric(),
        adjy = numeric()
    )
    sex_equiv <- c("Male", "Female", "Terminated", "Unknown")
    all_lab <- c(sex_equiv, ped$scales$labels, ped$scales$fill$labels)

    psize <- par("pin")  # plot region in inches
    stemp1 <- max(strwidth(all_lab, units = "inches", cex = cex))

    set_plot_area(cex, all_lab, 4, 5, 5)

    all_sex <- unique(as.numeric(ped$ped$sex))

    # Sex
    poly1 <- polygons(1)
    sex <- data.frame(
        x0 = 1, y0 = all_sex * 1.5,
        type = paste(names(poly1)[all_sex], 1, 1, sep = "_"),
        fill = "white",
        border = "black",
        id = "sex"
    )

    sex_label <- data.frame(
        x0 = 2, y0 = all_sex * 1.5 + boxh / 2,
        label = sex_equiv[all_sex], cex = cex,
        type = "text",
        fill = "black",
        id = "sex_label"
    )

    plot_df <- rbind.fill(plot_df, sex, sex_label)

    # Border
    bord_df <- ped$scales$border
    border_mods <- unique(ped$ped[, unique(bord_df[["column"]])])
    border <- data.frame(
        x0 = 3, y0 = seq_along(border_mods) * 1.5,
        type = rep("square_1_1", length(border_mods)),
        border = bord_df$border[match(border_mods, bord_df$mods)],
        fill = "white",
        id = "border"
    )
    lab <- bord_df$labels[match(border_mods, bord_df$mods)]
    lab[is.na(lab)] <- "NA"
    border_label <- data.frame(
        x0 = 4, y0 = seq_along(border_mods) * 1.5 + boxh / 2,
        label = lab, cex = cex,
        type = "text",
        fill = "black",
        id = "border_label"
    )

    plot_df <- rbind.fill(plot_df, border, border_label)

    ## Affected
    all_aff <- ped$scales$fill
    n_aff <- length(unique(all_aff$order))

    for (aff in seq_len(n_aff)) {
        aff_df <- all_aff[all_aff$order == aff, ]
        aff_mods <- unique(ped$ped[, unique(aff_df[["column_mods"]])])

        aff_bkg <- data.frame(
            x0 = 3 + aff * 2 * stemp1, y0 = seq_along(aff_mods) * 1.5,
            type = rep(paste("square", 1, 1, sep = "_"),
                length(aff_mods)
            ),
            border = "black",
            fill = "white",
            id = paste("aff_bkg", aff, aff_mods, sep = "_")
        )

        affected <- data.frame(
            x0 = 3 + aff * 2 * stemp1, y0 = seq_along(aff_mods) * 1.5,
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
            x0 = 4 + aff * 2 * stemp1, y0 = seq_along(aff_mods) * 1.5 + boxh / 2,
            label = lab, cex = cex,
            type = "text",
            fill = "black",
            id = paste("affected_label", aff, aff_mods, sep = "_")
        )
        plot_df <- rbind.fill(plot_df, aff_bkg, affected, affected_label)
    }

    ## Add status
    status <- unique(ped$ped[, "status"])

    if (any(status > 0)) {
        status <- data.frame(
            x0 = -2, y0 = c(1, 2) * 1.5,
            type = "square_1",
            border = "black",
            fill = "white",
            id = "status"
        )
        status_seg <- data.frame(
            x0 = -2 - 0.6 * par$boxw, y0 = 2 * 1.5 + 1.1 * par$boxh,
            x1 = -2 + 0.6 * par$boxw, y1 = 2 * 1.5 - 0.1 * par$boxh,
            type = "segments", fill = "black",
            id = "dead"
        )
        status_label <- data.frame(
            x0 = -2, y0 = c(1, 2) * 1.5 + boxh / 2,
            label = c("Alive", "Dead"),
            type = "text", cex = cex,
            fill = "black",
            id = "status_label"
        )

        plot_df <- rbind.fill(plot_df, status, status_seg, status_label)
    }
    plot_df[plot_df$type == "text", "adjx"] <- 0
    par_usr$usr <- c(
        min(df$x0) - 1, max(df$x0) + stemp1 * 3,
        min(df$y0), max(df$y0) + 1
    )
    list(df = plot_df, par_usr = par_usr)
})
