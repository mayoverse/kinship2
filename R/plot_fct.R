#' @importFrom graphics par strwidth strheight text lines
#' @importFrom graphics polygon frame segments
NULL

## Doc: Subregions and subsetting

#' Routine to subset a pedigree
#'
#' @param subreg a vector of length 4 giving the start and end positions
#' of the subregion and the start and end depths
#' @inheritParams auto_hint
#'
#' @return a pedigree list
#'
#' @export
subregion <- function(plist, subreg) {
    if (subreg[3] < 1 || subreg[4] > length(plist$n)) {
        stop("Invalid depth indices in subreg")
    }
    lkeep <- subreg[3]:subreg[4]
    for (i in lkeep) {
        if (!any(plist$pos[i, ] >= subreg[1] &
                    plist$pos[i, ] <= subreg[2]
            )) {
            stop(paste("No subjects retained on level", i))
        }
    }

    nid2 <- plist$nid[lkeep, ]
    n2 <- plist$n[lkeep]
    pos2 <- plist$pos[lkeep, ]
    spouse2 <- plist$spouse[lkeep, ]
    fam2 <- plist$fam[lkeep, ]
    if (!is.null(plist$twins))
        twin2 <- plist$twins[lkeep, ]

    for (i in seq_len(nrow(nid2))) {
        keep <- which(pos2[i, ] >= subreg[1] & pos2[i, ] <= subreg[2])
        nkeep <- length(keep)
        n2[i] <- nkeep
        nid2[i, 1:nkeep] <- nid2[i, keep]
        pos2[i, 1:nkeep] <- pos2[i, keep]
        spouse2[i, 1:nkeep] <- spouse2[i, keep]
        fam2[i, 1:nkeep] <- fam2[i, keep]
        if (!is.null(plist$twins))
            twin2[i, 1:nkeep] <- twin2[i, keep]

        if (i < nrow(nid2)) {
            # look ahead
            tfam <- match(fam2[i + 1, ], keep, nomatch = 0)
            fam2[i + 1, ] <- tfam
            if (any(spouse2[i, tfam] == 0)) {
                stop("A subregion cannot separate parents")
            }
        }
    }

    n <- max(n2)
    out <- list(n = n2[1:n], nid = nid2[, 1:n, drop = FALSE],
        pos = pos2[, 1:n, drop = FALSE],
        spouse = spouse2[, 1:n, drop = FALSE],
        fam = fam2[, 1:n, drop = FALSE]
    )
    if (!is.null(plist$twins)) {
        out$twins <- twin2[, 1:n, drop = FALSE]
    }
    out
}  # end subregion()

## Plotting function
#' Generate a circular element
#'
#' @param nslice number of slices
#' @param n Total number of points in the circle
#'
#' @return a list of x and y coordinates
#'
#' @export
circfun <- function(nslice, n = 50) {
    nseg <- ceiling(n / nslice)  # segments of arc per slice

    theta <- -pi / 2 - seq(0, 2 * pi, length = nslice + 1)
    out <- vector("list", nslice)
    for (i in 1:nslice) {
        theta2 <- seq(theta[i], theta[i + 1], length = nseg)
        out[[i]] <- list(x = c(0, cos(theta2) / 2),
            y = c(0, sin(theta2) / 2) + 0.5
        )
    }
    out
}


## Doc: polyfun
#' Generate a polygonal element
#'
#' @param nslice number of slices
#' @param coor Element form which to generate the polygon
#' containing x and y coordinates and theta
#'
#' @return a list of x and y coordinates
#'
#' @export
polyfun <- function(nslice, coor) {
    # make the indirect segments view
    zmat <- matrix(0, ncol = 4, nrow = length(coor$x))
    zmat[, 1] <- coor$x
    zmat[, 2] <- c(coor$x[-1], coor$x[1]) - coor$x
    zmat[, 3] <- coor$y
    zmat[, 4] <- c(coor$y[-1], coor$y[1]) - coor$y

    # Find the cutpoint for each angle Yes we could vectorize the loop, but
    # nslice is never bigger than about 10 (and usually <5), so why be
    # obscure?
    ns1 <- nslice + 1
    theta <- -pi / 2 - seq(0, 2 * pi, length = ns1)
    x <- y <- double(ns1)
    for (i in 1:ns1) {
        z <- (tan(theta[i]) * zmat[, 1] - zmat[, 3]) /
            (zmat[, 4] - tan(theta[i]) * zmat[, 2])
        tx <- zmat[, 1] + z * zmat[, 2]
        ty <- zmat[, 3] + z * zmat[, 4]
        inner <- tx * cos(theta[i]) + ty * sin(theta[i])
        indx <- which(is.finite(z) & z >= 0 & z <= 1 & inner > 0)
        x[i] <- tx[indx]
        y[i] <- ty[indx]
    }
    nvertex <- length(coor$x)
    temp <- data.frame(
        indx = c(1:ns1, rep(0, nvertex)),
        theta = c(theta, coor$theta),
        x = c(x, coor$x), y = c(y, coor$y)
    )
    temp <- temp[order(-temp$theta), ]
    out <- vector("list", nslice)
    for (i in 1:nslice) {
        rows <- which(temp$indx == i):which(temp$indx == (i + 1))
        out[[i]] <- list(
            x = c(0, temp$x[rows]),
            y = c(0, temp$y[rows]) + 0.5
        )
    }
    out
}

#' Create a list of the different polygonal elements
#'
#' @param nslice number of slices
#'
#' @return a list of polygonal elements with x, y coordinates
#' and theta
#'
#' @export
polygons <- function(nslice = 1) {
    if (nslice == 1) {
        polylist <- list(
            square = list(list(
                x = c(-1, -1, 1, 1) / 2,
                y = c(0, 1, 1, 0)
            )),
            circle = list(list(
                x = 0.5 * cos(seq(0, 2 * pi, length = 50)),
                y = 0.5 * sin(seq(0, 2 * pi, length = 50)) + 0.5
            )),
            diamond = list(list(
                x = c(0, -0.5, 0, 0.5),
                y = c(0, 0.5, 1, 0.5)
            )),
            triangle = list(list(
                x = c(0, -0.56, 0.56),
                y = c(0, 1, 1)
            ))
        )
    } else {
        square <- polyfun(nslice, list(
            x = c(-0.5, -0.5, 0.5, 0.5),
            y = c(-0.5, 0.5, 0.5, -0.5),
            theta = -c(3, 5, 7, 9) * pi / 4
        ))
        circle <- circfun(nslice)
        diamond <- polyfun(nslice, list(
            x = c(0, -0.5, 0, 0.5),
            y = c(-0.5, 0, 0.5, 0),
            theta = -(1:4) * pi / 2
        ))
        triangle <- polyfun(nslice, list(
            x = c(-0.56, 0, 0.56),
            y = c(-0.5, 0.5, -0.5),
            theta = c(-2, -4, -6) * pi / 3
        ))
        polylist <- list(
            square = square, circle = circle,
            diamond = diamond, triangle = triangle
        )
    }  ## else
    polylist
}

#'@importFrom ggplot2 geom_polygon aes annotate
NULL

#' Draw segments for a pedigree
#'
#' @param x0 x coordinate of the first point
#' @param y0 y coordinate of the first point
#' @param x1 x coordinate of the second point
#' @param y1 y coordinate of the second point
#' @param p ggplot object
#' @param ggplot_gen logical, if TRUE add the segments to the ggplot object
#' @param col line color
#' @param lwd line width
#' @param lty line type
#'
#' @return Plot the segments or add it to a ggplot object
#'
#' @export
draw_segment <- function(
    x0, y0, x1, y1,
    p, ggplot_gen,
    col = par("fg"), lwd = par("lwd"), lty = par("lty")
) {
    segments(x0, y0, x1, y1, col = col, lty = lty, lwd = lwd)
    if (ggplot_gen) {
        p <- p + annotate("segment", x = x0, y = y0,
            xend = x1, yend = y1, color = col, linetype = lty, linewidth = lwd
        )
    }
    p
}

#' Draw a polygon for a pedigree
#'
#' @param x x coordinates
#' @param y y coordinates
#' @param fill fill color
#' @param border border color
#' @param density density of shading
#' @param angle angle of shading
#' @inheritParams draw_segment
#'
#' @return Plot the polygon or add it to a ggplot object
#' @export
draw_polygon <- function(
    x, y, p, ggplot_gen = FALSE,
    fill = "grey", border = NULL, density = NULL, angle = 45
) {
    polygon(x, y, col = fill, border = border, density = density, angle = angle)
    if (ggplot_gen) {
        p <- p + geom_polygon(aes(x = x, y = y), fill = fill, color = border)
        # To add pattern stripes use ggpattern::geom_polygon_pattern
        # pattern_density = density[i], pattern_angle = angle[i]))
    }
    p
}

#' Draw text for a pedigree
#'
#' @param label text to be displayed
#' @param cex character expansion of the text
#' @param col text color
#' @param adjx x adjustment
#' @param adjy y adjustment
#' @inheritParams draw_segment
#' @inheritParams draw_polygon
#'
#' @return Plot the text or add it to a ggplot object
#'
#' @export
draw_text <- function(x, y, label, p, ggplot_gen = FALSE,
    cex = 1, col = NULL, adjx = 0, adjy = 0
) {
    text(x, y, label, cex = cex, col = col, adj = c(adjx, adjy))
    if (ggplot_gen) {
        p <- p + annotate(
            "text", x = x, y = y, label = label, size = cex / 0.3, color = col
        )
    }
    p
}

## Doc: 4 arcs for multiple instances of subj
#' Draw arcs for multiple instances of a subject
#' @inheritParams draw_segment
#'
#' @return Plot the arcs or add it to a ggplot object
#'
#' @export
draw_arc <- function(x0, y0, x1, y1, p, ggplot_gen = FALSE, lwd = 1,
    col = "black"
) {
    xx <- seq(x0, x1, length = 15)
    yy <- seq(y0, y1, length = 15) + (seq(-7, 7))^2 / 98 - 0.5
    lines(xx, yy, lty = 2, lwd = lwd, col = col)
    if (ggplot_gen) {
        p <- p + annotate("line", xx, yy, linetype = "dashed")
    }
    return(p)
}

#' @importFrom gridGraphics grid.echo
#' @importFrom grid grid.grab
NULL

#' Register the plot
#'
#' @export
grab_grob <- function() {
    grid.echo()
    grid.grab()
}

#' Set plotting area
#'
#' @param cex character expansion
#' @param id id of the subject
#' @param maxlev maximum level
#' @param xrange range of x values
#' @param symbolsize size of the symbols
#' @param ... other arguments passed to [par()]
#'
#' @return a list of user coordinates, old par, box width, box height,
#' label height and leg height
#' @keywords internal
#' @export
set_plot_area <- function(cex, id, maxlev, xrange, symbolsize, ...) {
    old_par <- par(xpd = TRUE, ...)  ## took out mar=mar
    psize <- par("pin")  # plot region in inches
    stemp1 <- strwidth("ABC", units = "inches", cex = cex) * 2.5 / 3
    stemp2 <- strheight("1g", units = "inches", cex = cex)
    stemp3 <- max(strheight(id, units = "inches", cex = cex))

    ht1 <- psize[2] / maxlev - (stemp3 + 1.5 * stemp2)
    if (ht1 <= 0) {
        stop("Labels leave no room for the graph, reduce cex")
    }
    ht2 <- psize[2] / (maxlev + (maxlev - 1) / 2)
    wd2 <- 0.8 * psize[1] / (0.8 + diff(xrange))

    boxsize <- symbolsize * min(ht1, ht2, stemp1, wd2)  # box size in inches
    # horizontal scale in inches
    hscale <- (psize[1] - boxsize) / diff(xrange)
    vscale <- (psize[2] - (stemp3 + stemp2 / 2 + boxsize)) /
        max(1, maxlev - 1)
    boxw <- boxsize / hscale  # box width in user units
    boxh <- boxsize / vscale  # box height in user units
    labh <- stemp2 / vscale  # height of a text string
    # how tall are the 'legs' up from a child
    legh <- min(1 / 4, boxh * 1.5)
    usr <- c(xrange[1] - boxw / 2, xrange[2] + boxw / 2,
        maxlev + boxh + stemp3 + stemp2 / 2, 1
    )
    list(usr = usr, old_par = old_par, boxw = boxw,
        boxh = boxh, labh = labh, legh = legh
    )
}
