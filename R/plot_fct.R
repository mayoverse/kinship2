## Doc: Subregions and subsetting

#' Routine to subset a pedigree
#'
#' @param plist a pedigree list
#' @param subreg a vector of length 4 giving the start and end positions
#'  of the subregion and the start and end depths
#'
#' @return a pedigree list
#'
#' @export
subregion2 <- function(plist, subreg) {
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
}  # end subregion2()

## Plotting function
#' Generate a circular element
#'
#' @param nslice number of slices
#' @param n
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
}  ## end circfun()


## Doc: polyfun
#' Generate a polygonal element
#'
#' @param nslice number of slices
#' @param object Element form which to generate the polygon
#' containing x and y coordinates and theta
#'
#' @return a list of x and y coordinates
#'
#' @export
polyfun <- function(nslice, object) {
    # make the indirect segments view
    zmat <- matrix(0, ncol = 4, nrow = length(object$x))
    zmat[, 1] <- object$x
    zmat[, 2] <- c(object$x[-1], object$x[1]) - object$x
    zmat[, 3] <- object$y
    zmat[, 4] <- c(object$y[-1], object$y[1]) - object$y

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
    nvertex <- length(object$x)
    temp <- data.frame(
        indx = c(1:ns1, rep(0, nvertex)),
        theta = c(theta, object$theta),
        x = c(x, object$x), y = c(y, object$y)
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
}  ## end polyfun()

#' Create a list of the different polygonal elements
#'
#' @param naffection number of affection status
#'
#' @return a list of polygonal elements with x, y coordinates
#' and theta
#'
#' @export
polygons <- function(naffection = 1) {
    if (naffection == 1) {
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
        square <- polyfun(naffection, list(
            x = c(-0.5, -0.5, 0.5, 0.5),
            y = c(-0.5, 0.5, 0.5, -0.5),
            theta = -c(3, 5, 7, 9) * pi / 4
        ))
        circle <- circfun(naffection)
        diamond <- polyfun(naffection, list(
            x = c(0, -0.5, 0, 0.5),
            y = c(-0.5, 0, 0.5, 0),
            theta = -(1:4) * pi / 2
        ))
        triangle <- polyfun(naffection, list(
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
#' @param x x coordinate
#' @param y y coordinate
#' @param label text to be displayed
#' @param p ggplot object
#' @param ggplot_gen logical, if TRUE add the text to the ggplot object
#'
#' @return Plot the text or add it to a ggplot object
#'
#' @export
draw_text <- function(x, y, label, p, ggplot_gen = FALSE, cex = 1, col = NULL) {
    text(x, y, label, cex = cex, col = col)
    if (ggplot_gen) {
        p <- p + annotate(
            "text", x = x, y = y, label = label, size = cex / 0.3, color = col
        )
    }
    p
}

## Doc: 4 arcs for multiple instances of subj
#' Draw arcs for multiple instances of a subject
#'
#' @param x x coordinate
#' @param y y coordinate
#' @param p ggplot object
#' @param ggplot_gen logical, if TRUE add the arcs to the ggplot object
#'
#' @return Plot the arcs or add it to a ggplot object
#'
#' @export
draw_arc <- function(x0, y0, x1, y1, p, ggplot_gen = FALSE, cex = 1, col = "black") {
    xx <- seq(x0, x1, length = 15)
    yy <- seq(y0, y1, length = 15) + (seq(-7, 7))^2 / 98 - 0.5
    lines(xx, yy, lty = 2, lwd = cex, col = col)
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
## Plotting region
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
    par_usr <- c(xrange[1] - boxw / 2, xrange[2] + boxw / 2,
        maxlev + boxh + stemp3 + stemp2 / 2, 1
    )
    list(par_usr = par_usr, old_par = old_par, boxw = boxw,
        boxh = boxh, labh = labh, legh = legh
    )
}

#' Generate the affected matrix
#'
#' Doc: still part of setup/data affected is a 0/1 matrix of any marker
#' data.  It may be attached to the pedigree or added here.  It can be a
#' vector of length [[n]] or a matrix with [[n]] rows. If not present,
#' the default is to plot open symbols without shading or color
#' If affected is a matrix, then the shading and/or density value for
#' ith column is taken from the ith element of the angle/density
#' arguments.
#' For purposes within the plot method, NA values in 'affected'
#' are coded to -1, and plotted as a question mark (?) in the plot
#' symbol region for that affected status
#'
#' @param affected a vector or matrix of 0/1 values
#' @param n number of subjects
#' @param angle angle of shading
#' @param density density of shading
#'
#' @return a matrix of 0/1 values
#'
#' @export
set_affected <- function(affected, n, angle, density) {
    if (is.null(affected)) {
        affected <- matrix(0, nrow = n)
    } else {
        if (is.matrix(affected)) {
            if (nrow(affected) != n) {
                stop("Wrong number of rows in affected")
            }
            if (is.logical(affected)) {
                affected <- 1 * affected
            }
            if (ncol(affected) > length(angle) ||
                    ncol(affected) > length(density)
            ) {
                stop("More columns in the affected matrix",
                    "than angle/density values"
                )
            }
        } else {
            if (length(affected) != n) {
                stop("Wrong length for affected")
            }
            if (is.logical(affected)) {
                affected <- as.numeric(affected)
            }
            if (is.factor(affected)) {
                affected <- as.numeric(affected) - 1
            }
        }
        if (max(affected, na.rm = TRUE) > min(affected, na.rm = TRUE)) {
            affected <- matrix(affected - min(affected, na.rm = TRUE),
                nrow = n
            )
        } else {
            affected <- matrix(affected, nrow = n)
        }

        ## JPS 4/28/17 bug fix b/c some cases NAs are not set to -1
        affected[is.na(affected)] <- -1
        if (!all(affected == 0 | affected == 1 | affected == -1)) {
            stop("Invalid code for affected status")
        }
    }
    affected
}