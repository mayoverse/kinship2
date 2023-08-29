#' @importFrom ggplot2 ggplot geom_polygon aes geom_text annotate ggtitle unit
#' @importFrom ggplot2 scale_fill_manual scale_color_manual geom_segment
#' @importFrom utils txtProgressBar setTxtProgressBar
#' @importFrom stats setNames
NULL

#' Plot pedigrees
#'
#' @description
#' plot objects created with the pedigree function
#'
#' @details
#' Two important parameters control the looks of the result.  One is the user
#' specified maximum width.  The smallest possible width is the maximum number
#' of subjects on a line, if the user's suggestion %' is too low it is
#' increased to 1+ that amount (to give just a little wiggle room). To make a
#' pedigree where all children are centered under parents simply make the width
#' large enough, however, the symbols may get very small.
#'
#' The second is `align`, a vector of 2 alignment parameters $a$ and $b$.
#' For each set of siblings at a set of locations `x` and with parents at
#' `p=c(p1,p2)` the alignment penalty is \\deqn{(1/k^a)\\sum{i=1}{k} [(x_i -
#' (p1+p2)/2)]^2} sum(x- mean(p))^2/(k^a) where k is the number of siblings in
#' the set. when $a=1$ moving a sibship with $k$ sibs one unit to the left or
#' right of optimal will incur the same cost as moving one with only 1 or two
#' sibs out of place.  If $a=0$ then large sibships are harder to move than
#' small ones, with the default value $a=1.5$ they are slightly easier to move
#' than small ones.  The rationale for the default is as long as the parents
#' are somewhere between the first and last siblings the result looks fairly
#' good, so we are more flexible with the spacing of a large family. By
#' tethering all the sibs to a single spot they are kept close to each other.
#' The alignment penalty for spouses is \\eqn{b(x_1 - x_2)^2}{b *(x1-x2)^2},
#' which tends to keep them together.  The size of $b$ controls the relative
#' importance of sib-parent and spouse-spouse closeness.
#'
#' @param df dataframe to use for the pedigree.
#' @param mark vector indicating the text to plot in the center of the box.
#' @param label vector indicating the text to plot bellow the box.
#' @param tips_names vector of column names in dataframe to show when hovering
#' with plotly.
#' @param fill vector of color to fill the box.
#' @param border vector of color for the border of the box.
#' @param ggplot_gen Boolean to indicate if a ggplot object should be created.
#' @param cex controls text size.  Default=1.
#' @param symbolsize controls symbolsize. Default=1.
#' @param branch defines how much angle is used to connect various levels of
#' nuclear families.
#' @param packed default=TRUE.  If TRUE, uniform distance between all
#' individuals at a given level.
#' @param align these parameters control the extra effort spent trying to align
#' children underneath parents, but without making the pedigree too wide.  Set
#' to FALSE to speed up plotting.
#' @param width default=8.  For a packed pedigree, the minimum width allowed in
#' the realignment of pedigrees.
#' @param density defines density used in the symbols.  Takes up to 4 different
#' values.
#' @param mar margin parmeters, as in the `par` function
#' @param angle defines angle used in the symbols.  Takes up to 4 different
#' values.
#' @param keep_par Default = FALSE, allows user to keep the parameter settings
#' the same as they were for plotting (useful for adding extras to the plot)
#' @param subregion 4-element vector for (min x, max x, min depth, max depth),
#' used to edit away portions of the plot coordinates returned by
#' align.pedigree
#' @param pconnect when connecting parent to children the program will try to
#' make the connecting line as close to vertical as possible, subject to it
#' lying inside the endpoints of the line that connects the children by at
#' least `pconnect` people.  Setting this option to a large number will
#' force the line to connect at the midpoint of the children.
#' @param ... Extra options that feed into the plot function.
#'
#' @return an invisible list containing
#' ## plist
#' A list that contains all the position information for
#' plotting the pedigree. This will useful for further functions (yet unwritten)
#' for manipulating the plot, but likely not to an ordinary user.
#' ## x,y
#' The x an and y plot coordinates of each subject in the plot.
#' The coordinate is for the top of the plotted symbol.
#' These will be in the same order as the input pedigree.  If someone in the
#' pedigree does not appear in the plot their coordinates will be NA.  If they
#' appear multiple times one of the instances is chosen.  (Which one is a
#' function of the order in which the pedigree was constructed.)
#' ## boxh
#' The height of the symbol, in user coordinates
#' ## boxw
#' The width of the symbol
#' ## call
#' A copy of the call that generated the plot
#'
#' @examples
#' data(sampleped)
#'
#' pedAll <- with(sampleped, pedigree(id, father, mother,
#'   sex,
#'   affected = cbind(affected, avail),
#'   famid = ped
#' ))
#'
#' ped2 <- pedAll['2']
#'
#' print(ped2)
#'
#' @section Side Effects: creates plot on current plotting device.
#' @seealso `pedigree`
#' @keywords hplot, genetics
#' @include align.R
#' @include plot_fct.R
#' @export
setMethod("plot", c(x = "Pedigree", y = "missing"),
    function(x, mark = NA,
        label = NA, tips_names = NA, fill = "grey", border = "black",
        ggplot_gen = FALSE, cex = 1, symbolsize = 1, branch = 0.6,
        packed = TRUE, align = c(1.5, 2), width = 6, psize = par("pin"),
        title = NULL, density = c(-1, 35, 65, 20),
        mar = c(4.1, 1, 4.1, 1), angle = c(90, 65, 40, 0),
        keep_par = FALSE, subregion, pconnect = 0.5, ...
    ) {
        ped <- x
        df <- x$ped

        id <- df$id
        status <- df$status
        affected <- df$affected
        avail <- df$avail + 1

        ## As of 2020-09, documention in no-web directory is moved to here and a
        ## vignette. Relevant sections in the vignette are marked in this code
        ## with Doc: followed by the section title
        call <- match.call()
        n <- length(id)
        if (n < 3) {
            stop("Cannot plot pedigree with fewer than 3 subjects")
        }

        ## Doc: This portion is documented as the setup-data Dead or Alive
        if (is.null(status)) {
            status <- rep(0, n)
        } else {
            if (!all(status == 0 | status == 1)) {
                stop("Invalid status code")
            }
            if (length(status) != n) {
                stop("Wrong length for status")
            }
        }
        if (!missing(id)) {
            if (length(id) != n) {
                stop("Wrong length for id")
            }
        }

        affected <- set_affected(affected, n, angle, density)

        all_var_checked <- lapply(mget(
            c("avail", "mark", "label", "fill", "border")
        ), function(var) {
            if (length(var) <= 1) {
                return(rep(var, n))
            } else if (length(var) != n) {
                stop(paste(var, "argument must be of length 1 or n"))
            } else {
                return(var)
            }
        })

        avail <- all_var_checked$avail
        mark <- all_var_checked$mark
        label <- all_var_checked$label
        fill <- all_var_checked$fill
        border <- all_var_checked$border

        # Test if all column used for the tips are present in dataframe
        if (!is.na(tips_names)) {
            if (any(!tips_names %in% colnames(df))) {
                stop("Column name :",
                    tips_names[!tips_names %in% colnames(df)],
                    "not present in data"
                )
            }
        }

        ## Doc: Sizing
        plist <- align(ped, packed = packed, width = width, align = align)
        if (!missing(subregion)) {
            plist <- subregion2(plist, subregion)
        }
        xrange <- range(plist$pos[plist$nid > 0])
        maxlev <- nrow(plist$pos)

        frame()
        params_plot <- set_plot_area(
            cex, id, maxlev, xrange, symbolsize, ...
        )

        par_usr <- params_plot$par_usr
        oldpar <- params_plot$oldpar
        boxw <- params_plot$boxw
        boxh <- params_plot$boxh
        labh <- params_plot$labh
        legh <- params_plot$legh

        # Test for presence of errors in par parameters
        if (any(is.na(par_usr))) {
            return(NULL)
        }
        par(usr = par_usr)
        ## Doc: end of sizing Doc: Sizing Doc: subsection: drawbox

        polylist <- polygons(ncol(affected))

        legend_labels <- c(
            "Unaffected and unavailable", "Unaffected and available",
            "Affected and unavailable", "Affected and available"
        )
        colors_plot <- unique(c(fill, border))

        p <- ggplot()
        ## Doc: symbols
        sex <- as.numeric(df$sex)

        ## Add title if exists
        if (!is.null(title)) {
            title(title)
            p <- p + ggtitle(title)
        }

        message("Drawing the individuals", appendLF = TRUE)
        prog_bar <- txtProgressBar(
            0, maxlev, char = "|", width = 50, style = 3
        )
        for (i in 1:maxlev) {
            setTxtProgressBar(prog_bar, i)
            for (j in seq_len(plist$n[i])) {
                k <- plist$nid[i, j]
                p <- drawbox(
                    plist$pos[i, j], i, sex[k], affected[k, ], status[k],
                    avail[k], mark[k], polylist, fill[k], border[k],
                    density, angle, boxw, boxh, p, id[k], df,
                    cex, ggplot_gen, tips_names
                )
                ## Draw id
                p <- draw_text(
                    plist$pos[i, j], i + boxh + labh * 0.7,
                    id[k], p, ggplot_gen, cex
                )
                if (!is.na(label[k])) {
                    # Label added if present under the individual
                    p <- draw_text(
                        plist$pos[i, j], i + boxh + labh * 0.7 * 2,
                        paste0("(", label[k], ")"), p, ggplot_gen, cex * 0.75
                    )
                }
            }
        }

        ## Doc: lines between spouses
        maxcol <- ncol(plist$nid)  # all have the same size
        message("\nDrawing connection spouse", appendLF = TRUE)
        for (i in 1:maxlev) {
            setTxtProgressBar(prog_bar, i)
            tempy <- i + boxh / 2
            if (any(plist$spouse[i, ] > 0)) {
                temp <- (1:maxcol)[plist$spouse[i, ] > 0]
                p <- draw_segment(
                    plist$pos[i, temp] + boxw / 2,
                    rep(tempy, length(temp)),
                    plist$pos[i, temp + 1] - boxw / 2,
                    rep(tempy, length(temp)),
                    p, ggplot_gen
                )

                if (length((1:maxcol)[plist$spouse[i, ] == 2])) {
                    # double line for double marriage
                    p <- draw_segment(
                        plist$pos[i, temp] + boxw / 2,
                        rep(tempy, length(temp)) + boxh / 10,
                        plist$pos[i, temp + 1] - boxw / 2,
                        rep(tempy, length(temp)) + boxh / 10,
                        p, ggplot_gen
                    )
                }
            }
        }

        ## Doc: Lines from offspring to parents
        message("\nDrawing connection childrens", appendLF = TRUE)
        for (i in 2:maxlev) {
            setTxtProgressBar(prog_bar, i)
            zed <- unique(plist$fam[i, ])
            zed <- zed[zed > 0]  # list of family ids

            for (fam in zed) {
                xx <- plist$pos[i - 1, fam + 0:1]
                parentx <- mean(xx)  # midpoint of parents

                # Draw the uplines
                who <- (plist$fam[i, ] == fam)  # The kids of interest
                if (is.null(plist$twins)) {
                    target <- plist$pos[i, who]
                } else {
                    twin_to_left <- (c(0, plist$twins[i, who])[1:sum(who)])
                    # increment if no twin to the left
                    temp <- cumsum(twin_to_left == 0)
                    # 5 sibs, middle 3 are triplets gives 1,2,2,2,3 twin, twin,
                    # singleton gives 1,1,2,2,3
                    tcount <- table(temp)
                    target <- rep(tapply(plist$pos[i, who], temp, mean), tcount)
                }
                yy <- rep(i, sum(who))
                p <- draw_segment(
                    plist$pos[i, who], yy,
                    target, yy - legh,
                    p, ggplot_gen
                )

                p <- draw_mz_twin_connection(
                    plist$twins[i, who],
                    plist$pos[i, who], target, i, legh,
                    p, ggplot_gen
                )

                # Add the horizontal line
                p <- draw_segment(
                    min(target), i - legh,
                    max(target), i - legh,
                    p, ggplot_gen
                )

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
                y1 <- i - legh
                if (branch == 0) {
                    p <- draw_segment(
                        x1, y1,
                        parentx, (i - 1) + boxh / 2,
                        p, ggplot_gen
                    )
                } else {
                    y2 <- (i - 1) + boxh / 2
                    x2 <- parentx
                    ydelta <- ((y2 - y1) * branch) / 2
                    p <- draw_segment(
                        c(x1, x1, x2), c(y1, y1 + ydelta, y2 - ydelta),
                        c(x1, x2, x2), c(y1 + ydelta, y2 - ydelta, y2),
                        p, ggplot_gen
                    )
                }
            }
        }  ## end of parent-child lines

        uid <- unique(plist$nid)
        ## JPS 4/27/17: unique above only applies to rows unique added to
        ## for loop iterator
        imax <- length(unique(uid[uid > 0]))
        i <- 0
        if (imax > 0) {
            message("\nDrawing arcs for same Id", appendLF = TRUE)
            prog_bar <- txtProgressBar(0, imax - 1, width = 50,
                style = 3, char = "|"
            )
        }
        for (id in unique(uid[uid > 0])) {
            setTxtProgressBar(prog_bar, i)
            indx <- which(plist$nid == id)
            if (length(indx) > 1) {
                # subject is a multiple
                tx <- plist$pos[indx]
                ty <- ((row(plist$pos))[indx])[order(tx)]
                tx <- sort(tx)
                for (j in 1:(length(indx) - 1)) {
                    p <- draw_arc(tx[j], ty[j], tx[j + 1], ty[j + 1], p, ggplot_gen)
                }
            }
            i <- i + 1
        }
        close(prog_bar)

        ## Doc: Final
        ckall <- paste(df$id[is.na(match(df$id, df$id[plist$nid]))],
            collapse = ", "
        )
        if (length(ckall > 0)) {
            message("Did not plot the following people: ", ckall, "\n")
        }

        if (!keep_par) {
            par(oldpar)
        }

        rplot <- grab_grob()

        tmp <- match(seq_len(length(df$id)), plist$nid)
        if (ggplot_gen) {
            invisible(list(plist = plist,
                x = plist$pos[tmp], y = row(plist$pos)[tmp],
                boxw = boxw, boxh = boxh,
                call = call, ggplot = p, plot = rplot
            ))
        } else {
            invisible(list(plist = plist,
                x = plist$pos[tmp], y = row(plist$pos)[tmp],
                boxw = boxw, boxh = boxh, call = call, plot = rplot
            ))
        }
    }
)
TRUE


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

#' Draw a box for a pedigree
#'
#' @param x x coordinate
#' @param y y coordinate
#' @param sex sex of the subject
#'
#' @return Plot the box or add it to a ggplot object
drawbox <- function(
    x, y, sex, affected, status, avail, mark, polylist,
    fill, border, density, angle, boxw, boxh, p_plot, id, data_tooltips,
    cex, ggplot_gen = FALSE, tips_names = NULL
) {
    for (i in seq_len(length(affected))) {
        plabel <- mark
        if (is.null(mark[i])) {
            plabel <- "?"
        }
        midx <- x + mean(range(polylist[[sex]][[i]]$x * boxw))
        midy <- y + mean(range(polylist[[sex]][[i]]$y * boxh))
        polygon(
            x + (polylist[[sex]])[[i]]$x * boxw,
            y + (polylist[[sex]])[[i]]$y * boxh,
            col = fill, border = border, density = density[i], angle = angle[i]
        )

        col_text <- "black"
        text(midx, midy, labels = plabel,
            cex = cex / length(affected), col = col_text
        )

        if (ggplot_gen) {
            p_plot <- p_plot + geom_polygon(aes(
                x = x + (polylist[[sex]])[[!!i]]$x * boxw,
                y = y + (polylist[[sex]])[[!!i]]$y * boxh
            ), fill = fill, color = border)
            # To add pattern stripes use ggpattern::geom_polygon_pattern
            # pattern_density = density[i], pattern_angle = angle[i]))

            if (!is.na(tips_names)) {
                tips <- data_tooltips[data_tooltips$id == id, tips_names]
                tips <- tips[][c(!is.na(tips))]
                tips <- paste(colnames(tips), ":", tips, collapse = "<br>")
            } else {
                tips <- NA
            }

            p_plot <- p_plot +
                suppressWarnings(geom_text(aes(
                    x = midx, y = midy, label = plabel,
                    text = tips, color = col_text
                )))
        }
    }
    if (status == 1) {
        draw_segment(
            x - 0.6 * boxw, y + 1.1 * boxh,
            x + 0.6 * boxw, y - 0.1 * boxh,
            p_plot, ggplot_gen
        )
    }
    ## Do a black slash per Beth, old line was (x + .6*boxw, y - .1 * boxh,
    ## col = avail)
    return(p_plot)
}  ## drawbox

#' Draw midpoint MZ twin line
draw_mz_twin_connection <- function(
    twins, pos, target, i, legh, p, ggplot_gen
) {
    ## draw midpoint MZ twin line
    if (any(twins == 1)) {
        who2 <- which(twins == 1)
        temp1 <- (pos[who2] + target[who2]) / 2
        temp2 <- (pos[who2 + 1] + target[who2]) / 2
        yy <- rep(i, length(who2)) - legh / 2
        p <- draw_segment(temp1, yy, temp2, yy, p, ggplot_gen)
    }

    # Add a question mark for those of unknown zygosity
    if (any(twins == 3)) {
        who2 <- which(twins == 3)
        temp1 <- (pos[who2] + target[who2]) / 2
        temp2 <- (pos[who2 + 1] + target[who2]) / 2
        yy <- rep(i, length(who2)) - legh / 2
        p <- draw_text((temp1 + temp2) / 2, yy, "?", p, ggplot_gen)
    }
    p
}

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
    fill, border = NULL, density = NULL, angle = 45
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
            "text", x = x, y = y, label = label, size = cex, color = col
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