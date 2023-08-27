#' @importFrom ggplot2 ggplot geom_polygon aes geom_text annotate ggtitle
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
#' data(sample.ped)
#'
#' pedAll <- with(sample.ped, pedigree(id, father, mother,
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
