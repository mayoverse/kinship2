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
#' The second is \code{align}, a vector of 2 alignment parameters $a$ and $b$.
#' For each set of siblings at a set of locations \code{x} and with parents at
#' \code{p=c(p1,p2)} the alignment penalty is \deqn{(1/k^a)\sum{i=1}{k} [(x_i -
#' (p1+p2)/2)]^2} sum(x- mean(p))^2/(k^a) where k is the number of siblings in
#' the set. when $a=1$ moving a sibship with $k$ sibs one unit to the left or
#' right of optimal will incur the same cost as moving one with only 1 or two
#' sibs out of place.  If $a=0$ then large sibships are harder to move than
#' small ones, with the default value $a=1.5$ they are slightly easier to move
#' than small ones.  The rationale for the default is as long as the parents
#' are somewhere between the first and last siblings the result looks fairly
#' good, so we are more flexible with the spacing of a large family. By
#' tethering all the sibs to a single spot they are kept close to each other.
#' The alignment penalty for spouses is \eqn{b(x_1 - x_2)^2}{b *(x1-x2)^2},
#' which tends to keep them together.  The size of $b$ controls the relative
#' importance of sib-parent and spouse-spouse closeness.
#'
#' @param df dataframe to use for the pedigree.
#' @param id id variable - used for labeling.
#' @param status can be missing.  If it exists, 0=alive/missing and 1=death.
#' @param affected vector, or matrix with up to 4 columns for affected
#' indicators. Subject's symbol is divided into sections for each status,
#' shaded if indicator is 1, not-shaded for 0, and symbol "?" if missing (NA)
#' @param avail vector indicating the availability of the individual.
#' If it exists, 0=unavailable and 1=available.
#' @param mark vector indicating the text to plot in the center of the box.
#' @param label vector indicating the text to plot bellow the box.
#' @param tips_names vector of column names in dataframe to show when hovering
#' with plotly.
#' @param fill vector of color to fill the box.
#' @param border vector of color for the border of the box.
#' @param ggplot_gen Boolean to indicate if a ggplot object should be created.
#' @param cex controls text size.  Default=1.
#' @param col color for each id.  Default assigns the same color to everyone.
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
#' @param mar margin parmeters, as in the \code{par} function
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
#' least \code{pconnect} people.  Setting this option to a large number will
#' force the line to connect at the midpoint of the children.
#' @param \dots Extra options that feed into the plot function.
#'
#' @return an invisible list containing
#' \item{plist}{a list that contains all the position information for
#' plotting the pedigree. This will useful for further functions (yet unwritten)
#' for manipulating the plot, but likely not to an ordinary user.}
#' \item{x,y}{the x an and y plot coordinates of each subject in the plot.
#' The coordinate is for the top of the plotted symbol.
#' These will be in the same order as the input pedigree.  If someone in the
#' pedigree does not appear in the plot their coordinates will be NA.  If they
#' appear multiple times one of the instances is chosen.  (Which one is a
#' function of the order in which the pedigree was constructed.)}
#' \item{boxh}{the height of the symbol, in user coordinates}
#' \item{boxw}{the width of the symbol}
#' \item{call}{a copy of the call that generated the plot}
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
#' ped2 <- pedAll["2"]
#'
#' print(ped2)
#'
#' @section Side Effects: creates plot on current plotting device.
#' @seealso \code{\link{pedigree}}
#' @keywords hplot, genetics
#' @export plot.pedigree
plot.pedigree <- function(df, id = df$id, status = df$status,
                          affected = df$affected, avail = df$avail + 1,
                          mark = NA, label = NA,
                          tips_names = NA,
                          fill = "white", border = "black",
                          ggplot_gen = FALSE,
                          cex = 1, symbolsize = 1, branch = 0.6,
                          packed = TRUE, align = c(1.5, 2), width = 6,
                          psize = par("pin"), ped = NA,
                          density = c(-1, 35, 65, 20), mar = c(4.1, 1, 4.1, 1),
                          angle = c(90, 65, 40, 0), keep_par = FALSE,
                          subregion, pconnect = .5, ...) {
  ## As of 2020-09, documention in no-web directory is moved to here and a
  ## vignette. Relevant sections in the vignette are marked in this code with
  ## Doc: followed by the section title

  call <- match.call()
  n <- length(df$id)
  if (n < 3) {
    stop("Cannot plot pedigree with fewer than 3 subjects")
  }

  ## Doc: This portion is documented as the setup-data
  if (is.null(status)) { # Dead or Alive
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
      print(length(id))
      stop("Wrong length for id")
    }
  }
  ## Doc: still part of setup/data
  ## affected is a 0/1 matrix of any marker data.  It may be attached to the
  ## pedigree or added here.  It can be a vector of length [[n]] or a matrix
  ## with [[n]] rows. If not present, the default is to plot open symbols
  ## without shading or color

  ## If affected is a matrix, then the shading and/or density value for ith
  ## column is taken from the ith element of the angle/density arguments.

  ## For purposes within the plot method, NA values in ``affected'' are
  ## coded to -1, and plotted as a question mark (?) in the plot symbol
  ## region for that affected status

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
        ncol(affected) > length(density)) {
        stop("More columns in the affected matrix than angle/density values")
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
      affected <- matrix(affected - min(affected, na.rm = TRUE), nrow = n)
      ## affected[is.na(affected)] <- -1
    } else {
      affected <- matrix(affected, nrow = n)
    }

    ## JPS 4/28/17 bug fix b/c some cases NAs are not set to -1
    affected[is.na(affected)] <- -1
    if (!all(affected == 0 | affected == 1 | affected == -1)) {
      stop("Invalid code for affected status")
    }
  }

  all_var_checked <- lapply(
    mget(c("avail", "mark", "label", "fill", "border")),
    function(var) {
      if (length(var) <= 1) {
        return(rep(var, n))
      } else if (length(var) != n) {
        stop(paste(var, "argument must be of length 1 or n"))
      } else {
        return(var)
      }
    }
  )

  avail <- all_var_checked$avail
  mark <- all_var_checked$mark
  label <- all_var_checked$label
  fill <- all_var_checked$fill
  border <- all_var_checked$border

  ## Doc: Subregions and subsetting
  subregion2 <- function(plist, subreg) {
    if (subreg[3] < 1 || subreg[4] > length(plist$n)) {
      stop("Invalid depth indices in subreg")
    }
    lkeep <- subreg[3]:subreg[4]
    for (i in lkeep) {
      if (!any(plist$pos[i, ] >= subreg[1] & plist$pos[i, ] <= subreg[2])) {
        stop(paste("No subjects retained on level", i))
      }
    }
    # Test if all column used for the tips are present in dataframe
    if (!is.na(tips_names)) {
      if (any(!tips_names %in% colnames(df))) {
        stop(paste(
          "Column name :",
          tips_names[!tips_names %in% colnames(df)],
          "not present in data"
        ), )
      }
    }

    nid2 <- plist$nid[lkeep, ]
    n2 <- plist$n[lkeep]
    pos2 <- plist$pos[lkeep, ]
    spouse2 <- plist$spouse[lkeep, ]
    fam2 <- plist$fam[lkeep, ]
    if (!is.null(plist$twins)) twin2 <- plist$twins[lkeep, ]

    for (i in seq_len(nrow(nid2))) {
      keep <- which(pos2[i, ] >= subreg[1] & pos2[i, ] <= subreg[2])
      nkeep <- length(keep)
      n2[i] <- nkeep
      nid2[i, 1:nkeep] <- nid2[i, keep]
      pos2[i, 1:nkeep] <- pos2[i, keep]
      spouse2[i, 1:nkeep] <- spouse2[i, keep]
      fam2[i, 1:nkeep] <- fam2[i, keep]
      if (!is.null(plist$twins)) twin2[i, 1:nkeep] <- twin2[i, keep]

      if (i < nrow(nid2)) { # look ahead
        tfam <- match(fam2[i + 1, ], keep, nomatch = 0)
        fam2[i + 1, ] <- tfam
        if (any(spouse2[i, tfam] == 0)) {
          stop("A subregion cannot separate parents")
        }
      }
    }

    n <- max(n2)
    out <- list(
      n = n2[1:n],
      nid = nid2[, 1:n, drop = FALSE],
      pos = pos2[, 1:n, drop = FALSE],
      spouse = spouse2[, 1:n, drop = FALSE],
      fam = fam2[, 1:n, drop = FALSE]
    )
    if (!is.null(plist$twins)) {
      out$twins <- twin2[, 1:n, drop = FALSE]
    }
    out
  } # end subregion2()

  if (length(ped) < 4) {
    if (is.na(ped)) {
      ped <- with(df, pedigree(id, dadid, momid, sex))
    } else {
      stop("Argument 'ped' given not recognized")
    }
  }

  ## Doc: Sizing
  plist <- align.pedigree(ped, packed = packed, width = width, align = align)
  if (!missing(subregion)) {
    plist <- subregion2(plist, subregion)
  }
  xrange <- range(plist$pos[plist$nid > 0])
  maxlev <- nrow(plist$pos)

  ## Plotting region
  frame()
  oldpar <- par(xpd = TRUE, ...) ## took out mar=mar
  psize <- par("pin") # plot region in inches
  stemp1 <- strwidth("ABC", units = "inches", cex = cex) * 2.5 / 3
  stemp2 <- strheight("1g", units = "inches", cex = cex)
  stemp3 <- max(strheight(id, units = "inches", cex = cex))

  ht1 <- psize[2] / maxlev - (stemp3 + 1.5 * stemp2)
  if (ht1 <= 0) {
    stop("Labels leave no room for the graph, reduce cex")
  }
  ht2 <- psize[2] / (maxlev + (maxlev - 1) / 2)
  wd2 <- .8 * psize[1] / (.8 + diff(xrange))

  boxsize <- symbolsize * min(ht1, ht2, stemp1, wd2) # box size in inches
  hscale <- (psize[1] - boxsize) / diff(xrange) # horizontal scale in inches
  vscale <- (psize[2] - (stemp3 + stemp2 / 2 + boxsize)) / max(1, maxlev - 1)
  boxw <- boxsize / hscale # box width in user units
  boxh <- boxsize / vscale # box height in user units
  labh <- stemp2 / vscale # height of a text string
  legh <- min(1 / 4, boxh * 1.5) # how tall are the 'legs' up from a child
  par_usr <- c(
    xrange[1] - boxw / 2, xrange[2] + boxw / 2,
    maxlev + boxh + stemp3 + stemp2 / 2, 1
  )

  # Test for presence of errors in par parameters
  if (any(is.na(par_usr))) {
    return(NULL)
  }
  par(usr = par_usr)
  ## Doc: end of sizing
  ## Doc: Sizing
  ## Doc:  subsection: drawbox

  ## Plotting function
  circfun <- function(nslice, n = 50) {
    nseg <- ceiling(n / nslice) # segments of arc per slice

    theta <- -pi / 2 - seq(0, 2 * pi, length = nslice + 1)
    out <- vector("list", nslice)
    for (i in 1:nslice) {
      theta2 <- seq(theta[i], theta[i + 1], length = nseg)
      out[[i]] <- list(
        x = c(0, cos(theta2) / 2),
        y = c(0, sin(theta2) / 2) + .5
      )
    }
    out
  } ## end circfun()

  ## Doc: polyfun
  polyfun <- function(nslice, object) {
    # make the indirect segments view
    zmat <- matrix(0, ncol = 4, nrow = length(object$x))
    zmat[, 1] <- object$x
    zmat[, 2] <- c(object$x[-1], object$x[1]) - object$x
    zmat[, 3] <- object$y
    zmat[, 4] <- c(object$y[-1], object$y[1]) - object$y

    # Find the cutpoint for each angle
    # Yes we could vectorize the loop, but nslice is never bigger than
    # about 10 (and usually <5), so why be obscure?
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
      x = c(x, object$x),
      y = c(y, object$y)
    )
    temp <- temp[order(-temp$theta), ]
    out <- vector("list", nslice)
    for (i in 1:nslice) {
      rows <- which(temp$indx == i):which(temp$indx == (i + 1))
      out[[i]] <- list(x = c(0, temp$x[rows]), y = c(0, temp$y[rows]) + .5)
    }
    out
  } ## end polyfun()

  if (ncol(affected) == 1) {
    polylist <- list(
      square = list(list(x = c(-1, -1, 1, 1) / 2, y = c(0, 1, 1, 0))),
      circle = list(list(
        x = .5 * cos(seq(0, 2 * pi, length = 50)),
        y = .5 * sin(seq(0, 2 * pi, length = 50)) + .5
      )),
      diamond = list(list(x = c(0, -.5, 0, .5), y = c(0, .5, 1, .5))),
      triangle = list(list(x = c(0, -.56, .56), y = c(0, 1, 1)))
    )
  } else {
    nc <- ncol(affected)
    square <- polyfun(nc, list(
      x = c(-.5, -.5, .5, .5), y = c(-.5, .5, .5, -.5),
      theta = -c(3, 5, 7, 9) * pi / 4
    ))
    circle <- circfun(nc)
    diamond <- polyfun(nc, list(
      x = c(0, -.5, 0, .5), y = c(-.5, 0, .5, 0),
      theta = -(1:4) * pi / 2
    ))
    triangle <- polyfun(nc, list(
      x = c(-.56, .0, .56), y = c(-.5, .5, -.5),
      theta = c(-2, -4, -6) * pi / 3
    ))
    polylist <- list(
      square = square, circle = circle, diamond = diamond,
      triangle = triangle
    )
  } ## else

  drawbox <- function(x, y, sex, affected, status, avail, mark, polylist,
                      fill, border,
                      density, angle, boxw, boxh, p_plot, id, data_tooltips) {
    for (i in seq_len(length(affected))) {
      plabel <- mark
      if (is.null(mark[i])) {
        plabel <- "?"
      }
      midx <- x + mean(range(polylist[[sex]][[i]]$x * boxw))
      midy <- y + mean(range(polylist[[sex]][[i]]$y * boxh))
      if (avail == 2) {
        borderSize <- c(boxw * 30, boxw * 10)
      } else {
        borderSize <- c(boxw * 5, boxw)
      }
      polygon(x + (polylist[[sex]])[[i]]$x * boxw,
        y + (polylist[[sex]])[[i]]$y * boxh,
        col = fill, border = border
      )

      col_text <- "black"
      text(midx, midy,
        labels = plabel,
        cex = cex / length(affected), col = col_text
      )

      if (ggplot_gen) {
        p_plot <- p_plot + ggplot2::geom_polygon(ggplot2::aes(
          x = x + (polylist[[sex]])[[i]]$x * boxw,
          y = y + (polylist[[sex]])[[i]]$y * boxh, fill = fill,
          color = border
        ))
        tips <- data_tooltips[data_tooltips$id == id, tips_names]
        tips <- tips[][c(!is.na(tips))]
        tips <- paste(colnames(tips), ":", tips, collapse = "<br>")

        p_plot <- p_plot + ggplot2::geom_text(ggplot2::aes(
          x = midx, y = midy, label = plabel,
          text = tips, color = col_text
        ))
      }
    }
    if (status == 1) {
      segments(
        x - .6 * boxw, y + 1.1 * boxh,
        x + .6 * boxw, y - .1 * boxh
      )
      if (ggplot_gen) {
        p_plot <- p_plot +
          ggplot2::annotate("segment",
            x = x - .6 * boxw, y = y + 1.1 * boxh,
            xend = x + .6 * boxw, yend = y - .1 * boxh
          )
      }
    }
    ## Do a black slash per Beth, old line was
    ## (x + .6*boxw, y - .1 * boxh, col = avail)
    return(p_plot)
  } ## drawbox
  legend_labels <- c(
    "Unaffected and unavailable",
    "Unaffected and available",
    "Affected and unavailable",
    "Affected and available"
  )
  colors_plot <- unique(c(fill, border))

  p <- ggplot2::ggplot() +
    ggplot2::scale_fill_manual(
      values = setNames(colors_plot, colors_plot), name = "Availability",
      labels = setNames(legend_labels, 1:4), drop = FALSE
    ) +
    ggplot2::scale_color_manual(values = setNames(colors_plot, colors_plot),
        guide = FALSE)

  ## Doc: symbols
  sex <- as.numeric(ped$sex)

  message("Drawing the individuals", appendLF = TRUE)
  prog_bar <- txtProgressBar(0, maxlev, char = "|", width = 50, style = 3)
  for (i in 1:maxlev) {
    setTxtProgressBar(prog_bar, i)
    for (j in seq_len(plist$n[i])) {
      k <- plist$nid[i, j]
      p <- drawbox(
        plist$pos[i, j], i, sex[k], affected[k, ],
        status[k], avail[k], mark[k], polylist,
        fill[k], border[k],
        density, angle, boxw, boxh, p, id[k], df
      )
      text(plist$pos[i, j], i + boxh + labh * .7, id[k],
        cex = cex,
        adj = c(.5, 1)
      )
      if (ggplot_gen) {
        p <- p +
          ggplot2::annotate("text",
            label = id[k],
            x = plist$pos[i, j], y = i + boxh + labh * .7
          )
      }
      if (!is.na(label[k])) { # Label added if present under the individual
        text(plist$pos[i, j],
          i + boxh + labh * .7 * 3,
          paste0("(", label[k], ")"),
          cex = cex * 0.75,
          adj = c(.5, 1)
        )
      }
    }
  }
  ## Doc: lines between spouses

  maxcol <- ncol(plist$nid) # all have the same size
  message("\nDrawing connection spouse", appendLF = TRUE)
  i <- 4
  for (i in 1:maxlev) {
    setTxtProgressBar(prog_bar, i)
    tempy <- i + boxh / 2
    if (any(plist$spouse[i, ] > 0)) {
      temp <- (1:maxcol)[plist$spouse[i, ] > 0]
      segments(
        x0 = plist$pos[i, temp] + boxw / 2,
        y0 = rep(tempy, length(temp)),
        x1 = plist$pos[i, temp + 1] - boxw / 2,
        y1 = rep(tempy, length(temp))
      )

      if (ggplot_gen) {
        for (i2 in seq_len(length(temp))) {
          p <- p +
            ggplot2::annotate("segment",
              x = plist$pos[i, temp][i2] + boxw / 2,
              y = rep(tempy, length(temp))[i2],
              xend = plist$pos[i, temp + 1][i2] - boxw / 2,
              yend = rep(tempy, length(temp))[i2]
            )
        }
      }
      temp <- (1:maxcol)[plist$spouse[i, ] == 2]
      if (length(temp)) { # double line for double marriage
        tempy <- tempy + boxh / 10
        segments(
          plist$pos[i, temp] + boxw / 2, rep(tempy, length(temp)),
          plist$pos[i, temp + 1] - boxw / 2, rep(tempy, length(temp))
        )
        if (ggplot_gen) {
          for (i2 in seq_len(length(temp))) {
            p <- p +
              ggplot2::annotate("segment",
                x = plist$pos[i, temp][i2] + boxw / 2,
                y = rep(tempy, length(temp))[i2],
                xend = plist$pos[i, temp + 1][i2] - boxw / 2,
                yend = rep(tempy, length(temp))[i2]
              )
          }
        }
      }
    }
  }
  ## Doc: Lines from offspring to parents

  message("\nDrawing connection childrens", appendLF = TRUE)
  for (i in 2:maxlev) {
    setTxtProgressBar(prog_bar, i)
    zed <- unique(plist$fam[i, ])
    zed <- zed[zed > 0] # list of family ids

    for (fam in zed) {
      xx <- plist$pos[i - 1, fam + 0:1]
      parentx <- mean(xx) # midpoint of parents

      # Draw the uplines
      who <- (plist$fam[i, ] == fam) # The kids of interest
      if (is.null(plist$twins)) {
        target <- plist$pos[i, who]
      } else {
        twin_to_left <- (c(0, plist$twins[i, who])[1:sum(who)])
        temp <- cumsum(twin_to_left == 0) # increment if no twin to the left
        # 5 sibs, middle 3 are triplets gives 1,2,2,2,3
        # twin, twin, singleton gives 1,1,2,2,3
        tcount <- table(temp)
        target <- rep(tapply(plist$pos[i, who], temp, mean), tcount)
      }
      yy <- rep(i, sum(who))
      segments(plist$pos[i, who], yy, target, yy - legh)
      if (ggplot_gen) {
        for (i2 in seq_len(length(yy))) {
          p <- p + ggplot2::annotate("segment",
            x = plist$pos[i, who][i2], y = yy[i2],
            xend = target[i2], yend = yy[i2] - legh
          )
        }
      }

      ## draw midpoint MZ twin line
      if (any(plist$twins[i, who] == 1)) {
        who2 <- which(plist$twins[i, who] == 1)
        temp1 <- (plist$pos[i, who][who2] + target[who2]) / 2
        temp2 <- (plist$pos[i, who][who2 + 1] + target[who2]) / 2
        yy <- rep(i, length(who2)) - legh / 2
        segments(temp1, yy, temp2, yy)
        if (ggplot_gen) {
          for (i2 in seq_len(length(yy))) {
            p <- p + ggplot2::annotate("segment",
              x = temp1[i2], y = yy[i2],
              xend = temp2[i2], yend = yy[i2]
            )
          }
        }
      }

      # Add a question mark for those of unknown zygosity
      if (any(plist$twins[i, who] == 3)) {
        who2 <- which(plist$twins[i, who] == 3)
        temp1 <- (plist$pos[i, who][who2] + target[who2]) / 2
        temp2 <- (plist$pos[i, who][who2 + 1] + target[who2]) / 2
        yy <- rep(i, length(who2)) - legh / 2
        text((temp1 + temp2) / 2, yy, "?")
        if (ggplot_gen) {
          for (i2 in seq_len(length(yy))) {
            p <- p +
              ggplot2::annotate("text",
                label = "?",
                x = (temp1[i2] + temp2[i2]) / 2,
                y = yy[i2]
              )
          }
        }
      }

      # Add the horizontal line
      segments(min(target), i - legh, max(target), i - legh)
      if (ggplot_gen) {
        p <- p + ggplot2::annotate("segment",
          x = min(target), y = i - legh,
          xend = max(target), yend = i - legh
        )
      }

      # Draw line to parents.  The original rule corresponded to
      #  pconnect a large number, forcing the bottom of each parent-child
      #  line to be at the center of the bar uniting the children.
      if (diff(range(target)) < 2 * pconnect) {
        x1 <- mean(range(target))
      } else {
        x1 <- pmax(min(target) + pconnect, pmin(
          max(target) - pconnect,
          parentx
        ))
      }
      y1 <- i - legh
      if (branch == 0) {
        segments(x1, y1, parentx, (i - 1) + boxh / 2)
        if (ggplot_gen) {
          p <- p + ggplot2::geom_segment(ggplot2::aes(
            x = x1, y = y1,
            xend = parentx, yend = (i - 1) + boxh / 2
          ))
        }
      } else {
        y2 <- (i - 1) + boxh / 2
        x2 <- parentx
        ydelta <- ((y2 - y1) * branch) / 2
        segments(
          c(x1, x1, x2), c(y1, y1 + ydelta, y2 - ydelta),
          c(x1, x2, x2), c(y1 + ydelta, y2 - ydelta, y2)
        )
        if (ggplot_gen) {
          for (i2 in seq_len(length(x1))) {
            p <- p + ggplot2::annotate("segment",
              x = x1[i2], y = y1[i2],
              xend = x1[i2], yend = y1[i2] + ydelta
            )
            p <- p + ggplot2::annotate("segment",
              x = x1[i2], y = y1[i2] + ydelta,
              xend = x2[i2], yend = y2[i2] - ydelta
            )
            p <- p + ggplot2::annotate("segment",
              x = x2[i2], y = y2[i2] - ydelta,
              xend = x2[i2], yend = y2[i2]
            )
          }
        }
      }
    }
  } ## end of parent-child lines

  ## Doc: 4 arcs for multiple instances of subj
  arcconnect <- function(x, y, p) {
    xx <- seq(x[1], x[2], length = 15)
    yy <- seq(y[1], y[2], length = 15) + (seq(-7, 7))^2 / 98 - .5
    lines(xx, yy, lty = 2)
    if (ggplot_gen) {
      p <- p + ggplot2::annotate("line", xx, yy, linetype = "dashed")
    }
    return(p)
  }

  uid <- unique(plist$nid)
  ## JPS 4/27/17: unique above only applies to rows
  ## unique added to for loop iterator
  imax <- length(unique(uid[uid > 0]))
  i <- 0
  if (imax > 0) {
    message("\nDrawing arcs for same Id", appendLF = TRUE)
    prog_bar <- txtProgressBar(0, imax - 1, width = 50, style = 3, char = "|")
  }
  for (id in unique(uid[uid > 0])) {
    setTxtProgressBar(prog_bar, i)
    indx <- which(plist$nid == id)
    if (length(indx) > 1) { # subject is a multiple
      tx <- plist$pos[indx]
      ty <- ((row(plist$pos))[indx])[order(tx)]
      tx <- sort(tx)
      for (j in 1:(length(indx) - 1)) {
        p <- arcconnect(tx[j + 0:1], ty[j + 0:1], p)
      }
    }
    i <- i + 1
  }
  close(prog_bar)

  ## Doc: finish/Final
  ckall <- paste(df$id[is.na(match(df$id, df$id[plist$nid]))],
    collapse = ", "
  )
  if (length(ckall > 0)) {
    message("Did not plot the following people: ", ckall, "\n")
  }

  if (!keep_par) {
    par(oldpar)
  }

  tmp <- match(seq_len(length(df$id)), plist$nid)
  if (ggplot_gen) {
    invisible(list(
      plist = plist, x = plist$pos[tmp], y = row(plist$pos)[tmp],
      boxw = boxw, boxh = boxh, call = call, ggplot = p, plot = recordPlot()
    ))
  } else {
    invisible(list(
      plist = plist, x = plist$pos[tmp], y = row(plist$pos)[tmp],
      boxw = boxw, boxh = boxh, call = call, plot = recordPlot()
    ))
  }
}
