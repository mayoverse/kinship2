#' @importFrom ggplot2 ggplot geom_polygon aes geom_text annotate ggtitle
#' @importFrom ggplot2 scale_fill_manual scale_color_manual geom_segment
#' @importFrom utils txtProgressBar setTxtProgressBar
#' @importFrom stats setNames
#' @importFrom graphics clip
NULL

#' Plot Pedigrees
#'
#' @description
#' This function is used to plot a Pedigree object.
#'
#' It is a wrapper for [plot_fromdf()] and [ped_to_plotdf()] as well as
#' [ped_to_legdf()] if `legend = TRUE`.
#'
#' @details
#' Two important parameters control the looks of the result.  One is the user
#' specified maximum width.  The smallest possible width is the maximum number
#' of subjects on a line, if the user's suggestion is too low it is
#' increased to 1 + that amount (to give just a little wiggle room).
#'
#' To make a Pedigree where all children are centered under parents simply
#' make the width large enough, however, the symbols may get very small.
#'
#' The second is `align`, a vector of 2 alignment parameters `a` and
#' `b`.
#' For each set of siblings at a set of locations `x` and with parents at
#' `p=c(p1,p2)` the alignment penalty is
#'
#' \deqn{(1/k^a)\sum{i=1}{k} [(x_i - (p1+p2)/2)]^2}
#'
#' \deqn{\sum(x- \overline(p))^2/(k^a)}
#'
#' Where k is the number of siblings in the set.
#'
#' When `a = 1` moving a sibship with `k` sibs one unit to the
#' left or right of optimal will incur the same cost as moving one with
#' only 1 or two sibs out of place.
#'
#' If `a = 0` then large sibships are harder to move than small ones,
#' with the default value `a = 1.5` they are slightly easier to move
#' than small ones.  The rationale for the default is as long as the parents
#' are somewhere between the first and last siblings the result looks fairly
#' good, so we are more flexible with the spacing of a large family.
#' By tethering all the sibs to a single spot they are kept close to each other.
#' The alignment penalty for spouses is \eqn{b(x_1 - x_2)^2}{b *(x1-x2)^2},
#' which tends to keep them together. The size of `b` controls the relative
#' importance of sib-parent and spouse-spouse closeness.
#'
#' @param x A Pedigree object.
#' @inheritParams ped_to_plotdf
#' @inheritParams plot_fromdf
#' @inheritParams set_plot_area
#' @inheritParams subregion
#' @inheritParams align
#' @param fam_to_plot default=1.  If the Pedigree contains multiple families,
#' this parameter can be used to select which family to plot.
#' It can be a numeric value or a character value. If numeric, it is the
#' index of the family to plot returned by `unique(x$ped$famid)`.
#' If character, it is the family id to plot.
#' @param legend default=FALSE.  If TRUE, a legend will be added to the plot.
#' @param leg_cex default=0.8.  Controls the size of the legend text.
#' @param leg_symbolsize default=0.5.  Controls the size of the legend symbols.
#' @param leg_loc default=NULL.  If NULL, the legend will be placed in the
#' upper right corner of the plot.  Otherwise, a 4-element vector of the form
#' (x0, x1, y0, y1) can be used to specify the location of the legend.
#' @param leg_adjx default=0.  Controls the horizontal labels adjustment of
#' the legend.
#' @param leg_adjy default=0.  Controls the vertical labels adjustment
#' of the legend.
#' @param ... Extra options that feed into the plot function.
#'
#' @return an invisible list containing
#' - df : the data.frame used to plot the Pedigree
#' - par_usr : the user coordinates used to plot the Pedigree
#' - ggplot : the ggplot object if ggplot_gen = TRUE
#'
#' @examples
#' data(sampleped)
#' pedAll <- Pedigree(sampleped)
#' #plot(pedAll)
#'
#' @section Side Effects: Creates plot on current plotting device.
#' @seealso [Pedigree()]
#' @include align.R
#' @include plot_fct.R
#' @include ped_to_plotdf.R
#' @include ped_to_legdf.R
#' @include plot_fromdf.R
#' @aliases plot.Pedigree
#' @aliases plot,Pedigree
#' @keywords Pedigree-plot
#' @export
#' @docType methods
setMethod("plot", c(x = "Pedigree", y = "missing"),
    function(x, aff_mark = TRUE,
        label = NULL, ggplot_gen = FALSE, cex = 1, symbolsize = 1, branch = 0.6,
        packed = TRUE, align = c(1.5, 2), width = 6,
        title = NULL, subreg = NULL, pconnect = 0.5, fam_to_plot = 1,
        legend = FALSE, leg_cex = 0.8, leg_symbolsize = 0.5,
        leg_loc = NULL, leg_adjx = 0, leg_adjy = 0, ...
    ) {
        famlist <- unique(famid(x))
        if (length(famlist) > 1) {
            message("Multiple families present, only plotting family ",
                fam_to_plot
            )
            if (is.numeric(fam_to_plot)) {
                fam_to_plot <- famlist[fam_to_plot]
            }
            x <- x[famid(x) == fam_to_plot]
        }

        lst <- ped_to_plotdf(x, packed, width, align, subreg,
            cex, symbolsize, pconnect, branch, aff_mark, label, ...
        )

        p <- plot_fromdf(lst$df, usr = lst$par_usr$usr,
            title = title, ggplot_gen = ggplot_gen,
            boxw = lst$par_usr$boxw, boxh = lst$par_usr$boxh
        )

        if (legend) {
            leg <- ped_to_legdf(x, cex = leg_cex,
                boxw = lst$par_usr$boxw * leg_symbolsize,
                boxh = lst$par_usr$boxh * leg_symbolsize,
                adjx = leg_adjx, adjy = leg_adjy
            )
            if (is.null(leg_loc)) {
                wh_fr <- lst$par_usr$usr
                leg_loc <- c(
                    wh_fr[1] + 1, wh_fr[2],
                    wh_fr[3] + 0.1, wh_fr[3] + 0.4
                )
            }
            leg$df$x0 <- scales::rescale(leg$df$x0,
                c(leg_loc[1], leg_loc[2])
            )
            leg$df$y0 <- scales::rescale(leg$df$y0,
                c(leg_loc[3], leg_loc[4])
            )
            clip(leg_loc[1] - 1, leg_loc[2] + 1, leg_loc[3] - 1, leg_loc[4] + 1)
            plot_fromdf(leg$df, add_to_existing = TRUE,
                boxw = lst$par_usr$boxw * leg_symbolsize,
                boxh = lst$par_usr$boxh * leg_symbolsize
            )
        }

        if (ggplot_gen) {
            invisible(list(df = lst$df, par_usr = lst$par_usr, ggplot = p))
        } else {
            invisible(list(df = lst$df, par_usr = lst$par_usr))
        }
    }
)
