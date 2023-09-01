#' @importFrom ggplot2 ggplot geom_polygon aes geom_text annotate ggtitle
#' @importFrom ggplot2 scale_fill_manual scale_color_manual geom_segment
#' @importFrom utils txtProgressBar setTxtProgressBar
#' @importFrom stats setNames
#' @importFrom graphics clip
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
#' @include ped_to_plotdf.R
#' @include ped_to_legdf.R
#' @include plot_fromdf.R
#' @export
setMethod("plot", c(x = "Pedigree", y = "missing"),
    function(x, mark = TRUE,
        label = NULL, tips_names = NULL, fill = "grey", border = "black",
        ggplot_gen = FALSE, cex = 1, symbolsize = 1, branch = 0.6,
        packed = TRUE, align = c(1.5, 2), width = 6,
        title = NULL, density = c(-1, 35, 65, 20),
        mar = c(4.1, 1, 4.1, 1), angle = c(90, 65, 40, 0),
        keep_par = FALSE, subregion = NULL, pconnect = 0.5, family = 1,
        legend = FALSE, leg_cex = 0.8, leg_symbolsize = 0.5,
        leg_loc = NULL, ...
    ) {
        lst <- ped_to_plotdf(x, packed, width, align, subregion,
            cex, symbolsize, pconnect, branch, mark, label, ...
        )
        famlist <- unique(x$ped$family)
        if (length(famlist) > 1) {
            message("Multiple families present, only plotting family",
                family
            )
            lst <- lst[[family]]
        }
        p <- plot_fromdf(lst$df, usr = lst$par_usr$usr,
            title = title, ggplot_gen = ggplot_gen,
            boxw = lst$par_usr$boxw, boxh = lst$par_usr$boxh
        )

        if (legend) {
            leg <- ped_to_legdf(x, cex = leg_cex,
                boxw = lst$par_usr$boxw * leg_symbolsize,
                boxh = lst$par_usr$boxh * leg_symbolsize
            )
            if (is.null(leg_loc)) {
                wh_fr <- lst$par_usr$usr
                leg_loc <- c(
                    wh_fr[1] + 1, wh_fr[2],
                    wh_fr[3] + 0.1, wh_fr[3] + 0.4
                )
            }
            leg$leg_df$x0 <- scales::rescale(leg$leg_df$x0,
                c(leg_loc[1], leg_loc[2])
            )
            leg$leg_df$y0 <- scales::rescale(leg$leg_df$y0,
                c(leg_loc[3], leg_loc[4])
            )
            clip(leg_loc[1] - 1, leg_loc[2] + 1, leg_loc[3] - 1, leg_loc[4] + 1)
            plot_fromdf(leg$leg_df, add_to_existing = TRUE,
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
TRUE
