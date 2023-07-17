# This module was created during the St Jude Bio-Hackathon of May 2023 by the team 13.
# Author: Louis Le Nézet (louislenezet@gmail.com)

# Documentation
#' R Shiny module to generate pedigree graph
#'
#' @param id A string.
#' @param df A dataframe containing the information about the individual to plot
#' @returns A Shiny module.
#' @examples
#' Pedigree_demo()
#### Library needed #### ----------
usethis::use_package("dplyr")
usethis::use_package("shiny")

#### Function to plot pedigree #### ----------
ped_plot <- function(df, cex_plot = 1, mar = rep(0.5, 4), psize = par("pin"),
  tips_names = NULL, to_plotly = FALSE, mark = df$affected, fill = df$fill,
  border = df$border, label = NA, title = NULL) {
  ped_plot <- plot.pedigree(
    df, symbolsize = 1, cex = cex_plot,
    mark = mark, label = label,
    mar = mar, psize = psize,
    tips_names = tips_names,
    fill = fill, border = border,
    ggplot_gen = to_plotly, title = title
  ) # General Pedigree
  print("Bal: ped_plot, Plot Done")
  if (to_plotly) {
    ggp <- ped_plot$ggplot + ggplot2::scale_y_reverse() +
       ggplot2::theme(
        panel.grid.major =  ggplot2::element_blank(),
        panel.grid.minor =  ggplot2::element_blank(),
        axis.title.x =  ggplot2::element_blank(),
        axis.text.x =  ggplot2::element_blank(),
        axis.ticks.x =  ggplot2::element_blank(),
        axis.ticks.y =  ggplot2::element_blank(),
        axis.title.y =  ggplot2::element_blank(),
        axis.text.y =  ggplot2::element_blank()
      )
    ## To make it interactive
    print("Bal: ped_plot, Converting to plotly")
    plotly::ggplotly(
        ggp +
        ggplot2::theme(legend.position = "none"),
      tooltip = "text")
  } else {
    ped_plot$plot
  }
}


#### UI function of the module #### ----------
plot_ped_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("plot")),
    checkboxInput(ns("interactive"),
      label = "Make the pedigree interactive",
      value = FALSE)
  )
}

#### Server function of the module #### ----------
plot_ped_server <- function(id, ped, title) {
    stopifnot(shiny::is.reactive(ped))
    moduleServer(id, function(input, output, session) {
        print("Bal: plot_ped_server")
        ns <- NS(id)
        plot_ped <- reactive({
            ped_plot(ped(), to_plotly = input$interactive, title = title)
        })
        output$plot <- renderUI({
            if (input$interactive) {
                print("Bal: plot_ped_server, interactive")
                output$ped_plotly <- plotly::renderPlotly({
                  if (!"plotly" %in% class(plot_ped())){
                    return(NULL)
                  }
                    plot_ped()
                })
                plotly::plotlyOutput(ns("ped_plotly"))
            } else {
                print("Bal: plot_ped_server, not interactive")
                output$ped_plot <- renderPlot({
                    if (!"grob" %in% class(plot_ped())) {
                      return(NULL)
                    }
                    gridExtra::grid.arrange(plot_ped())
                })
                plotOutput(ns("ped_plot"))
            }
        })
        return(plot_ped)
    })
}

#### Demo function of the module #### ----------
plot_ped_demo <- function() {
  data(sample.ped)
  colnames(sample.ped) <- c("family", "id", "dadid",
    "momid", "sex", "affected", "avail")
  df <- sample.ped[sample.ped$family == 1, ]
  df <- generate_aff_inds(df, "affected", threshold = 0, sup_thres_aff = TRUE)
  df <- generate_colors(df, "affected")$df
  ui <- fluidPage(
    plot_ped_ui("ped"),
    plot_download_ui("saveped")
  )
  server <- function(input, output, session) {
    plot_ped <- plot_ped_server("ped", reactive({
      df
    }))
    plot_download_server("saveped", plot_ped)
  }
  shinyApp(ui, server)
}
