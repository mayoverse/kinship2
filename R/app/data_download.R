usethis::use_package("shiny")

data_download_ui <- function(id) {
    ns <- shiny::NS(id)
    shiny::tagList(
        shiny::uiOutput(ns("data_text")),
        shiny::uiOutput(ns("download_data"))
    )
}

data_download_server <- function(id, df, label) {
    stopifnot(shiny::is.reactive(df))
    ns <- shiny::NS(id)
    shiny::moduleServer(id, function(input, output, session) {
        shiny::observeEvent(df(), {
            if (nrow(df()) == 0) {
                output$data_text <- shiny::renderUI({
                    shiny::HTML(paste(label, "doesn't have any rows"))
                })
                output$download_data <- shiny::renderUI({
                    NULL
                })
            } else {
                output$data_text <- shiny::renderUI({
                    shiny::HTML(paste(label, "with", nrow(df()), "rows",
                    "<br/>", "(Data can only be export to Download folder)"))
                })
                output$download_data <- shiny::renderUI({
                    output$data_downloader <- shiny::downloadHandler(filename = function() {
                        paste(label, Sys.Date(), ".csv", sep = "")
                    },
                        content = function(file) {
                        write.csv2(df(), file)
                    })
                    shiny::downloadButton(ns("data_downloader"),
                        paste("Download", label, "as .csv"))
                })
            }
        })
    })
}

data_download_demo <- function() {
    ui <- shiny::fluidPage(
        data_download_ui("datafile")
    )
    server <- function(input, output, session) {
        data_download_server("datafile",
            shiny::reactive({mtcars}),
            "mtcars")
    }
    shiny::shinyApp(ui, server)
}