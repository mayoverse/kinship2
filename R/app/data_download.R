usethis::use_package("shiny")

data_download_ui <- function(id) {
    ns <- shiny::NS(id)
    shiny::tagList(
        shiny::uiOutput(ns("data_text")),
        shiny::uiOutput(ns("btn_dwld"))
    )
}

data_download_server <- function(id, df, filename, label = NULL, helper = TRUE) {
    stopifnot(shiny::is.reactive(df))
    ns <- shiny::NS(id)
    shiny::moduleServer(id, function(input, output, session) {
        output$data_dwld <- shiny::downloadHandler(filename = function() {
                paste(filename, ".csv", sep = "")
            },
                content = function(file) {
                write.csv2(df(), file)
            })
        shiny::observeEvent(df(), {
            if (nrow(df()) == 0) {
                output$data_text <- shiny::renderUI({
                    shiny::HTML(paste(label, "doesn't have any rows"))
                })
                output$btn_dwld <- shiny::renderUI({
                    NULL
                })
            } else {
                if (helper) {
                    output$data_text <- shiny::renderUI({
                        shiny::HTML(paste(label, "with", nrow(df()), "rows",
                        "<br/>", "(Data can only be export to Download folder)"))
                    })
                } else {
                    output$data_text <- shiny::renderUI({
                        NULL
                    })
                }

                output$btn_dwld <- shiny::renderUI({
                    shiny::downloadButton(ns("data_dwld"), label = label)
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
            "mtcars_data_file", "mtcars")
    }
    shiny::shinyApp(ui, server)
}

data_download_demo()
