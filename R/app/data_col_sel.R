usethis::use_package("shiny")

data_col_sel_ui <- function(id) {
    ns <- shiny::NS(id)
    shiny::tagList(
        shiny::uiOutput(ns("all_cols_sel"))
    )
}

data_col_sel_server <- function(id, df, cols_used, title, null = FALSE) {
    stopifnot(shiny::is.reactive(df))
    ns <- shiny::NS(id)
    shiny::moduleServer(id, function(input, output, session) {
        print("Bal: data_col_sel_server")
        cols_list <- shiny::reactive({
            all_cols <- colnames(df())
            if (null) {
                all_cols <- c("", all_cols)
            }
            setNames(all_cols, all_cols)
        })

        v <- list()
        all_sel <- shiny::reactive({
            for (col in cols_used){
                v[[col]] <- shiny::selectInput(ns(paste0("select_", col)),
                    label = shiny::h5(paste(title, col)),
                    choices = cols_list())
            }
            v
        })

        output$all_cols_sel <- shiny::renderUI({
            all_sel()
        })

        r <- list()
        col_select_list <- shiny::reactive({
            for (col in cols_used){
                input_select_cols <- input[[paste0("select_", col)]]
                r[[col]] <- input_select_cols
            }
            r
        })
        return(col_select_list)
    })
}

data_col_sel_demo <- function() {
    ui <- shiny::fluidPage(
        data_col_sel_ui("datafile"),
        shiny::tableOutput("selected_cols")
    )
    server <- function(input, output, session) {
        col_sel <- data_col_sel_server("datafile",
            shiny::reactive({mtcars}),
            c("test", "id"),
            "Select columns")
        output$selected_cols <- shiny::renderTable({
            as.data.frame(col_sel())
        })
    }
    shiny::shinyApp(ui, server)
}
