usethis::use_package("shiny")
usethis::use_package("shinyjs")
usethis::use_package("dplyr")
shiny::shinyServer(function(input, output, session) {
    ## Data import -------------------------
    df <- data_import_server(id = "dataImport")
    cols_needed <- data_col_sel_server("dataColSel", df,
        c("indId", "fatherId", "motherId", "gender", "steril", "available"),
        "Select columns :", null = TRUE)
    ## Data management ---------------------
    df_list <- shiny::reactive({
        cols_ren <- cols_needed()[cols_needed() != ""]
        cols_needed <- c("indId", "fatherId", "motherId", "gender")
        if (any(!cols_needed %in% names(cols_ren))) {
            NULL
        } else {
            df_rename <- df()
            data.table::setnames(df_rename,
                old = as.vector(unlist(cols_ren)),
                new = names(cols_ren))
            df_list <- print_console(check_data(df_rename), session)
            if (length(df_list[[2]]) > 0) {
                showNotification(paste(nrow(df_list[[2]]),
                    "errors as occured.",
                    "All individuals with errors wil be discarded"))
            }
            df_list
        }
    })

    ## Rendered UI ---------------------------
    ## Family selection variable
    output$families_var <- renderUI({
        print(df_list())
        dfn <- df_list()
        if (!is.null(dfn)) {
            col_no <- c("id", "dadid", "momid")
            col_all <- colnames(dfn)
            col_av <- setdiff(col_all, col_no)
            col_sel <- c()
            for (col in col_av) {
                if (any(!is.na(dfn[[col]]))) {
                    col_sel <- c(col_sel, col)
                }
            }
            selectInput("families_sel",
                label = h5("Select Variable to use as families indicator"),
                choices = as.list(setNames(col_sel, col_sel)),
                selected = col_sel[1]
            )
        } else {
            NULL
        }
    })

    ## End --------------------------------
    if (!interactive()) {
        session$onSessionEnded(function() {
        shiny::stopApp()
        q("no")
        })
    }
})
