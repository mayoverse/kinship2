# This module was created during the St Jude Bio-Hackathon of May 2023
# by the team 13.
# author: Max Qiu (maxqiu@unl.edu)
# author: Louis Le Nézet (louislenezet@gmail.com)

#### Library needed #### ----------
usethis::use_package("shiny")
usethis::use_package("readxl")
usethis::use_package("shinyWidgets")

#### Function needed to work #### ----------
#' Read data
#'
#' @description Read dataframe based on the extension of the file
#'
#' @details This function detect the extension of the file and proceed to use
#' the according function to read it with the parameters given by the user.
#' @param file The file path
#' @param sep A string defining the separator to use for the file
#' @param quote A string defining the quote to use
#' @param header A boolean defining if the dataframe contain a header or not
#' @param df_name A string defining the name of the dataframe / sheet to use
#' @param stringsAsFactors A boolean defining if all the strings should be
#' interpreted ad factor
#' @param to_char A boolean defining if all the dataset should be read as
#' character.
#' @returns A dataframe.
#' @examples
#' \dontrun{
#'     read_data("path/to/my/file.txt", sep=",", header=FALSE)
#' }
#' @keywords data
#' @export read_data
read_data <- function(file, sep = ";", quote = "'", header = TRUE, df_name = NA,
                stringsAsFactors = FALSE, to_char = TRUE) {
    print("Bal: read_data")
    shiny::req(file)
    if (!is.null(file)) {
        ext <- tools::file_ext(file)
        shiny::validate(shiny::need(
            ext %in% c("csv", "txt", "xls", "xlsx", "rda", "tab"),
            "Please upload a (csv, txt, xls, xlsx, rda, tab) file"
        ))
        if (to_char) {
            col_classes <- "character"
            col_types <- "text"
        } else {
            col_classes <- NA
            col_types <- NULL
        }
        if (ext %in% c("csv", "txt")) {
            df <- utils::read.csv(file,
                sep = sep,
                quote = quote, header = header,
                colClasses = col_classes
            )
        } else if (ext %in% c("tab")) {
            df <- utils::read.table(file, quote = quote,
                header = header, sep = sep, fill = TRUE,
                colClasses = col_classes)
        } else if (ext %in% c("xls", "xlsx")) {
            sheets_present <- readxl::excel_sheets(file)
            if (is.null(df_name)) {
              message("Needs the name of the sheet to use 'df_name'")
              df <- NULL
            } else {
              if (df_name %in% sheets_present) {
                df <- as.data.frame(readxl::read_excel(file,
                                                       sheet = df_name,
                                                       col_names = header,
                                                       col_types = col_types
                ))
              } else {
                message("Error: Sheet selected isn't in file")
                df <- NULL
              }
            }
        } else if (ext %in% c("rda")) {
            shiny::req(df_name)
            all_data <- base::load(file)
            if (is.na(df_name)) {
              message("Needs the name of the dataframe to use 'df_name'")
              df <- NULL
            } else {
              if (df_name %in% all_data) {
                  df <- get(df_name)
              } else {
                  message("Error: dataframe selected isn't in file")
                  df <- NULL
              }
            }
        }
        as.data.frame(unclass(df),
            stringsAsFactors = stringsAsFactors
        )
    } else {
        message("Error: data selected is null")
        NULL
    }
}

#' Get dataframe name
#'
#' @description Extract the name of the different dataframe present in a file
#'
#' @details This function detect the extension of the file and extract if
#' necessary the different dataframe / sheet names available.
#' @param file The file path
#' @returns A vector of all the dataframe name present.
#' @examples
#' \dontrun{
#'     get_dataframe("path/to/my/file.txt")
#' }
#' @keywords dataframe
#' @export get_dataframe
get_dataframe <- function(file) {
    print("Bal: get_dataframe")
    shiny::req(file)
    ext <- tools::file_ext(file)
    if (ext %in% c("xls", "xlsx")) {
        sheets_present <- readxl::excel_sheets(file)
        if (!is.null(sheets_present)) {
            sheets_present
        } else {
            message("No sheets find in file")
            NULL
        }
    } else if (ext == "rda") {
        base::load(file)
    } else {
        message("File not an xls, xlsx nor rda")
        NULL
    }
}

#### UI function of the module #### ----------
#' Data import ui
#'
#' @description R Shiny module UI to import data files
#'
#' @details This module allow to import multiple type of data.
#' The file type currently supported are csv, txt, xls, xslx, rda.
#' The UI ask the user for the file localisation, the separator,
#' the needs to format to character, the quote format, the presence
#' of heading, the conversion of string to factor, and the dataframe
#' selection if multiple dataframe are present in one file (xlsx, rda).
#'
#' @param id A string.
#' @param label A string use to prompt the user
#' @returns A Shiny UI.
#' @examples
#' \dontrun{
#'     data_import_demo()
#' }
#' @keywords data
#' @export data_import_ui
data_import_ui <- function(id) {
    ns <- shiny::NS(id)
    shiny::tagList(
        shiny::uiOutput(ns("file")),
        shiny::actionButton(ns("options"), "Options", style = "simple",
            size = "sm", color = "warning"),
        shiny::selectInput(ns("sep"), "Separator", c(
            "Comma" = ",",
            "Semi-colon" = ";",
            "Tabulation" = "\t",
            "Space" = " "
        ), selected = "\t"),
        shiny::uiOutput(ns("dfSelection"))
    )
}

#### Server function of the module #### ----------
#' Data import server
#'
#' @description R Shiny module server to import data files
#'
#' @details This module allow to import multiple type of data.
#' The file type currently supported are csv, txt, xls, xslx, rda.
#' The server dynamically create a selection input if multiple
#' dataframe are present in the file selected.
#'
#' @param id A string.
#' @returns A Shiny server.
#' @examples
#' \dontrun{
#'     data_import_demo()
#' }
#' @keywords data
#' @export data_import_server
data_import_server <- function(id,
    label = "Select data file",
    max_request_size = 30) {
    options(shiny.maxRequestSize = max_request_size * 1024^2)
    shiny::moduleServer(id, function(input, output, session) {
        print("Bal: data_import_server")
        ## File rendering selection ------------------------
        output$file <- shiny::renderUI({
            shiny::fileInput(ns("file"), label)
        })

        # The selected file, if any
        user_file <- shiny::reactive({
            # If no file is selected, don't do anything
            shiny::validate(shiny::need(input$file, message = FALSE))
            input$file
        })

        ## Options rendering selection --------------------
        opt <- shiny::reactiveValues(
            heading = TRUE,
            to_char = FALSE,
            stringsAsFactors = FALSE,
            quote = "\""
        )
        shiny::observeEvent(input$options, {
            # display a modal dialog with a header, textinput and action buttons
            shiny::showModal(shiny::modalDialog(
                shiny::tags$h2("Select your options"),
                shiny::checkboxInput(ns("heading"), "Has heading",
                    value = opt$heading),
                shiny::checkboxInput(ns("to_char"), "Load all data as strings",
                    value = opt$to_char),
                shiny::checkboxInput(ns("stringsAsFactors"),
                    "Strings as factors", value = opt$stringsAsFactors),
                shinyWidgets::pickerInput(ns("quote"), "Quote", c(
                    "None" = "",
                    "Double quote" = "\"",
                    "Single quote" = "'",
                    "Both" = "\"'"
                ), selected = opt$quote, multiple = FALSE),
                footer = shiny::tagList(
                    shiny::actionButton(ns("close"), "Close"),
                )
            ))
        })

        # Store the information if the user clicks close
        shiny::observeEvent(input$close, {
            shiny::removeModal()
            opt$heading <- input$heading
            opt$to_char <- input$to_char
            opt$stringsAsFactors <- input$stringsAsFactors
            opt$quote <- input$quote
        })

        ## Data selection ------------------------
        df <- shiny::reactive({
            file <- user_file()$datapath
            read_data(file, input$sep, opt$quote,
                      opt$heading, input$dfSelected,
                      opt$stringsAsFactors, opt$to_char)
        })

        # We can run observers in here if we want to
        shiny::observe({
            msg <- sprintf("File %s was uploaded", user_file()$name)
            message(msg, "\n")
        })

        ns <- shiny::NS(id)

        output$dfSelection <- shiny::renderUI({
            file <- user_file()$datapath
            df_name <- get_dataframe(file)
            if (!is.null(df_name)) {
                shiny::selectInput(ns("dfSelected"),
                    label = label,
                    choices = df_name, selected = df_name[1]
                )
            } else {
                NULL
            }
        })

        # Return the reactive that yields the data frame
        return(df)
    })
}