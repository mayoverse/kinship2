print_console <- function(expr, session) {
  withCallingHandlers(
    results <- expr,
    message = function(m) {
      shinyjs::html("console", m$message, TRUE)
    },
    error = function(e) {
      shinyjs::html("console", e$message, TRUE)
    },
    warning = function(w) {
      shinyjs::html("console", w$message, TRUE)
    }
  )
  session$sendCustomMessage(type = "scrollCallback", 1)
  results
}
