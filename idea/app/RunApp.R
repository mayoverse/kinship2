##Pedigree App##
# This is the setup part of the Pedigree Shiny web application. You can run the
# application executing this script
print("Bal: RunApp")
## Charging the needed libraries --------------------------
packages <- c("shiny", "ggplot2", "kinship2", "stringr", "plyr", "plotly", "DT",
    "shinyjs", "shinyWidgets", "tidyr", "readxl", "cowplot", "gridExtra",
    "gridGraphics", "shinycssloaders", "LIM")
packageCheck <- lapply(packages, FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
        install.packages(x, dependencies = TRUE)
        library(x, character.only = TRUE)
    }
})
## Setting the working directory --------------------------
#Check for directory to use if wrong please change
#the working directory to PedigreeApp folder
this_file_dir <- function() {
    cmd_args <- commandArgs(trailingOnly = FALSE)
    needle <- "--file="
    match <- grep(needle, cmd_args)
    if (length(match) > 0) {
        # Rscript
        path_script <- normalizePath(sub(needle, "", cmd_args[match]))
    } else {
        # 'source' via R console
        path_script <- normalizePath(sys.frames()[[1]]$ofile)
    }
    return(sub("\\\\RunApp.R", "", path_script))
}

is_rstudio <- Sys.getenv("RSTUDIO") == "1"
if (is_rstudio) {
    message("Running from Rstudio detected")
    dir_script <- dirname(rstudioapi::getActiveDocumentContext()$path)
    launch_browser <- getOption("shiny.launch.browser", interactive())
} else {
    message("Running from R detected")
    dir_script <- this_file_dir()
    launch_browser <- TRUE
}
message(paste("The following directory will be used:", dir_script))
## Launching app with correct directory -----------------
runApp(dir_script, launch.browser = launch_browser)
TRUE
