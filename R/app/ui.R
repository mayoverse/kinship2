usethis::use_package("shiny")
usethis::use_package("bootstrap")

# Define UI for application that draws a histogram
shiny::shinyUI(shiny::fluidPage(
    ## Configuration -------------------------------
    shinyjs::useShinyjs(),
    tags$head(
        tags$style(HTML("hr {border-top: 1px solid #000000;}
                        .navigationBar{background-color:#0001;height:350px}
                        #console{max-height: 300px;overflow-y:auto;}
                        #legendToPlot{max-height:150px};")),
        tags$script(
        'Shiny.addCustomMessageHandler("scrollCallback",
            function(color) {
            var objDiv = document.getElementById("console");
            objDiv.scrollTop = objDiv.scrollHeight;
            }
        );
        var dimension = [0, 0];
        $(document).on("shiny:connected", function(e) {
            dimension[0] = window.innerWidth;
            dimension[1] = window.innerHeight;
            Shiny.onInputChange("dimension", dimension);
        });
        $(window).resize(function(e) {
            dimension[0] = window.innerWidth;
            dimension[1] = window.innerHeight;
            Shiny.onInputChange("dimension", dimension);
        });
        '
        )
    ),
    ## Application title --------------------------
    fluidRow(
        column(12, align = "center",
            titlePanel("Pedigree creation")
        )
    ),
    ## Navigation bar -----------------------------
    fluidRow(title = "Navigation", class = "navigationBar",
        ## ___Data and Family selection -------------
        column(2,
            data_import_ui(id = "dataImport")),
        column(2,
            data_col_sel_ui(id = "dataColSel")),
        column(2,
            uiOutput("family_sel")
        ),
    ),
    fluidRow(
        column(5, align = "center",
            uiOutput("families_var"),
            tableOutput("families_table"))
    ),

    ## Console ------------------------------------------------
    fluidRow(
        pre(id = "console")
    )

))
