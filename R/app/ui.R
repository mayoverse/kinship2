usethis::use_package("shiny")
usethis::use_package("bootstrap")

# Define UI for application that draws a histogram
shiny::shinyUI(shiny::fluidPage(
    ## Configuration -------------------------------
    shinyjs::useShinyjs(),
    tags$head(
        tags$style(HTML("hr {border-top: 1px solid #000000;}
                        .navigationBar{background-color:#0001;height:300px}
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
            data_import_ui(id = "data_ped_import")),
        column(6,
            column(6, data_col_sel_ui(id = "data_ped_col_sel1")),
            column(6, data_col_sel_ui(id = "data_ped_col_sel2"))
        ),
        column(2,
            data_import_ui(id = "data_rel_import")),
        column(2,
            data_col_sel_ui(id = "data_rel_col_sel"))
    ),
    hr(),
    ## Errors download ----------------------------
    fluidRow(title = "Errors download",
        column(6, align = "center",
            h5(strong("Pedigree data errors")),
            data_download_ui(id = "ped_errors")),
        column(6, align = "center",
            h5(strong("Relationship data errors")),
            data_download_ui(id = "rel_errors"))
    ),
    hr(),
    ## Family and Health selection ---------------------------
    fluidRow(title = "Family and Health Selection",
        column(4, align = "center",
            uiOutput("families_var_selector"),
            tableOutput("families_table"),
            uiOutput("family_selector")),
        column(4, align = "center",
            uiOutput("health_var_selector"),
            uiOutput("health_full_scale_box"),
            uiOutput("health_threshold_box")),
        column(4, align = "center",
            uiOutput("health_aff_sel"))
    ),

    ## Console ------------------------------------------------
    fluidRow(
        pre(id = "console")
    )

))
