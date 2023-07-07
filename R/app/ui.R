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
        align = "center",
        h3("Download errors"),
        column(6,
            h5(strong("Pedigree data errors")),
            data_download_ui(id = "ped_errors")),
        column(6, align = "center",
            h5(strong("Relationship data errors")),
            data_download_ui(id = "rel_errors"))
    ),
    hr(),
    ## Family and Health selection ---------------------------
    fluidRow(title = "Family and Health selection",
        column(4, align = "center",
            h3("Family selection"),
            uiOutput("families_var_selector"),
            tableOutput("families_table"),
            uiOutput("family_selector")),
        column(4, align = "center",
            h3("Health selection"),
            uiOutput("health_var_selector"),
            uiOutput("health_full_scale_box"),
            uiOutput("health_threshold_box"),
            uiOutput("health_aff_selector")),
        column(4, align = "center",
            h3("Family information"),
            textOutput("family_infos_title"),
            tableOutput("family_info_table"))
    ),
    hr(),
    ## Informative individuals selection ----------------------
    fluidRow(
        column(6, align = "center",
            h3("Informative individuals"),
            column(6,
                uiOutput("inf_var_selector"),
            ),
            column(6,
                uiOutput("inf_custvar_selector"),
                uiOutput("inf_custvar_textinput")
            )
        ),
        ## Filtering options ------------------------
        column(6, align = "center",
            h3("Filtering options"),
            column(6,
                numericInput("kin_max",
                    label = h5(strong("Max kinship")),
                    value = 4,
                    min = 1)
            ),
            column(6, align = "left",
                checkboxInput("trim_ped",
                    label = "Trim non informative parents",
                    value = TRUE),
                checkboxInput("keep_inf",
                    label = "When trimmed, keep individuals with infos",
                    value = TRUE),
                checkboxInput("interactivness",
                    label = "Make the pedigree interactive",
                    value = FALSE)
            )
        )
    ),
    hr(),
    ## Subfamily selection -------------------------
    fluidRow(
        column(6, align = "center",
            h3("Subfamily selection"),
            column(6,
                uiOutput("subfamilies_table")
            ),
            column(6,
                uiOutput("subfamily_selector")
            )
        )
    ),
    ## Console ------------------------------------------------
    fluidRow(
        pre(id = "console")
    )

))
