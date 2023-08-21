usethis::use_package("shiny")
usethis::use_package("shinyjs")
usethis::use_package("shinyWidgets")
usethis::use_package("dplyr")
usethis::use_package("DT")
usethis::use_package("gridExtra")

shiny::shinyServer(function(input, output, session) {
    ## Ped data import -------------------------
    ped_df <- data_import_server(id = "data_ped_import",
        label = "Select pedigree file :")
    cols_needed_ped1 <- data_col_sel_server("data_ped_col_sel1", ped_df,
        c("indId", "fatherId", "motherId", "gender"),
        "Select columns :", null = TRUE)
    cols_needed_ped2 <- data_col_sel_server("data_ped_col_sel2", ped_df,
        c("family", "steril", "available", "status"),
        "Select columns :", null = TRUE)
    ## Rel data import -------------------------
    rel_df <- data_import_server(id = "data_rel_import",
        label = "Select relationship file :")
    cols_needed_rel <- data_col_sel_server("data_rel_col_sel", rel_df,
        c("id1", "id2", "code"),
        "Select columns :", null = TRUE)

    ## Data normalisation ----------------------
    ped_df_norm <- shiny::reactive({
        print("Bal: data management")
        cols_ren <- c(cols_needed_ped1()[cols_needed_ped1() != "NA"],
            cols_needed_ped2()[cols_needed_ped2() != "NA"])
        cols_needed_ped <- c("indId", "fatherId", "motherId", "gender")
        if (any(!cols_needed_ped %in% names(cols_ren)) |
            any(!cols_ren %in% colnames(ped_df()))) {
            NULL
        } else {
            if (any(duplicated(as.vector(unlist(cols_ren))))){
                showNotification("You have selected twice the same column !")
                NULL
            } else {
                df_rename <- data.table::copy(ped_df())
                data.table::setnames(df_rename,
                    old = as.vector(unlist(cols_ren)),
                    new = names(cols_ren))
                ped_df_norm <- print_console(norm_ped(df_rename), session)
                if (length(ped_df_norm[!is.na(ped_df_norm$error), ]) > 0) {
                    showNotification(paste(
                        nrow(ped_df_norm[!is.na(ped_df_norm$error), ]),
                        "errors as occured.",
                        "All individuals with errors wil be discarded"))
                }
                ped_df_norm
            }
        }
    })
    rel_df_norm <- shiny::reactive({
        print("Bal: rel management")
        cols_ren <- cols_needed_rel()[cols_needed_rel() != "NA"]
        cols_needed_rel <- c("id1", "id2", "code")
        if (any(!cols_needed_rel %in% names(cols_ren))) {
            NULL
        } else {
            rel_rename <- rel_df()
            data.table::setnames(rel_rename,
                old = as.vector(unlist(cols_ren)),
                new = names(cols_ren))
            rel_df_norm <- print_console(norm_Red(rel_rename), session)
            if (length(rel_df_norm[!is.na(rel_df_norm$error)]) > 0) {
                showNotification(paste(
                    nrow(rel_df_norm[!is.na(rel_df_norm$error), ]),
                    "errors as occured.",
                    "All individuals with errors wil be discarded"))
            }
            rel_df_norm
        }
    })

    ## Errors download -------------------------
    shiny::observeEvent(ped_df_norm(), {
        data_download_server("ped_errors",
            shiny::reactive({ped_df_norm()[!is.na(ped_df_norm()$error), ]}),
            "Pedigree data errors")
    })
    shiny::observeEvent(rel_df_norm(), {
        data_download_server("rel_errors",
            shiny::reactive({rel_df_norm()[!is.na(rel_df_norm()$error), ]}),
            "Relationship data errors")
    })

    ## Families selection and information ------
    # Families table
    families_table <- reactive({
        print("Bal: families_table")
        ped_df <- ped_df_norm()[is.na(ped_df_norm()$error)]
        if (!is.null(ped_df) & !is.null(input$families_var_sel)) {
            get_families_table(ped_df, input$families_var_sel)
        } else {
            NULL
        }
    })
    # Family selection variable
    output$families_var_selector <- renderUI({
        print("Bal: families_var")
        dfn <- ped_df_norm()[is.na(ped_df_norm()$error)]
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
            selectInput("families_var_sel",
                label = h5("Select Variable to use as families indicator"),
                choices = as.list(setNames(col_sel, col_sel)),
                selected = col_sel[1]
            )
        } else {
            NULL
        }
    })

    # Families table rendering
    output$families_table <- DT::renderDataTable({
        families_table()
    }, options = list(paging = FALSE, scrollX = TRUE,
        scrollY = "200px", scrollCollapse = TRUE),
    rownames = FALSE)
    # Family selection
    output$family_selector <- renderUI({
        print("Bal: family_sel")
        if (!is.null(families_table())) {
            fam_nb <- as.numeric(families_table()$FamilyNum)
            if (max(fam_nb) > 0) {
                numericInput("family_sel",
                    label = h5(strong("Select family to use")),
                    value = 1, min = min(fam_nb), max = max(fam_nb))
            } else {
                textOutput("No family present (only unconnected individuals)")
            }
        } else {
            NULL
        }
    })

    # Select family df
    ped_df_fam <- reactive({
        shiny::req(input$family_sel)
        shiny::req(ped_df_norm())
        print("Bal: ped_df_fam")
        df <- ped_df_norm()[is.na(ped_df_norm()$error)]
        if (!is.null(df) & !is.null(input$family_sel) & input$family_sel > 0) {
            df[df$family == input$family_sel, ]
        } else {
            NULL
        }
    })

    # Family information
    output$family_info_table <- renderTable({
        shiny::req(ped_df_aff())
        print("Bal: family_info_table")
        df <- ped_df_aff()$df
        if (!is.null(df)) {
            base::table(df$avail, df$mods_aff,
                useNA = "ifany",
                dnn = c("Availability", input$health_var_sel)) %>%
                as.data.frame() %>%
                tidyr::spread(Availability, Freq)
        } else {
            NULL
        }
    })
    output$family_infos_title <- renderText({
        print("Bal: family_infos_title")
        if (!is.null(families_table())) {
            paste("Health & Availability data representation for family",
                input$family_sel)
        } else {
            NULL
        }
    })

    ## Health selection ------------------------
    output$health_var_selector <- renderUI({
        if (!is.null(ped_df_fam())) {
            cols_all <- colnames(ped_df_fam())
            selectInput("health_var_sel",
                label = h5("Select Variable to use as health indicator"),
                choices = as.list(setNames(cols_all, cols_all)))
        } else {
            NULL
        }
    })
    output$health_full_scale_box <- renderUI({
        if (!is.null(input$health_var_sel)) {
        checkboxInput("health_full_scale", label = "Full scale color",
                        value = FALSE)
        } else {
            NULL
        }
    })
    output$health_threshold_box <- renderUI({
        if (!is.null(input$health_var_sel)) {
            if (is.numeric(ped_df_fam()[[input$health_var_sel]])) {
                checkboxInput("health_threshold_sup",
                    label = "Affected are strickly superior to threshold",
                    value = TRUE)
            } else {
                NULL
            }
        } else {
            NULL
        }
    })
    output$health_aff_selector <- renderUI({
        shiny::req(input$health_var_sel)
        if (!is.null(input$health_var_sel)) {
            health_df <- ped_df_fam()
            health_df <- health_df[, input$health_var_sel]
            if (length(health_df) != 0) {
                if (is.numeric(health_df)) {
                    min_h <- min(health_df, na.rm = TRUE)
                    max_h <- max(health_df, na.rm = TRUE)
                    if (any(is.na(c(min_h, max_h))) |
                        any(is.infinite(c(min_h, max_h)))) {
                        h5(paste("No value found for", input$health_var_sel))
                    } else {
                        sliderInput("health_threshold_val",
                            label = h5(paste("Threshold of",
                                input$health_var_sel,
                                "to determine affected individuals")),
                            sep = "'",
                            min = min_h,
                            max = max_h,
                            value = (max_h + min_h) / 2)
                    }
                } else {
                    health_var_lev <- levels(as.factor(health_df))
                    if (length(health_var_lev) == 0) {
                        h5(paste("No value found for", input$health_var_sel))
                    }
                    var_to_use <- as.list(setNames(health_var_lev,
                        health_var_lev))
                    shinyWidgets::pickerInput("health_aff_mods",
                        label = "Selection of affected modalities",
                        choices = var_to_use,
                        options = list(`actions-box` = TRUE),
                        multiple = TRUE, selected = health_var_lev)
                }
            } else {
                NULL
            }
        } else {
            NULL
        }
    })

    ## Health affected generation --------------
    ped_df_aff <- shiny::reactive({
        shiny::req(ped_df_fam())
        print("Bal: ped_df_aff")
        if (!is.null(ped_df_fam())) {
            tryCatch({
                ped_df_aff <- print_console(generate_aff_inds(ped_df_fam(),
                    col_aff = input$health_var_sel,
                    mods_aff = input$health_aff_mods,
                    threshold = input$health_threshold_val,
                    sup_thres_aff = input$health_threshold_sup),
                    session)
                generate_colors(ped_df_aff, "affected",
                    keep_full_scale = input$health_full_scale)
            },
            error = function(e) {
                NULL
            })
        } else {
            NULL
        }
    })
    ## Informative individuals selection -------
    #Informative individuals custom selection
    output$inf_var_selector <- renderUI({
        if (!is.null(ped_df_aff())) {
            selectInput("inf_selected",
                    label = h5(strong("Select informative individuals")),
                    choices = list(
                        "All individuals" = "All",
                        "Available or Affected" = "AvOrAf",
                        "Available only" = "Av",
                        "Affected only" = "Af",
                        "Available and Affected" = "AvAf",
                        "Custom" = "Cust"),
                    selected = "All")
        } else {
            NULL
        }
    })
    output$inf_custvar_selector <- renderUI({
        shiny::req(input$inf_selected == "Cust")
        df <- ped_df_aff()$df
        if (input$inf_selected == "Cust" & !is.null(df)) {
            col_present <- colnames(df)
            selectInput("inf_custvar_sel",
                label = "Select Variable to use to select informative individuals",
                choices = as.list(setNames(col_present, col_present)))
        } else {
            NULL
        }
    })
    output$inf_custvar_textinput <- renderUI({
        shiny::req(input$inf_selected == "Cust")
        if (input$inf_selected == "Cust") {
            textAreaInput("inf_custvar_val", label = h5("Custom selection"),
                placeholder = "Please enter individuals values separate by a comma")
        } else {
            NULL
        }
    })
    #Informative individuals selection
    inf_inds_selected <- reactive({
        shiny::req(ped_df_aff())
        print("Bal: inf_inds_selected")
        shiny::req(input$inf_selected)
        if (input$inf_selected != "Cust") {
            return(input$inf_selected)
        }
        print("Bal: inf_inds_selected custom")
        shiny::req(input$inf_custvar_val)
        shiny::req(input$inf_custvar_sel)
        if (!identical(input$inf_custvar_val, "")) {
            inf_custvar_sel <- input$inf_custvar_sel
            inf_custvar_val <- unlist(strsplit(input$inf_custvar_val, ","))

            df <- ped_df_aff()$df
            index <- which(df[, inf_custvar_sel] %in% inf_custvar_val)
            if (any(is.na(index))) {
                showNotification(paste("Values", inf_custvar_val[is.na(index)],
                    "not present in", inf_custvar_sel))
                NULL
            } else {
                df$id[index[!is.na(index)]]
            }
        } else {
            showNotification("Custom option selected but no individual id given")
            NULL
        }
    })
    inf_inds_sel_txt <- function() {
        isolate({
            if (input$inf_selected == "Cust") {
                paste(input$InfCustVariable, "(id ",
                    paste(inf_inds_selected(), collapse = ","),
                    ")")
            } else {
                inf_inds_selected()
            }
        })
    }
    ped_df_inf <- reactive({
        shiny::req(inf_inds_selected())
        shiny::req(input$trim_ped)
        shiny::req(input$keep_infos)
        shiny::req(ped_df_aff())
        print("Bal: ped_df_inf")
        df <- ped_df_aff()$df
        inf_inds <- inf_inds_selected()

        if (!is.null(df) & !any(is.na(inf_inds))) {
            tryCatch({
                df_from_inf <- select_from_inf(df,
                    inf_inds = inf_inds, kin_max = input$kin_max)
                df_from_inf <- num_child(df_from_inf,
                        relation = rel_df_norm()$norm)
                if (input$trim_ped) {
                    df_from_inf$useful <- useful_inds(df_from_inf, inf_inds,
                        input$keep_infos)
                    df_from_inf <- fixParents.data.frame(df_from_inf,
                        delete = FALSE, filter = "useful")
                    df_from_inf <- fixParents.data.frame(df = df_from_inf,
                        delete = TRUE)
                }
                df_from_inf
            },
            error = function(e) {
                message(paste("Error in ped_df_inf", e))
                NULL
            })
        } else {
            NULL
        }
    })
    ## Subfamily selection ---------------------
    subfamilies_table <- reactive({
        print("Bal: subfamilies_table")
        shiny::req(ped_df_inf())
        ped_df <- ped_df_inf()
        if (!is.null(ped_df) & !is.null(input$families_var_sel)) {
            get_families_table(ped_df, input$families_var_sel)
        } else {
            NULL
        }
    })
    output$subfamilies_table <- DT::renderDataTable({
        subfamilies_table()
    }, options = list(paging = FALSE, scrollX = TRUE,
        scrollY = "200px", scrollCollapse = TRUE),
    rownames = FALSE)
    output$subfamily_selector <- renderUI({
        print("Bal: subfamily_sel")
        if (!is.null(subfamilies_table())) {
            fam_nb <- as.numeric(subfamilies_table()$FamilyNum)
            if (max(fam_nb) > 0) {
                numericInput("subfamily_sel",
                    label = h5(strong("Select subfamily to use")),
                    value = 1, min = min(fam_nb), max = max(fam_nb))
            } else {
                textOutput("No family present (only unconnected individuals)")
            }
        } else {
            NULL
        }
    })

    ## Final ped_df ---------------------------
    ped_df_final <- reactive({
        print("Bal: ped_df_final")
        shiny::req(ped_df_inf())
        shiny::req(input$subfamily_sel)
        ped_df <- ped_df_inf()
        if (!is.null(ped_df) & !is.null(input$subfamily_sel)) {
            ped_df <- ped_df[ped_df$family == input$subfamily_sel, ]
        }
        ped_df
    })

    ped_gens <- reactive({
        ped <- with(ped_df_final(), pedigree(id, dadid, momid, sex, affected))
        align.pedigree(ped)$n
    })

    ## Plotting -------------------------------
    plotped_obj <- plot_ped_server(
        "plot_ped", ped_df_final, get_title_app(short = FALSE))

    output$legend_plot <- renderPlot({
        shiny::req(ped_df_aff())
        cols <- ped_df_aff()$scales
        legend <- create_legend(cols, size = 0.8)
        gridExtra::grid.arrange(legend$A, legend$B, ncol = 2)
    }, height = 150)

    ## Download pedigree-----------------------
    get_title_app <- function(short = FALSE) {
            df <- families_table()
            get_title(
                input$family_sel, input$subfamily_sel,
                input$families_var_sel,
                df[df$FamilyNum == input$family_sel, "Major mod"],
                inf_inds_sel_txt(), input$kin_max,
                input$trim_ped, input$keep_infos,
                nrow(ped_df_final()),
                short_title = short)
    }
    data_download_server("plot_data_dwnl", ped_df_final,
        filename = get_title_app(short = TRUE), label = "Subfamily data",
        helper = FALSE)

    shiny::observeEvent(ped_gens(), {
        plot_download_server(
            "plot_ped_dwnl", plotped_obj,
            filename = get_title_app(short = TRUE), label = "Subfamily plot",
            width = max(c(ped_gens() * 80, 500)),
            height = max(c(length(ped_gens()) * 150, 500)))
    })

    ## End ------------------------------------
    if (!interactive()) {
        session$onSessionEnded(function() {
        shiny::stopApp()
        q("no")
        })
    }
})
