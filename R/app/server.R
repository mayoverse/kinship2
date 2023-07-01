usethis::use_package("shiny")
usethis::use_package("shinyjs")
usethis::use_package("shinyWidgets")
usethis::use_package("dplyr")

## Sketch for family Health and Availability informations
sketch <- htmltools::withTags({
  table(
    class = "display",
    thead(
      tr(
        th(rowspan = 2,"Variable used"),
        th(class = 'dt-center',colspan = 3, "Availability")
      ),
      tr(
        th("Ind not Avail"),
        th("Ind Avail")
      )
    )
  )
})


shiny::shinyServer(function(input, output, session) {
    ## Ped data import -------------------------
    ped_df <- data_import_server(id = "data_ped_import",
        label = "Select pedigree file :")
    cols_needed_ped1 <- data_col_sel_server("data_ped_col_sel1", ped_df,
        c("indId", "fatherId", "motherId"),
        "Select columns :", null = TRUE)
    cols_needed_ped2 <- data_col_sel_server("data_ped_col_sel2", ped_df,
        c("gender", "steril", "available"),
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
        cols_ren <- c(cols_needed_ped1()[cols_needed_ped1() != ""],
            cols_needed_ped2()[cols_needed_ped2() != ""])
        cols_needed_ped <- c("indId", "fatherId", "motherId", "gender")
        if (any(!cols_needed_ped %in% names(cols_ren))) {
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
                ped_df_norm <- print_console(check_data(df_rename), session)
                if (length(ped_df_norm[[2]]) > 0) {
                    showNotification(paste(nrow(ped_df_norm[[2]]),
                        "errors as occured.",
                        "All individuals with errors wil be discarded"))
                }
                ped_df_norm
            }
        }
    })
    rel_df_norm <- shiny::reactive({
        print("Bal: rel management")
        cols_ren <- cols_needed_rel()[cols_needed_rel() != ""]
        cols_needed_rel <- c("id1", "id2", "code")
        if (any(!cols_needed_rel %in% names(cols_ren))) {
            NULL
        } else {
            rel_rename <- rel_df()
            data.table::setnames(rel_rename,
                old = as.vector(unlist(cols_ren)),
                new = names(cols_ren))
            rel_df_norm <- print_console(check_rel(rel_rename), session)
            if (length(ped_df_norm[[2]]) > 0) {
                showNotification(paste(nrow(ped_df_norm[[2]]),
                    "errors as occured.",
                    "All individuals with errors wil be discarded"))
            }
            rel_df_norm
        }
    })

    ## Errors download -------------------------
    shiny::observeEvent(ped_df_norm(), {
        data_download_server("ped_errors", 
            shiny::reactive({ped_df_norm()$errors}),
            "Pedigree data errors")
    })
    shiny::observeEvent(rel_df_norm(), {
        data_download_server("rel_errors",
            shiny::reactive({rel_df_norm()$errors}),
            "Relationship data errors")
    })

    ## Families selection and information ------
    # Families table
    families_table <- reactive({
        print("Bal: families_table")
        ped_df <- ped_df_norm()$norm
        if (!is.null(ped_df) & !is.null(input$families_var_sel)) {
            get_families_table(ped_df, input$families_var_sel)
        } else {
            NULL
        }
    })
    # Family selection variable
    output$families_var_selector <- renderUI({
        print("Bal: families_var")
        dfn <- ped_df_norm()[[1]]
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
    output$families_table <- renderTable({
        families_table()
    })
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
    # Family information
    output$family_info_table <- DT::renderDataTable({
        if (!is.null(families_table()) & !is.null(ped_df_aff())) {
            print("Bal: family_info_table")
            fam_nb <- as.numeric(families_table()$FamilyNum)
            if (max(fam_nb) > 0) {
                fam_sel <- input$family_sel
                if (fam_sel > 0) {
                    fam_df <- ped_df_aff()[ped_df_aff()$family == fam_sel,] 
                    fam_aff_cont <- fam_df %>%
                        dplyr::group_by(mods_aff, avail) %>%
                        dplyr::tally() %>%
                        tidyr::spread(avail, n)
                    DT::datatable(fam_aff_cont, container = sketch,
                        rownames=FALSE,
                        options = list(
                            columnDefs = list(
                            list(targets = "_all", className = "dt-center")),
                            dom = "t")
                    )
                } else {
                    NULL
                }
            } else {
                NULL
            }
        } else {
            NULL
        }
    })
    output$family_infos_title <- renderText({
        print("Bal: family_infos_title")
        if (!is.null(families_table())) {
            paste("Health & Availability data representation for family", input$family_sel)
        }else{
            NULL
        }
    })

    ## Health selection ------------------------
    output$health_var_selector <- renderUI({
        if (!is.null(ped_df_norm()$norm)) {
            cols_all <- colnames(ped_df_norm()$norm)
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
            if (is.numeric(ped_df_norm()$norm[[input$health_var_sel]])) {
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
        if (!is.null(input$health_var_sel)) {
            health_df <- ped_df_norm()$norm
            health_df <- health_df[health_df$family == input$family_sel,
                input$health_var_sel]
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

    ## Health affected generation ---------
    ped_df_aff <- shiny::reactive({
        print("Bal: ped_df_aff")
        if (!is.null(ped_df_norm())) {
            print_console(generate_aff_inds(ped_df_norm()$norm,
                col_aff = input$health_var_sel,
                mods_aff = input$health_aff_mods,
                threshold = input$health_threshold_val,
                sup_thres_aff = input$health_threshold_sup),
                session)
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
