usethis::use_package("dplyr")
## Choose family to plot the name used will be the most numerous race
get_families_table <- function(df, var) {
  if (is.null(df[[var]]) || is.null(df$family)) {
    return(NULL)
  }
  df <- as.table(table(var = df[[var]], Famille = df$family))

  families_table <- t(apply(df, 2,
    function(x) {
        c(rownames(df)[which.max(x)], sum(x))
    }))
  colnames(families_table) <- c("Major mod", "Nb Ind")

  families_table <- as.data.frame(families_table)
  families_table$FamilyNum <- rownames(families_table)
  families_table <- families_table[c(3, 1, 2)]
  if ("0" %in% families_table$FamilyNum) {
    families_table$"Major mod"[families_table$FamilyNum == "0"] <- "Connected to none"
  }
  return(families_table)
}


select_from_inf <- function(df, inf_inds, kin_max) {
  if (nrow(df) > 2) {
    print("Bal: select_from_inf")

    col_used <- c("to_use")
    df <- check_columns(df, cols_used = col_used, others_cols = TRUE)

    df_kin <- max_kin_inf(df, inf_inds)
    df_kin$to_use <- df_kin$kin <= kin_max & !is.na(df_kin$kin)

    df_kin_trim <- fixParents.data.frame(df = df_kin, delete = FALSE,
      filter = "to_use")
    df_kin_trim <- fixParents.data.frame(df = df_kin_trim, delete = TRUE)
    if (nrow(df_kin_trim) > 2) {
      df_kin_trim$family <- with(df_kin_trim, makefamid(id, momid, dadid))
    } else {
      message("Not Enough individuals")
      NULL
    }
    df_kin_trim
  } else {
    message("Not Enough individuals")
    NULL
  }
}

create_legend <- function(lgd_list, nbColMany = 2, size = 1) {
  legend <- list()
  it1 <- 1
  for (it1 in seq_along(lgd_list)) {
    i <- names(lgd_list)[[it1]]
    print(lgd_list[[i]])
    legend_labels <- names(lgd_list[[i]])
    nbMod <- length(lgd_list[[i]])
    if (i == "Availability") {
      fillColor <- rep("white", length(lgd_list[[i]]))
      borderColor <- lgd_list[[i]]
    } else {
      fillColor <- lgd_list[[i]]
      borderColor <- rep("black", length(lgd_list[[i]]))
    }

    data <- data.frame(X = rep(1, nbMod), Y = 1:nbMod, Aff = as.character(1:nbMod))
    if (nbMod > 5) {
      nbcol <- nbColMany
    } else {
      nbcol <- 1
    }

    grob <- ggplot(data, aes(X, Y)) +
      geom_point(aes(fill = Aff), shape = 21, size = 10, stroke = 1.5) +
      scale_fill_manual(
        values = setNames(fillColor, 1:nbMod), name = i,
        labels = setNames(legend_labels, 1:nbMod), drop = FALSE, guide = TRUE
      ) +
      guides(fill = guide_legend(override.aes = list(col = borderColor, fill = fillColor, size = size * 10), ncol = nbcol)) +
      theme(
        legend.margin = margin(0, 0, 0, 0, "in"),
        legend.title = element_text(size = 20 * size),
        legend.text = element_text(size = 18 * size),
        legend.justification = "top",
        plot.background = element_rect(fill = "red"),
      )

    legend[[LETTERS[it1]]] <- ggpubr::get_legend(grob)
  }
  return(legend)
}

get_title <- function(
  family_sel, subfamily_sel,
  family_var, mod,
  inf_selected, kin_max, trim_ped, keep_infos,
  nb_rows,
  short_title = FALSE) {
      if (subfamily_sel == "0") {
          "Subfamily containing individuals not linked to any"
      } else {
        if (short_title) {
          trim_text <- ifelse(trim_ped, ifelse(keep_infos, "11", "10"), "00")
          title <- paste0(c("Ped", family_var, mod,
              "-K", kin_max, "-T", trim_text,
              "-I", inf_selected, "_SF", subfamily_sel),
              collapse = "")
          title <- stringr::str_replace(title, "/", "-")
          stringr::str_replace(title, " ", "-")
      } else {
          trim_text <- ifelse(trim_ped, "trimmed", "")
          paste0(c("Pedigree", trim_text,
            "of", family_var, mod,
            "family N\u00B0", family_sel,
            "sub-family N\u00B0", subfamily_sel,
            "( N=", nb_rows, ") from",
            inf_selected, "individuals"), collapse = " ")
      }
  }
}