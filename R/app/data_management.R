usethis::use_package("dplyr")
check_data <- function(df) {
     # Deletion of duplicated lines
    df <- df %>%
        dplyr::distinct(.keep_all = TRUE)

    col_used <- c("family")
    df <- check_columns(df, cols_used = col_used, others_cols = TRUE)
    message("Checked columns")

    if (is.null(df)) {
        NULL
    } else {
        df_norm <- norm_data(df)
        message("Pedigree data normalization done")

        message("Making family number")
        df_norm[[1]]$family <- as.factor(with(df_norm[[1]],
            makefamid(id, dadid, momid)))
        df_norm
    }
}

check_rel <- function(df) {
     # Deletion of duplicated lines
    df <- df %>%
        dplyr::distinct(.keep_all = TRUE)

    col_needed <- c("id1", "id2", "code")
    col_used <- c("family")
    df <- check_columns(df, col_needed, col_used, others_cols = FALSE)
    message("Checked columns")

    if (is.null(df)) {
        NULL
    } else {
        df_norm <- norm_rel(df)
        message("Relationship data normalization done")
        df_norm
    }
}


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
    df_kin <- max_kin_inf(df, inf_inds)
    df_kin_lim <- df_kin[df_kin$kin <= kin_max, ]
    df_kin_trim <- fixParents.data.frame(df = df_kin_lim, delete = TRUE)

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