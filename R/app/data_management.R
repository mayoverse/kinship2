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
        message("Data normalization done")

        message("Making family number")
        df_norm[[1]]$family <- as.factor(with(df_norm[[1]],
            makefamid(id, dadid, momid)))
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
