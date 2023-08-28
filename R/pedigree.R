#' Create a Pedigree object from a data.frame
#'
#'  This constructor help to create a \\code{Pedigree} object from
#' `data.frame`.
#'
#' @param ped_df A data.frame with the individuals informations.
#' @param rel_df A data.frame with the special relationships between
#' individuals.
#' @param cols_ren_ped A named list with the columns to rename for the
#' pedigree dataframe.
#' @param cols_ren_rel A named list with the columns to rename for the
#' relationship matrix.
#' @param scales A list of two data.frame with the scales to use for the
#' affection status and the other one for the border color (e.g availability).
#' @param normalize A logical to know if the data should be normalised.
#' @param ... Other arguments to pass to the function `generate_colors`.
#'
#' @return A Pedigree object.
#' @export pedigree
pedigree <- function(
    ped_df = data.frame(
        id = character(),
        dadid = character(),
        momid = character(),
        sex = numeric(),
        family = character(),
        available = numeric(),
        affected = numeric()
    ),
    rel_df = data.frame(
        id1 = character(),
        id2 = character(),
        code = numeric(),
        family = character()
    ),
    cols_ren_ped = list(
        "indId" = "id",
        "fatherId" = "dadid",
        "motherId" = "momid",
        "gender" = "sex"
    ),
    cols_ren_rel = list(
        "indId1" = "id1",
        "indId2" = "id2"
    ),
    scales = list(
        fill = data.frame(
            order = numeric(),
            column_values = character(),
            column_mods = character(),
            mods = numeric(),
            labels = character(),
            affected = logical(),
            fill = character(),
            density = numeric(),
            angle = numeric()
        ),
        border = data.frame(
            column = character(),
            mods = numeric(),
            labels = character(),
            border = character()
        )
    ),
    hints = list(
        order = NULL,
        spouse = NULL
    ),
    normalize = TRUE,
    ...
) {
    ## Rename columns ped
    old_cols <- as.vector(unlist(cols_ren_ped))
    new_cols <- names(cols_ren_ped)
    cols_to_ren <- match(old_cols, names(ped_df))
    names(ped_df)[cols_to_ren[!is.na(cols_to_ren)]] <-
        new_cols[!is.na(cols_to_ren)]

    ## Rename columns rel
    old_cols <- as.vector(unlist(cols_ren_rel))
    new_cols <- names(cols_ren_rel)
    cols_to_ren <- match(old_cols, names(rel_df))
    names(rel_df)[cols_to_ren[!is.na(cols_to_ren)]] <-
        new_cols[!is.na(cols_to_ren)]
    ## Normalise the data before creating the object
    if (normalize) {
        ped_df <- norm_ped(ped_df)
        rel_df <- norm_rel(rel_df)
    } else {
        cols_need <- c("id", "dadid", "momid", "sex")
        cols_to_use <- c("steril", "avail", "family", "status", "affected")
        ped_df <- check_columns(
            ped_df, cols_need, "", cols_to_use,
            others_cols = TRUE, cols_to_use_init = TRUE
        )
        cols_need <- c("id1", "id2", "code")
        cols_to_use <- c("family")
        rel_df <- check_columns(
            rel_df, cols_need, "", cols_to_use, cols_to_use_init = TRUE
        )
    }
    if (any(!is.na(ped_df$error))) {
        warning("The pedigree informations are not valid.")
        print("Here is the normalised pedigree informations with the errors")
        return(ped_df)
    }

    if (any(!is.na(rel_df$error))) {
        warning("The relationship informations are not valid.")
        print(paste0("Here is the normalised relationship informations",
            "with the errors"
        ))
        return(rel_df)
    }
    ## Create the object
    ped <- new("Pedigree", ped = ped_df, rel = rel_df,
        scales = scales, hints = hints
    )

    generate_colors(ped, ...)
}
