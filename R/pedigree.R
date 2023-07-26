#' Create a Pedigree object
#'
#' These functions help to create a \\code{Pedigree} object from
#' `data.frame`.
#'
#' @param ped_df
#' @param rel_df
#' @param cols_ren_ped
#' @return A Pedigree object.
#' @export
pedigree <- function(x, ...) {
    UseMethod("pedigree")
}

#' Create a Pedigree object from a data.frame
#'
#' @param ped_df A data.frame with the individuals informations.
#' @param rel_df A data.frame with the special relationships between individuals.
#' @param cols_ren_ped A named list with the columns to rename.
#' @param scales A data.frame with the scales to use for the affection status.
#'
#' @return A Pedigree object.
#'
#' @method pedigree data.frame
#' @export
pedigree.data.frame <- function(
    ped_df = data.frame(
        id = character(),
        dadid = character(),
        momid = character(),
        sex = numeric()),
    rel_df = data.frame(
        id1 = character(),
        id2 = character(),
        code = numeric()),
    cols_ren_ped = list(
        "indId" = "id",
        "fatherId" = "dadid",
        "motherId" = "momid",
        "gender" = "sex"),
    scales = data.frame(
        column = character(),
        mods_labels = character(),
        fill = character(),
        border = character(),
        density = numeric(),
        angle = numeric())) {
    ## Rename columns
    old_cols <- as.vector(unlist(cols_ren_ped))
    new_cols <- names(cols_ren_ped)
    cols_to_ren <- match(old_cols, names(ped_df))
    names(ped_df)[cols_to_ren[!is.na(cols_to_ren)]] <-
        new_cols[!is.na(cols_to_ren)]
    ## Normalise the data before creating the object
    ped_df <- norm_ped(ped_df)
    rel_df <- norm_rel(rel_df)
    ## Create the object
    new("Pedigree", ped = ped_df, rel = rel_df, scales = scales)
}
TRUE
