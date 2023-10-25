na_to_length <- function(x, y, value) {
    if (length(x) == 1 & all(is.na(x))) {
        rep(value, length(y))
    } else {
        x
    }
}

#' Constructor for the Ped class
#' @description Constructor for the Ped class
#' @param id A character vector with the id of the individuals.
#' @param dadid A character vector with the id of the father of the individuals.
#' @param momid A character vector with the id of the mother of the individuals.
#' @param family A character vector with the family of the individuals.
#' @param sex A factor vector with the sex of the individuals (i.e. `male`,
#' `female`, `unknown` or `terminated`).
#' @param steril A numeric vector with the sterilisation status of the
#' individuals (i.e. `0` = not sterilised, `1` = sterilised, `NA` = unknown).
#' @param status A numeric vector with the affection status of the
#' individuals (i.e. `0` = alive, `1` = dead, `NA` = unknown).
#' @param avail A numeric vector with the availability status of the
#' individuals (i.e. `0` = not available, `1` = available, `NA` = unknown).
#'
#' @return A Ped object.
#' @seealso [Pedigree()]
#'
#' @export
Ped <- function(
    id, sex, dadid, momid, family = NA,
    steril = NA, status = NA, avail = NA,
    affected = NA, useful = NA, kin = NA,
    missid = NA
) {
    family <- na_to_length(family, id, as.character(NA))
    id <- as.character(id)
    dadid <- as.character(dadid)
    momid <- as.character(momid)
    missid <- as.character(missid)

    sex <- sex_to_factor(sex)

    steril <- na_to_length(steril, id, as.numeric(NA))
    status <- na_to_length(status, id, as.numeric(NA))
    avail <- na_to_length(avail, id, as.numeric(NA))
    affected <- na_to_length(affected, id, as.numeric(NA))
    useful <- na_to_length(useful, id, as.numeric(NA))
    kin <- na_to_length(kin, id, as.numeric(NA))

    df_child <- num_child(id, dadid, momid, rel_df = NULL, missid = missid)

    new(
        "Ped",
        id = id, dadid = dadid, momid = momid, family = family,
        sex = sex, steril = steril, status = status, avail = avail,
        affected = affected, useful = useful, kin = kin,
        num_child_total = df_child$num_child_tot,
        num_child_direct = df_child$num_child_dir,
        num_child_indirect = df_child$num_child_ind
    )
}
