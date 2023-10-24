is_valid_hints <- function(object) {
    errors <- c()

    #### Check that the slots have the right columns ####
    errors <- c(errors, check_slot_fd(object, "order", "numeric"))
    errors <- c(errors, check_slot_fd(object, "spouse", "matrix"))

    #### Check that the hints spouse matrix is valid ####
    if (dim(object@spouse) != 3) {
        errors <- c(errors, "The spouse matrix must have 3 columns.")
    }
    if (any(is.numeric(object@spouse[, 1:3]))) {
        errors <- c(
            errors, "The spouse matrix must contains only numeric values."
        )
    }

    return(errors)
}

is_valid_scales <- function(object) {
    errors <- c()

    fill_cols <- c(
        "order", "column_values", "column_mods", "mods",
        "labels", "affected", "fill", "density", "angle"
    )
    border_cols <- c("column", "mods", "labels", "border")
    errors <- c(errors, check_slot_fd(object, "scales", c("fill", "border")))
    errors <- c(errors, check_slot_fd(object@scales, "fill", fill_cols))
    errors <- c(errors, check_slot_fd(object@scales, "border", border_cols))

    if (length(errors) == 0) {
        TRUE
    } else {
        errors
    }

    return(errors)
}


is_valid_ped <- function(object) {
    missid <- NA
    errors <- c()

    #### Check that the ped columns have the right values ####
    # Check for ped@id uniqueness
    if (any(duplicated(object@id))) {
        errors <- c(errors, "Id in ped slot must be unique")
    }

    # Control values for ped
    errors <- c(errors, check_values(object@id, missid, present = FALSE))
    errors <- c(errors, check_values(
        object@dadid, c(object@id, missid)
    ))
    errors <- c(errors, check_values(
        object@momid, c(object@id, missid)
    ))
    sex_code <- c("male", "female", "unknown", "terminated")
    errors <- c(errors, check_values(object@sex, sex_code))
    errors <- c(errors, check_values(object@steril, c(0, 1, NA)))
    errors <- c(errors, check_values(object@status, c(0, 1, NA)))
    errors <- c(errors, check_values(object@avail, c(0, 1, NA)))
    errors <- c(errors, check_values(object@affected, c(0, 1, NA)))

    # Control sex for parents
    id <- object@id
    momid <- object@momid
    dadid <- object@dadid
    sex <- object@sex
    is_dad <- id %in% dadid
    is_mom <- id %in% momid
    if (any(sex[is_dad] != "male")) {
        errors <- c(errors, "Some dad are not male")
    }
    if (any(sex[is_mom] != "female")) {
        errors <- c(errors, "Some mom are not female")
    }
    if (any(
        (dadid %in% missid & (! momid %in% missid)) |
            ((! dadid %in% missid) & momid %in% missid)
    )) {
        errors <- c(errors, "Individuals should have both parents or none")
    }

    if (length(errors) == 0) {
        TRUE
    } else {
        errors
    }

    return(errors)
}

is_valid_rel <- function(object) {
    errors <- c()

    rel_cols <- c("id1", "id2", "code", "family")
    #### Check that the slots have the right columns ####
    errors <- c(errors, check_slot_fd(object, NULL, rel_cols))

    #### Check that the rel columns have the right values ####
    codes <- c("MZ twin", "DZ twin", "UZ twin", "Spouse")
    errors <- c(errors, check_values(object@rel@code, codes))

    if (length(errors) == 0) {
        TRUE
    } else {
        errors
    }

    return(errors)
}

is_valid_pedigree <- function(object) {
    errors <- c()

    #### Check that the family id and individual id present in the rel slot ####
    #### are present in the ped slot ####
    errors <- c(errors, check_values(
        object@rel@family, c(object@ped@family, NA)
    ))
    errors <- c(errors, check_values(object@rel@id1, object@ped@id))
    errors <- c(errors, check_values(object@rel@id2, object@ped@id))

    #### Check if twins has same parents ####
    code <- object@rel@code
    ncode <- as.numeric(code)
    id1 <- object@rel@id1
    id2 <- object@rel@id2
    id <- object@ped@id
    momid <- object@ped@momid
    dadid <- object@ped@dadid
    sex <- object@ped@sex
    temp1 <- match(id1, id, nomatch = 0)
    temp2 <- match(id2, id, nomatch = 0)
    if (any(ncode < 3)) {
        twins <- (ncode < 3)
        if (any(momid[temp1[twins]] != momid[temp2[twins]])) {
            errors <- c(errors, "twins found with different mothers")
        }
        if (any(dadid[temp1[twins]] != dadid[temp2[twins]])) {
            errors <- c(errors, "twins found with different fathers")
        }
    }

    #### Check if the monozygote twins has same gender ####
    if (any(ncode == 1)) {
        mztwins <- (ncode == 1)
        if (any(sex[temp1[mztwins]] != sex[temp2[mztwins]])) {
            errors <- c(errors, "MZ twins with different genders")
        }
    }

    #### Check that the scales columns have the right values ####
    errors <- c(errors, check_values(
        object@scales@fill$column_values, colnames(object@ped)
    ))
    errors <- c(errors, check_values(
        object@scales@fill$column_mods, colnames(object@ped)
    ))
    errors <- c(errors, check_values(
        object@scales@border$column, colnames(object@ped)
    ))

    #### Check that all fill modalities are present in the pedigree data ####
    for (col in unique(object@scales@fill$column)){
        errors <- c(errors, check_values(
            object@ped[[col]],
            object@scales@fill[object@scales@fill$column_mods == col, "mods"]
        ))
    }
    #### Check that all borders modalities are present in the pedigree data ####
    for (col in unique(object@scales@border$column)){
        errors <- c(errors, check_values(
            object@ped[[col]],
            object@scales@border[object@scales@border$column_mods == col, "mods"]
        ))
    }

    if (length(errors) == 0) {
        TRUE
    } else {
        errors
    }

    return(errors)
}