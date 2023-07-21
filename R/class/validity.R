#' Check if the colnames are present in an object slot
#'
#' @param object An object.
#' @param slot A slot of object.
#' @param colnames A character vector with the colnames to check.
#'
#' @return A character vector with the errors if any.
checkColnamesSlot <- function(object, slot = NULL, colnames) {
    array_names <- colnames(slot(object, slot))

    if (length(array_names) == 0) {
        paste0("Missing required colnames for ", slot,
            ". See pedigree documentation.")
    } else if (any(checkColnames(array_names, colnames))) {
        paste0(paste0(colnames[checkColnames(array_names, colnames)],
            collapse = ", "),
            " column is not present on slot ", slot, ".")
    }
}

#' Check if the colnames are in the array
#'
#' @param array_names A character vector with the colnames of the array.
#' @param colnames A character vector with the colnames to check.
#'
#' @return A logical vector.
checkColnames <- function(array_names, colnames) {
    !colnames %in% array_names
}

#' Check values in a slot
#'
#' Check if the all the values in a slot are in a vector of values.
#'
#' @param object An object.
#' @param slot A slot of the object.
#' @param column A column of the slot.
#' @param values A vector of values to check.
#'
#' @return A character vector with the errors if any.
checkValues <- function(object, slot, column, values) {
    val <- slot(object, slot)[[column]]
    val_abs <- !val %in% values
    if (any(val_abs)) {
        paste0("Values ", val[val_abs], " in column ", column,
            " of slot ", slot, " should be in ",
            paste0(values, collapse = ", "), ".")
    } else {
        character()
    }
}

#' Pedigree validity method.
#'
#' Check if the Pedigree object is valid.
#'
#' It will check :
#' the colnames of the slots
#' the values in the columns of the ped, rel and scale slot
#' @param object A Pedigree object.
#' @return A logical value or a character vector with the errors.
isValid <- function(object) {
    missid <- "0"
    errors <- c()

    #### Check that the slots have the right columns ####
    ped_cols <- c("id", "dadid", "momid", "family",
        "sex", "steril", "status", "avail")
    rel_cols <- c("id1", "id2", "code", "family")
    scale_cols <- c("column", "mods_labels", "fill",
        "border", "density", "angle")
    errors <- c(errors, checkColnamesSlot(object, "ped", ped_cols))
    errors <- c(errors, checkColnamesSlot(object, "rel", rel_cols))
    errors <- c(errors, checkColnamesSlot(object, "scales", scale_cols))


    #### Check that the ped columns have the right values ####
    # Check for ped$id uniqueness
    if (any(duplicated(object@ped$id))) {
        errors <- c(errors, "Id in ped slot must be unique")
    }

    # Control values for ped
    errors <- c(errors, checkValues(object, "ped", "dadid",
        c(object@ped$id, missid)))
    errors <- c(errors, checkValues(object, "ped", "momid",
        c(object@ped$id, missid)))
    errors <- c(errors, checkValues(object, "ped", "sex", c(1:4)))
    errors <- c(errors, checkValues(object, "ped", "steril", c(0, 1, NA)))
    errors <- c(errors, checkValues(object, "ped", "status", c(0, 1, NA)))
    errors <- c(errors, checkValues(object, "ped", "avail", c(0, 1, NA)))

    # Control sex for parents
    id <- object@ped$id
    momid <- object@ped$momid
    dadid <- object@ped$dadid
    sex <- object@ped$sex
    is_dad <- id %in% dadid
    is_mom <- id %in% momid
    if (any(sex[is_dad] != 1)) {
        errors <- c(errors, "Some dad or not male")
    }
    if (any(sex[is_mom] != 2)) {
        errors <- c(errors, "Some mom or not female")
    }
    if (any((dadid == missid & momid != missid) |
        (dadid != missid & momid == missid))) {
        errors <- c(errors, "Individuals should have both parents or none")
    }

    #### Check that the rel columns have the right values ####
    errors <- c(errors, checkValues(object, "rel", "code",
        c("1", "2", "3", "4")))
    errors <- c(errors, checkValues(object, "rel", "family",
        c(object@ped$family, NA)))
    errors <- c(errors, checkValues(object, "rel", "id1", object@ped$id))
    errors <- c(errors, checkValues(object, "rel", "id2", object@ped$id))

    # Check if twins has same parents
    code <- object@rel$code
    ncode <- as.numeric(code)
    id1 <- object@rel$id1
    id2 <- object@rel$id2
    temp1 <- match(id1, id, nomatch = 0)
    temp2 <- match(id2, id, nomatch = 0)
    if (any(ncode < 3)) {
        twins <- (ncode < 3)
        if (any(momid[temp1[twins]] != momid[temp2[twins]])) {
            errors <- c(errors, "Twins found with different mothers")
        }
        if (any(dadid[temp1[twins]] != dadid[temp2[twins]])) {
            errors <- c(errors, "Twins found with different fathers")
        }
    }

    # Check if the monozygote twins has same gender
    if (any(code == "1")) {
        mztwins <- (code == "1")
        if (any(sex[temp1[mztwins]] != sex[temp2[mztwins]])) {
            errors <- c(errors, "MZ Twins with different genders")
        }
    }

    # Check that the scales columns have the right values
    errors <- c(errors, checkValues(object, "scales", "column",
        colnames(object@ped)))

    if (length(errors) == 0) {
        TRUE
    } else {
        errors
    }
}
TRUE
