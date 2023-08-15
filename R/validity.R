#' Print0 to max
#' 
#' Print0 the elements inside a vector until a maximum is reached.
#' 
#' @param x A vector.
#' @param max The maximum number of elements to print.
#' @param ... Additional arguments passed to print0
#' 
#' @export
paste0max <- function(x, max = 5, ...) {
    if (length(x) > max) {
        paste(paste0(unique(x[1:max]), collapse=", ", ...), "...")
    } else {
        paste0(unique(x), collapse=", ", ...)
    }
}

#' Check if the colnames are present in an object slot
#'
#' @param object An object.
#' @param slot A slot of object.
#' @param colnames A character vector with the colnames to check.
#'
#' @return A character vector with the errors if any.
checkColnamesSlot <- function(object, slot = NULL, colnames=character()) {
    array_names <- colnames(slot(object, slot))

    if (length(array_names) == 0) {
        paste0(
            "Missing columns in ", slot,
            " slot. See pedigree documentation.")
    } else if (any(!colnames %in% array_names)) {
        paste0(
            paste0max(colnames[!colnames %in% array_names]),
            " column(s) is not present in slot ", slot, ".")
    }
}

#' Check values in a slot
#'
#' Check if the all the values in a slot are in a vector of values.
#'
#' @param object An object.
#' @param slot A slot of the object.
#' @param column A column of the slot.
#' @param values A vector of values to check.
#' @param present A logical value indicating if the values should be present
#' or not
#'
#' @return A character vector with the errors if any.
checkValuesSlot <- function(object, slot, column, values, present=TRUE) {
    if (length(column) > 1) {
        stop("column must be a character vector of length 1.")
    }
    if (is.null(checkColnamesSlot(object, slot, column))){
        val <- slot(object, slot)[[column]]
        if (present) {
            val_abs <- !val %in% values
            should <- " should be in "
        } else {
            val_abs <- val %in% values
            should <- " should not be in "
        }
        
        if (any(val_abs)) {
            paste0(
                "Values ", paste0max(val[val_abs]), " in column ", column,
                " of slot ", slot, should,
                paste0max(values), ".")
        }
    } else {
        checkColnamesSlot(object, slot, column)
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
    ped_cols <- c(
        "id", "dadid", "momid", "family",
        "sex", "steril", "status", "avail")
    rel_cols <- c("id1", "id2", "code", "family")
    scale_cols <- c(
        "column", "mods_labels", "fill",
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
    errors <- c(errors, checkValuesSlot(
        object, "ped", "id", missid, present = FALSE))
    errors <- c(errors, checkValuesSlot(
        object, "ped", "dadid", c(object@ped$id, missid)))
    errors <- c(errors, checkValuesSlot(
        object, "ped", "momid", c(object@ped$id, missid)))
    sex_code <- c("male", "female", "unknown", "terminated")
    errors <- c(errors, checkValuesSlot(object, "ped", "sex", sex_code))
    errors <- c(errors, checkValuesSlot(object, "ped", "steril", c(0, 1, NA)))
    errors <- c(errors, checkValuesSlot(object, "ped", "status", c(0, 1, NA)))
    errors <- c(errors, checkValuesSlot(object, "ped", "avail", c(0, 1, NA)))

    # Control sex for parents
    id <- object@ped$id
    momid <- object@ped$momid
    dadid <- object@ped$dadid
    sex <- object@ped$sex
    is_dad <- id %in% dadid
    is_mom <- id %in% momid
    if (any(sex[is_dad] != "male")) {
        errors <- c(errors, "Some dad are not male")
    }
    if (any(sex[is_mom] != "female")) {
        errors <- c(errors, "Some mom are not female")
    }
    if (any((dadid %in% missid & (! momid %in% missid)) |
        ((! dadid %in% missid) & momid %in% missid))) {
        errors <- c(errors, "Individuals should have both parents or none")
    }

    #### Check that the rel columns have the right values ####
    codes <- c("MZ twin", "DZ twin", "UZ twin", "Spouse")
    errors <- c(errors, checkValuesSlot(
        object, "rel", "code", codes))
    errors <- c(errors, checkValuesSlot(
        object, "rel", "family", c(object@ped$family, NA)))
    errors <- c(errors, checkValuesSlot(object, "rel", "id1", object@ped$id))
    errors <- c(errors, checkValuesSlot(object, "rel", "id2", object@ped$id))

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
            errors <- c(errors, "twins found with different mothers")
        }
        if (any(dadid[temp1[twins]] != dadid[temp2[twins]])) {
            errors <- c(errors, "twins found with different fathers")
        }
    }

    # Check if the monozygote twins has same gender
    if (any(ncode == 1)) {
        mztwins <- (ncode == 1)
        if (any(sex[temp1[mztwins]] != sex[temp2[mztwins]])) {
            errors <- c(errors, "MZ twins with different genders")
        }
    }

    # Check that the scales columns have the right values
    errors <- c(errors, checkValuesSlot(
        object, "scales", "column", colnames(object@ped)))

    if (length(errors) == 0) {
        TRUE
    } else {
        errors
    }
}
