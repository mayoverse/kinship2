#' Print0 to max
#'
#' Print0 the elements inside a vector until a maximum is reached.
#'
#' @param x A vector.
#' @param max The maximum number of elements to print.
#' @param ... Additional arguments passed to print0
#'
#' @return The character vector aggregated until the maximum is reached.
#' @keywords internal
paste0max <- function(x, max = 5, sep = "", ...) {
    lgt <- min(length(x), max)
    ext <- ifelse(length(x) > max, "...", "")
    paste(
        "'", paste0(x[seq_len(lgt)], collapse = "', '"),
        "'", ext, sep = sep, ...
    )
}

#' Check if the fields are present in an object slot
#'
#' @param obj An object.
#' @param slot A slot of object.
#' @param fields A character vector with the fields to check.
#'
#' @return A character vector with the errors if any.
#' @keywords internal
check_slot_fd <- function(obj, slot = NULL, fields = character()) {
    if (is.object(obj)) {
        obj <- as.list(obj)
    }
    if (is.null(slot)) {
        array_names <- names(obj)
    } else if (is.data.frame(obj[[slot]])) {
        array_names <- colnames(obj[[slot]])
    } else if (is.list(obj[[slot]])) {
        array_names <- names(obj[[slot]])
    } else {
        stop(
            "Slot ", slot, " is not a data.frame or a list. ",
            class(obj[[slot]]), " found."
        )
    }
    if (length(array_names) == 0) {
        paste0(
            "Missing fields in ", slot,
            " slot. See Pedigree documentation."
        )
    } else if (any(!fields %in% array_names)) {
        paste0(
            "`", paste0max(fields[!fields %in% array_names]),
            "`", " column(s) is not present in slot ", slot, "."
        )
    }
}

#' Check values in a slot
#'
#' Check if the all the values in a slot are in a vector of values.
#'
#' @param obj An object.
#' @param slot A slot of the object.
#' @param column A column of the slot.
#' @param values A vector of values to check.
#' @param present A logical value indicating if the values should be present
#' or not
#'
#' @return A character vector with the errors if any.
#' @keywords internal
check_values <- function(val, ref, name = NULL, present = TRUE) {
    if (length(dim(val)) > 1) {
        stop("val must be a vector")
    }

    if (present) {
        val_abs <- !val %in% ref
        should <- " should be in "
    } else {
        val_abs <- val %in% ref
        should <- " should not be in "
    }

    val_name <- ifelse(is.null(name), "Values ", paste(name, "values "))

    if (any(val_abs)) {
        paste0(
            val_name, paste0max(val[val_abs]), should,
            paste0max(ref)
        )
    }
}

#' Check if the Hints are valid
#'
#' Check if horder and spouse slots are valid (i.e. horder is numeric and
#' spouse is a matrix with 3 columns).
#' Check if the spouse matrix is valid (i.e. only numeric values).
#' @param object A Hints object.
#'
#' @return A character vector with the errors or `TRUE` if no errors.
#' @keywords internal
#'
is_valid_hints <- function(object) {
    errors <- c()

    #### Check that the slots have the right columns ####
    errors <- c(errors, check_slot_fd(object, "horder", "numeric"))
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

#' Check if the Scales are valid
#'
#' Check if the fill and border slots are valid (i.e. they have the right
#' columns).
#'
#' @param object A Scales object.
#'
#' @return A character vector with the errors or `TRUE` if no errors.
#' @keywords internal
#'
#' @export
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

#' Check if the Ped is valid
#'
#' Multiple checks are done here
#'
#' 1. Check that the ped ids slots have the right values
#' 2. Check that the sex, steril, status, avail and affected slots have the
#' right values
#' 3. Check that dad are male and mom are female
#' 4. Check that individuals have both parents or none
#' 
#' @param object A Ped object.
#'
#' @return A character vector with the errors or `TRUE` if no errors.
#'
#' @keywords internal
is_valid_ped <- function(object) {
    missid <- NA_character_
    errors <- c()

    #### Check that all slots are parallele ####
    #errors <- c(errors, parallel_slot_names(object))

    #### Check that the ped columns have the right values ####
    # Check for ped@id uniqueness
    if (any(duplicated(object@id))) {
        errors <- c(errors, "Id in ped slot must be unique")
    }

    # Control values for ids
    famid <- unique(object@famid)
    errors <- c(errors, check_values(
        famid, c(""), "famid", present = FALSE
    ))
    errors <- c(errors, check_values(
        object@id,
        c(
            missid, "",
            paste(as.character(missid), famid, sep = "_"),
            paste("", famid, sep = "_")
        ),
        "id", present = FALSE
    ))
    errors <- c(errors, check_values(
        object@dadid, c(object@id, missid)
    ))
    errors <- c(errors, check_values(
        object@momid, c(object@id, missid)
    ))

    # Control values for sex, steril, status, avail and affected
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

    rel_cols <- c("id1", "id2", "code", "famid")
    #### Check that the slots have the right columns ####
    errors <- c(errors, check_slot_fd(object, NULL, rel_cols))

    #### Check that the rel columns have the right values ####
    codes <- c("MZ twin", "DZ twin", "UZ twin", "Spouse")
    errors <- c(errors, check_values(object@code, codes))

    #### Check that id1 is different from id2 ####
    if (any(object@id1 == object@id2)) {
        id1e <- object@id1[object@id1 == object@id2]
        id2e <- object@id2[object@id1 == object@id2]
        errors <- c(errors, paste(
            "id1 '", paste0(id1e, collapse = "', '"),
            "' should be different to id2 '", paste0(id2e, collapse = "', '"),
            "'.", sep = ""
        ))
    }

    #### Check that all id1 is smaller than id2 ####
    if (any(object@id1 > object@id2)) {
        id1b <- object@id1[object@id1 > object@id2]
        id2b <- object@id2[object@id1 > object@id2]
        errors <- c(errors, paste(
            "id1 '", paste0(id1b, collapse = "', '"),
            "' should be smaller than id2 '", paste0(id2b, collapse = "', '"),
            "'.", sep = ""
        ))
    }

    #### Check absence of duplicate ####
    idr <- paste(object@id1, object@id2, sep = "_")
    if (any(duplicated(idr))) {
        idd <- idr[duplicated(idr)]
        errors <- c(errors, paste(
            "Pairs of individuals should be unique",
            " ('", paste0(idd, collapse = "', '"), "').", sep = ""
        ))
    }

    if (length(errors) == 0) {
        TRUE
    } else {
        errors
    }
    return(errors)
}

is_valid_pedigree <- function(object) {
    errors <- c()

    #### Check that the famid id and individual id present in the rel slot ####
    #### are present in the ped slot ####
    errors <- c(errors, check_values(
        object@rel@famid, c(object@ped@famid, NA)
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