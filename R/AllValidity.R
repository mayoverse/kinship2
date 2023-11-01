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
            "No fields in ", slot,
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
#' @param val A vector of values to check.
#' @param ref A vector of reference values.
#' @param name A character vector with the name of the values to check.
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

    #### Check that the slots are of the right class ####
    if (! is.numeric(object@horder)) {
        errors <- c(errors, "horder slot must be numeric")
    }
    if (! is.data.frame(object@spouse)) {
        errors <- c(errors, "spouse slot must be a data.frame")
    }

    #### Check that the horder slot is valid ####
    errors <- c(errors, check_values(
        object@horder, NA_real_, "horder", present = FALSE
    ))

    #### Check that the hints spouse data.frame is valid ####
    errors <- c(errors, check_slot_fd(
        object, "spouse", c("idl", "idr", "anchor")
    ))

    if (!is.factor(object@spouse$anchor)) {
        errors <- c(errors, "anchor column must be a factor")
    }

    errors <- c(errors, check_values(
        object@spouse$anchor, c("left", "right", "either"), "anchor"
    ))

    idmin <- pmin(object@spouse$idl, object@spouse$idr)
    idmax <- pmax(object@spouse$idl, object@spouse$idr)
    if (any(idmin == idmax)) {
        errors <- c(errors, "idl and idr should be different")
    }
    dup <- anyDuplicated(cbind(idmin, idmax))
    if (dup) {
        dup <- paste(idmin[dup], idmax[dup], sep = "_")
        errors <- c(errors, paste(
            "idl and idr should be unique:",
            paste(dup, collapse = ", "),
            "couples are present more than once in the spouse slot."
        ))
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
    errors <- c(errors, check_slot_fd(object, NULL, c("fill", "border")))
    errors <- c(errors, check_slot_fd(object, "fill", fill_cols))
    errors <- c(errors, check_slot_fd(object, "border", border_cols))

    #### Check that the fill columns have the right values ####
    ## Check for logical columns
    col_log <- c("affected")
    err_log <- col_log[!unlist(lapply(object@fill[col_log], is, "logical"))]
    if (length(err_log) > 0) {
        errors <- c(errors, paste(
            err_log, " column(s) must be logical", sep = ""
        ))
    }

    ## Check for numeric columns
    col_num <- c("density", "angle", "order", "mods")
    err_num <- col_num[!unlist(lapply(object@fill[col_num], is, "numeric"))]
    if (length(err_num) > 0) {
        errors <- c(errors, paste(
            err_num, " column(s) must be numeric", sep = ""
        ))
    }

    ## Check for character columns
    col_char <- c(
        "column_values", "column_mods", "labels", "fill"
    )
    err_char <- col_char[!unlist(lapply(object@fill[col_char], is, "character"))]
    if (length(err_char) > 0) {
        errors <- c(errors, paste(
            err_char, " column(s) must be character", sep = ""
        ))
    }

    #### Check that the border columns have the right values ####
    ## Check for character columns
    col_char <- c("column", "labels", "border")
    err_char <- col_char[!unlist(lapply(object@border[col_char], is, "character"))]
    if (length(err_char) > 0) {
        errors <- c(errors, paste(
            err_char, " column(s) must be character", sep = ""
        ))
    }

    ## Check for numeric columns
    col_num <- c("mods")
    err_num <- col_num[!unlist(lapply(object@border[col_num], is, "numeric"))]
    if (length(err_num) > 0) {
        errors <- c(errors, paste(
            err_num, " column(s) must be numeric", sep = ""
        ))
    }

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
        object@dadid, c(object@id, missid), "dadid"
    ))
    errors <- c(errors, check_values(
        object@momid, c(object@id, missid), "momid"
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
        object@rel@famid, c(object@ped@famid, NA), "Rel famid"
    ))
    errors <- c(errors, check_values(object@rel@id1, object@ped@id, "Rel id1"))
    errors <- c(errors, check_values(object@rel@id2, object@ped@id, "Rel id2"))

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
    ped <- as.data.frame(ped(object))
    errors <- c(errors, check_values(
        fill(object)$column_values, colnames(ped),
        "fill column_values"
    ))
    errors <- c(errors, check_values(
        fill(object)$column_mods, colnames(ped),
        "fill column_mods"
    ))
    errors <- c(errors, check_values(
        border(object)$column, colnames(ped),
        "border column"
    ))

    #### Check that all fill modalities are present in the pedigree data ####
    for (col in unique(fill(object)$column)){
        errors <- c(errors, check_values(
            ped[[col]],
            fill(object)[fill(object)$column_mods == col, "mods"],
            paste("fill column", col)
        ))
    }
    #### Check that all borders modalities are present in the pedigree data ####
    for (col in unique(border(object)$column)){
        errors <- c(errors, check_values(
            ped[[col]],
            border(object)[border(object)$column == col, "mods"],
            paste("border column", col)
        ))
    }

    #### Check that the hints are valid ####
    if (length(horder(object)) > 0 &&
            length(horder(object)) != length(object)
    ) {
        errors <- c(errors,
            "Length for horder component should be equal to Pedigree length"
        )
    }

    idl <- spouse(object)$idl
    idr <- spouse(object)$idr

    ## Check for presence of spouses in Ped object
    idabs <- c(idl, idr)[!c(idl, idr) %in% id(ped(object))]
    if (length(idabs) > 0) {
        errors <- c(errors, paste(
            "Hints spouse(s)",
            paste(idabs, sep = ","),
            "not present in the Ped object"
        ))
    }

    ## Check for sex of spouses
    idls <- sex(ped(object))[match(idl, id(ped(object)))]
    idlr <- sex(ped(object))[match(idr, id(ped(object)))]
    sps <- paste(idls, idlr, sep = "_")
    sps <- sps %in% c("female_male", "male_female")
    if (any(!sps)) {
        errors <- c(errors, paste(
            "Hints spouse(s)",
            paste(paste(idl[!sps], idr[!sps], sep = "_"), sep = ","),
            "not female, male"
        ))
    }
    if (length(errors) == 0) {
        TRUE
    } else {
        errors
    }

    return(errors)
}