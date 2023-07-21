usethis::use_package("plyr")
usethis::use_package("dplyr")

## Normalize the date for the library and get the errors
#' Normalise dataframe
#'
#' @description Normalise dataframe for pedigree object
#'
#' @details Normalise a dataframe and check for columns correspondance
#' to be able to use it as an input to create pedigree object.
#' Multiple test are done and errors are checked.
#' Sex is calculated based in the `gender` column the following notations
#' are accepted: f, woman, female, 2 and m, man, male, 1.
#' The `steril` column need to be a boolean either TRUE, FALSE or 'NA'.
#' Will be considered available any individual with no 'NA' values in the
#' `available` column.
#' Duplicated `indId` will nullify the relationship of the individual.
#' All individuals with errors will be remove from the dataframe and will
#' be transfered to the error dataframe.
#'
#' @param ped_df The dataframe to process
#' @param na_strings Vector of strings to be considered as NA values
#'
#' @return List of two dataframe
#' `norm` is the dataframe with all the individuals without any errors.
#' `errors` is the dataframe containing the individuals with errors.
#'
#' @examples
#' data(ped_data)
#' norm_ped(ped_data)
#'
#'@export norm_ped
norm_ped <- function(ped_df, na_strings = c("NA", ""),
    missid = "0", try_num = FALSE) {
    print("Bal: norm_ped")
    err_cols <- c("sexErrMoFa", "sexErrFa", "sexErrMo", "sexErrTer", "sexNA",
        "sexError", "idErrFa", "idErrMo", "idErrSelf", "idErrOwParent",
        "idErrBothParent", "idError", "error")
    cols_need <- c("indId", "fatherId", "motherId", "gender")
    cols_used <- c("sex", "avail", "id", "dadid", "momid", err_cols, "family")
    cols_to_use <- c("steril", "available", "familyId", "status")

    ped_df <- check_columns(ped_df, cols_need, cols_used, cols_to_use,
        others_cols = TRUE, cols_to_use_init = TRUE, cols_used_init = TRUE)

    ped_df <- dplyr::mutate_if(ped_df, is.character,
        ~replace(., . %in% na_strings, NA))

    #### Sex ####
    if (is.factor(ped_df$gender) | is.numeric(ped_df$gender)) {
        ped_df$gender <- as.character(ped_df$gender)
    }
    # Normalized difference notations for sex
    sex_equiv <- c(f = "female", m = "male", woman = "female", man = "male",
        female = "female", male = "male", `2` = "female", `1` = "male",
        `3` = "unknown", `4` = "terminated")
    ped_df$sex <- as.character(plyr::revalue(as.factor(casefold(ped_df$gender,
        upper = FALSE)), sex_equiv))
    sex_codes <- c("male", "female", "unknown", "terminated")
    ped_df$sex[!ped_df$sex %in% sex_codes] <- "unknown"

    is_father <- ped_df$id %in% ped_df$dadid & !is.na(ped_df$id)
    is_mother <- ped_df$id %in% ped_df$momid & !is.na(ped_df$id)

    # Add missing sex due to parenthood
    ped_df$sex[is.na(ped_df$gender) & is_father] <- "male"
    ped_df$sex[is.na(ped_df$gender) & is_mother] <- "female"

    # Add terminated for sterilized individuals
    if ("steril" %in% colnames(ped_df)) {
        ped_df$sex[ped_df$steril == "TRUE" & !is.na(ped_df$steril) &
            !is_father & !is_mother] <- "terminated"
        ped_df$sexErrTer[ped_df$steril == "TRUE" & !is.na(ped_df$steril) &
            (is_father | is_mother)] <- "IsSterelisedButIsParent"
        nb_steril_parent <- nrow(ped_df[
            ped_df$sexErrTer == "IsSterelisedButIsParent" &
            !is.na(ped_df$sexErrTer), ])
        message(paste(nb_steril_parent, "steril corrected"))
        ped_df$steril[ped_df$steril == "TRUE" & !is.na(ped_df$steril) &
            (is_father | is_mother)] <- FALSE
    }

    # Check for gender proportion Doc: Errors2
    if (all(ped_df$sex %in% c("unknown", "terminated"))) {
        stop("Invalid values for 'sex'")
    } else if (mean(ped_df$sex == "unknown") > 0.25) {
        warning("More than 25% of the gender values are 'unknown'")
    }

    # Check error between sex and parentality
    ped_df$sexNA[!ped_df$sex %in%
        c("male", "female", "terminated")] <- "SexNotRecognise"
    ped_df$sexErrMoFa[is_father & is_mother] <- "IsMotherAndFather"
    ped_df$sexErrFa[is_father & ped_df$sex != "male"] <- "IsFatherButNotMale"
    ped_df$sexErrMo[is_mother &
        ped_df$sex != "female"] <- "IsMotherButNotFemale"

    # Unite all sex errors in one column
    ped_df <- tidyr::unite(ped_df, "sexError",
        c("sexNA", "sexErrMoFa", "sexErrMo", "sexErrFa", "sexErrTer"),
        na.rm = TRUE, sep = "_", remove = TRUE)
    ped_df$sexError[ped_df$sexError == ""] <- NA

    #### Id #### Check id type
    for (id in c("indId", "fatherId", "motherId", "familyId")) {
        if (!is.numeric(ped_df[[id]])) {
            ped_df[[id]] <- as.character(ped_df[[id]])
            if (length(grep("^ *$", ped_df[[id]])) > 0) {
                stop("A blank or empty string is not allowed as the ",
                    id, " variable")
            }
        }
    }

    # Make a new id from the family and subject pair
    pre_famid <- ped_df$familyId
    pre_famid[!is.na(pre_famid)] <- paste0(pre_famid[!is.na(pre_famid)], "_")
    pre_famid[is.na(pre_famid)] <- ""

    ped_df$id <- paste0(as.character(pre_famid), as.character(ped_df$indId))
    ped_df$dadid <- paste0(as.character(pre_famid),
        as.character(ped_df$fatherId))
    ped_df$momid <- paste0(as.character(pre_famid),
        as.character(ped_df$motherId))

    # Get duplicated id key
    id_duplicated <- ped_df$id[base::duplicated(ped_df$id)]

    ped_df$dadid[ped_df$dadid %in% id_duplicated] <- NA
    ped_df$momid[ped_df$momid %in% id_duplicated] <- NA

    # Set NA id to missid
    ped_df$id[ped_df$id == "NA" | is.na(ped_df$id)] <- missid
    ped_df$dadid[ped_df$dadid == "NA" | is.na(ped_df$dadid)] <- missid
    ped_df$momid[ped_df$momid == "NA" | is.na(ped_df$momid)] <- missid

    # Fix parents missing or wrong sex
    ped_df <- fixParents(ped_df)

    # OwnParent
    id_own_parent <- ped_df$id[ped_df$id == ped_df$dadid |
        ped_df$id == ped_df$momid]

    # Register errors
    ped_df$idErrFa[ped_df$dadid %in% id_duplicated &
        !is.na(ped_df$dadid)] <- "Fatherid_duplicated"
    ped_df$idErrMo[ped_df$momid %in% id_duplicated &
        !is.na(ped_df$momid)] <- "Motherid_duplicated"
    ped_df$idErrSelf[ped_df$id %in% id_duplicated &
        !is.na(ped_df$id)] <- "Selfid_duplicated"
    ped_df$idErrOwnParent[ped_df$id %in% id_own_parent] <- "IsItsOwnParent"
    ped_df$idErrBothParent[(ped_df$dadid == missid & ped_df$momid != missid) |
        (ped_df$dadid != missid & ped_df$momid == missid)] <- "IsItsOwnParent"

    # Unite all id errors in one column
    ped_df <- tidyr::unite(ped_df, "idError",
        c("idErrFa", "idErrMo", "idErrSelf",
            "idErrOwnParent", "idErrBothParent"),
        na.rm = TRUE, sep = "_", remove = TRUE)
    ped_df$idError[ped_df$idError == ""] <- NA

    #### Available ####
    if ("available" %in% colnames(ped_df)) {
        ped_df$avail[!is.na(ped_df$available) & ped_df$available != 0] <- 1
        ped_df$avail[is.na(ped_df$available) | ped_df$available == 0] <- 0
    } else {
        ped_df$avail <- NA
    }

    #### Status ####
    if ("status" %in% colnames(ped_df)) {
        ped_df$status[!is.na(ped_df$status) & ped_df$status != 0] <- 1
        ped_df$status[is.na(ped_df$status) | ped_df$status == 0] <- 0
    } else {
        ped_df$status <- NA
    }

    #### Family ####
    df_noerr <- ped_df[is.na(ped_df$idError), c("id", "dadid", "momid")]
    id <- df_noerr$id
    dadid <- df_noerr$dadid
    momid <- df_noerr$momid
    df_noerr$family <- with(df_noerr, makefamid(id, dadid, momid))
    ped_df <- merge(ped_df[!colnames(ped_df) == "family"],
        df_noerr[c("id", "family")], by = "id", all.x = TRUE)

    #### Convert to num ####
    if (try_num) {
        message("Converting to numeric if possible")
        col_to_num <- colnames(ped_df)[!colnames(ped_df) %in%
            c(cols_need, cols_to_use)]
        for (i in col_to_num) {
            is_num <- sapply(ped_df[[i]], check_num_na, na_as_num = TRUE)
            if (all(is_num)) {
                ped_df[i] <- as.numeric(ped_df[[i]])
            }
        }
    }

    ped_df$error[ped_df$idError != "" | ped_df$sexError != ""] <- 1

    ped_df
}

usethis::use_package("stringr")

norm_rel <- function(rel_df, na_strings = c("NA", ""), missid = "0") {
    print("Bal: norm_rel")
    #### Check columns ####
    err_cols <- c("codeErr", "sameIdErr", "idError", "id1Err", "id2Err")
    col_needed <- c("id1", "id2", "code")
    col_to_use <- c("family")
    rel_df <- check_columns(rel_df, col_needed, err_cols, col_to_use,
        others_cols = FALSE, cols_to_use_init = TRUE, cols_used_init = TRUE)
    if (nrow(rel_df) > 0) {
        rel_df <- dplyr::mutate_if(rel_df, is.character,
            ~replace(., . %in% na_strings, NA))

        #### Check for code ####
        code_equiv <- c(mztwin = "1", dztwin = "2", uztwin = "3", spouse = "4")
        rel$code <- as.character(plyr::revalue(as.factor(
            stringr::str_remove_all(
                casefold(as.character(rel$code), upper = FALSE),
            " ")), code_equiv))

        code_vali <- c("1", "2", "3", "4")
        rel_df$codeErr[!rel_df$code %in% code_vali] <- "CodeNotRecognise"

        #### Check for id errors #### Set ids as characters
        rel_df <- rel_df %>%
            mutate(across(c("id1", "id2"), as.character))

        ## Compute id with family id
        pre_famid <- rel_df$family
        pre_famid[!is.na(pre_famid)] <- paste0(pre_famid[!is.na(pre_famid)],
            "_")
        pre_famid[is.na(pre_famid)] <- ""

        rel_df$id1 <- paste0(pre_famid, rel_df$id1)
        rel_df$id2 <- paste0(pre_famid, rel_df$id2)

        rel_df$sameIdErr[rel_df$id1 == rel_df$id2] <- "SameId"
        len1 <- nchar(rel_df$id1)
        len2 <- nchar(rel_df$id2)
        rel_df$id1Err[is.na(len1) | len1 == missid] <- "Id1length0"
        rel_df$id2Err[is.na(len2) | len2 == missid] <- "Id2length0"

        # Unite all id errors in one column
        rel_df <- tidyr::unite(rel_df, "idError",
            c("sameIdErr", "id1Err", "id2Err"),
            na.rm = TRUE, sep = "_", remove = TRUE)
        rel_df$idError[rel_df$idError == ""] <- NA
        rel_df$error[rel_df$idError != ""] <- 1
    }
    rel_df
}
TRUE
