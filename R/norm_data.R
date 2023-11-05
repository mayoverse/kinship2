#' @importFrom plyr revalue
#' @importFrom dplyr mutate %>% across mutate_at mutate_if
#' @importFrom tidyr unite
#' @importFrom stringr str_remove_all
NULL

#' Compute id with family id
#'
#' @description Compute id with family id if the family id available
#'
#' @inheritParams Ped
#' @inheritParams is_parent
#' @keywords internal
#' @return The id with the family id merged
prefix_famid <- function(famid, id, missid = NA_character_) {
    if (length(famid) > 1 && length(famid) != length(id)) {
        stop("famid and id must have the same length.")
    }

    pre_famid <- ifelse(
        is.na(famid) | is.null(famid),
        "", paste0(as.character(famid), "_")
    )
    ifelse(id %in% missid, missid, paste0(pre_famid, as.character(id)))
}

#' Normalise dataframe
#'
#' @description Normalise dataframe for Pedigree object
#'
#' @details Normalise a dataframe and check for columns correspondance
#' to be able to use it as an input to create Pedigree object.
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
#' @param ped_df A data.frame with the individuals informations.
#' The minimum columns required are `indID`, `fatherId`, `motherId` and
#' `gender`.
#' The `famid` column can also be used to specify the family of the
#' individuals and will be merge to the `id` field separated by an
#' underscore.
#' The following columns are also recognize `sterilisation`, `available`,
#' `vitalStatus`, `affection`. The four of them will be transformed with the
#' [vect_to_binary()] function.
#' They respectively correspond to the sterilisation status,
#' the availability status, the death status and the affection status
#' of the individuals. The values recognized for those columns are `1` or
#' `0`.
#' @param na_strings Vector of strings to be considered as NA values
#' @param try_num Boolean defining if the function should try to convert
#' all the columns to numeric.
#' @inheritParams is_parent
#'
#' @return A dataframe with the errors identified in the `error` column
#'
#' @include utils.R
#' @examples
#' df <- data.frame(
#'     indId = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
#'     fatherId = c("A", 0, 1, 3, 0, 4, 1, 0, 6, 6),
#'     motherId = c(0, 0, 2, 2, 0, 5, 2, 0, 8, 8),
#'     gender = c(1, 2, "m", "man", "f", "male", "m", "m", "f", "f"),
#'     available = c("A", "1", 0, NA, 1, 0, 1, 0, 1, 0),
#'     famid = c(1, 1, 1, 1, 1, 1, 1, 2, 2, 2),
#'     sterilisation = c("TRUE", "FALSE", TRUE, FALSE, 1, 0, 1, 0, 1, "TRUE"),
#'     vitalStatus = c("TRUE", "FALSE", TRUE, FALSE, 1, 0, 1, 0, 1, 0),
#'     affection = c("TRUE", "FALSE", TRUE, FALSE, 1, 0, 1, 0, 1, 0)
#' )
#' norm_ped(df)
#' @export
norm_ped <- function(
    ped_df, na_strings = c("NA", ""), missid = NA_character_, try_num = FALSE
) {
    err_cols <- c(
        "sexErrMoFa", "sexErrFa", "sexErrMo", "sexErrTer", "sexNA",
        "sexError", "idErr", "idErrFa", "idErrMo", "idErrSelf",
        "idErrOwnParent", "idErrBothParent", "idError", "error"
    )
    err <- data.frame(matrix(NA, nrow = nrow(ped_df), ncol = length(err_cols)))
    colnames(err) <- err_cols
    cols_need <- c("indId", "fatherId", "motherId", "gender")
    cols_used <- c(
        "sex", "steril", "status", "avail", "id", "dadid", "momid", "famid",
        "error", "affected"
    )
    cols_to_use <- c(
        "available", "family", "sterilisation", "vitalStatus", "affection"
    )
    ped_df <- check_columns(
        ped_df, cols_need, cols_used, cols_to_use,
        others_cols = TRUE, cols_to_use_init = TRUE, cols_used_init = TRUE
    )
    if (nrow(ped_df) > 0) {
        ped_df <- mutate_if(
            ped_df, is.character, ~replace(., . %in% na_strings, NA)
        )

        #### Id #### Check id type
        for (id in c("indId", "fatherId", "motherId", "family")) {
            ped_df[[id]] <- as.character(ped_df[[id]])
        }
        err$idErr <- lapply(
            as.data.frame(t(ped_df[, c(
                "indId", "fatherId", "motherId", "family"
            )])),
            function(x) {
                if (any(x == "" & !is.na(x))) {
                    "One id is Empty"
                } else {
                    NA_character_
                }
            }
        )
        ## Make a new id from the family and subject pair
        ped_df$famid <- ped_df$family
        ped_df$id <- prefix_famid(ped_df$famid, ped_df$indId, missid)
        ped_df$dadid <- prefix_famid(ped_df$famid, ped_df$fatherId, missid)
        ped_df$momid <- prefix_famid(ped_df$famid, ped_df$motherId, missid)

        ped_df <- mutate_at(ped_df, c("id", "dadid", "momid"),
            ~replace(., . %in% c(na_strings, missid), NA_character_)
        )

        #### Sex ####
        ped_df$sex <- sex_to_factor(ped_df$gender)

        is_father <- ped_df$id %in% ped_df$dadid & !is.na(ped_df$id)
        is_mother <- ped_df$id %in% ped_df$momid & !is.na(ped_df$id)

        ## Add missing sex due to parenthood
        ped_df$sex[is_father] <- "male"
        ped_df$sex[is_mother] <- "female"

        ## Add terminated for sterilized individuals that is neither dad nor mom
        if ("sterilisation" %in% colnames(ped_df)) {
            ped_df$steril <- vect_to_binary(ped_df$sterilisation, logical = TRUE)
            ped_df$sex[
                ped_df$steril == 1 & !is.na(ped_df$steril) &
                    !is_father & !is_mother
            ] <- "terminated"
            err$sexErrTer[
                (ped_df$sex == "terminated" |
                        ped_df$steril == 1 & !is.na(ped_df$steril)
                )  & (is_father | is_mother)
            ] <- "isSterilButIsParent"
            nb_steril_parent <- sum(!is.na(err$sexErrTer))
            ped_df$steril[!is.na(err$sexErrTer) &
                    (is_father | is_mother)
            ] <- FALSE
        }

        ## Check error between sex and parentality
        err$sexNA[!ped_df$sex %in%
                c("male", "female", "terminated", "unknown")
        ] <- "sexNotRecognise"
        err$sexErrMoFa[is_father & is_mother] <- "isMotherAndFather"
        err$sexErrFa[
            is_father & ped_df$sex != "male"
        ] <- "isFatherButNotMale"
        err$sexErrMo[is_mother &
                ped_df$sex != "female"
        ] <- "isMotherButNotFemale"

        ## Unite all sex errors in one column
        err <- unite(
            err, "sexError",
            c("sexNA", "sexErrMoFa", "sexErrMo", "sexErrFa", "sexErrTer"),
            na.rm = TRUE, sep = "_", remove = TRUE
        )
        err$sexError[err$sexError == ""] <- NA


        #### Continue to check id #####
        ## Get duplicated id key
        id_duplicated <- ped_df$id[base::duplicated(ped_df$id)]

        ## OwnParent
        id_own_parent <- ped_df$id[
            ped_df$id == ped_df$dadid | ped_df$id == ped_df$momid
        ]

        ## Register errors
        err$idErrFa[ped_df$dadid %in% id_duplicated &
                !is.na(ped_df$dadid)
        ] <- "fatherIdDuplicated"
        err$idErrMo[ped_df$momid %in% id_duplicated &
                !is.na(ped_df$momid)
        ] <- "motherIdDuplicated"
        err$idErrSelf[ped_df$id %in% id_duplicated &
                !is.na(ped_df$id)
        ] <- "selfIdDuplicated"
        err$idErrOwnParent[ped_df$id %in% id_own_parent] <- "isItsOwnParent"
        err$idErrBothParent[
            (ped_df$dadid %in% missid & (!ped_df$momid %in% missid)) |
                ((!ped_df$dadid %in% missid) & ped_df$momid %in% missid)
        ] <- "oneParentMissing"

        ## Unite all id errors in one column
        err <- unite(
            err, "idError", c(
                "idErr", "idErrFa", "idErrMo", "idErrSelf",
                "idErrOwnParent", "idErrBothParent"
            ), na.rm = TRUE, sep = "_", remove = TRUE
        )
        err$idError[err$idError == ""] <- NA

        #### Available ####
        if ("available" %in% colnames(ped_df)) {
            ped_df$avail <- vect_to_binary(ped_df$available, logical = TRUE)
        }
        #### Status ####
        if ("vitalStatus" %in% colnames(ped_df)) {
            ped_df$status <- vect_to_binary(ped_df$vitalStatus, logical = TRUE)
        }
        #### Affected ####
        if ("affection" %in% colnames(ped_df)) {
            ped_df$affected <- vect_to_binary(ped_df$affection, logical = TRUE)
        }

        #### Convert to num ####
        if (try_num) {
            col_to_num <- colnames(ped_df)[
                !colnames(ped_df) %in%
                    c(cols_need, cols_to_use)
            ]
            for (i in col_to_num) {
                is_num <- lapply(ped_df[[i]], check_num_na, na_as_num = TRUE)
                if (all(is_num)) {
                    ped_df[i] <- as.numeric(ped_df[[i]])
                }
            }
        }

        ped_df$error <- unite(
            err, "error", c("idError", "sexError"),
            na.rm = TRUE, sep = "_", remove = TRUE
        )$error
        ped_df$error[ped_df$error == ""] <- NA
    }
    ped_df
}

#' Normalise relationship dataframe
#'
#' @description Normalise relationship dataframe for Pedigree object
#'
#' @inheritParams norm_ped
#' @inheritParams Pedigree
#' @inheritParams is_parent
#'
#' @examples
#' df <- data.frame(
#'    id1 = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
#'    id2 = c(2, 3, 4, 5, 6, 7, 8, 9, 10, 1),
#'    code = c("MZ twin", "DZ twin", "UZ twin", "Spouse", 1, 2,
#'       3, 4, "MzTwin", "sp oUse"),
#'    famid = c(1, 1, 1, 1, 1, 1, 1, 2, 2, 2)
#' )
#' norm_rel(df)
#'
#' @return A dataframe with the errors identified
#' @export
norm_rel <- function(rel_df, na_strings = c("NA", ""), missid = NA_character_) {
    #### Check columns ####
    err_cols <- c("codeErr", "sameIdErr", "id1Err", "id2Err", "error")
    err <- data.frame(matrix(NA, nrow = nrow(rel_df), ncol = length(err_cols)))
    colnames(err) <- err_cols
    cols_needed <- c("id1", "id2", "code")
    cols_used <- c("error")
    cols_to_use <- c("famid")
    rel_df <- check_columns(
        rel_df, cols_needed, cols_used, cols_to_use,
        others_cols = FALSE, cols_to_use_init = TRUE, cols_used_init = TRUE
    )
    if (nrow(rel_df) > 0) {
        rel_df <- mutate_if(
            rel_df, is.character,
            ~replace(., . %in% na_strings, NA)
        )

        #### Check for code ####
        rel_df$code <- rel_code_to_factor(rel_df$code)
        err$codeErr[!rel_df$code %in%
                c("MZ twin", "DZ twin", "UZ twin", "Spouse")
        ] <- "CodeNotRecognise"

        #### Check for id errors #### Set ids as characters
        rel_df <- rel_df %>%
            mutate(across(c("id1", "id2", "famid"), as.character))

        ## Check for non null ids
        len1 <- nchar(rel_df$id1)
        len2 <- nchar(rel_df$id2)
        err$id1Err[is.na(len1) | len1 %in% missid] <- "indId1length0"
        err$id2Err[is.na(len2) | len2 %in% missid] <- "indId2length0"

        ## Compute id with family id
        rel_df$id1 <- prefix_famid(rel_df$famid, rel_df$id1, missid)
        rel_df$id2 <- prefix_famid(rel_df$famid, rel_df$id2, missid)

        err$sameIdErr[rel_df$id1 == rel_df$id2] <- "SameId"

        ## Unite all id errors in one column
        rel_df$error <- unite(err, "error",
            c("sameIdErr", "id1Err", "id2Err", "codeErr"),
            na.rm = TRUE, sep = "_", remove = TRUE
        )$error
        rel_df$error[rel_df$error == ""] <- NA
    }
    rel_df
}
