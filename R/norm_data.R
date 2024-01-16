#' @importFrom plyr revalue
#' @importFrom dplyr mutate %>% across mutate_at mutate_if
#' @importFrom tidyr unite
#' @importFrom stringr str_remove_all
NULL

#' Normalise a Ped object dataframe
#'
#' @description Normalise dataframe for a Ped object
#'
#' @details Normalise a dataframe and check for columns correspondance
#' to be able to use it as an input to create a Ped object.
#' Multiple test are done and errors are checked.
#' Sex is calculated based on the `gender` column.
#'
#' The `steril` column need to be a boolean either TRUE, FALSE or 'NA'.
#' Will be considered available any individual with no 'NA' values in the
#' `available` column.
#' Duplicated `indId` will nullify the relationship of the individual.
#' All individuals with errors will be remove from the dataframe and will
#' be transfered to the error dataframe.
#'
#' A number of checks are done to ensure the dataframe is correct:
#'
#' ## On identifiers:
#'    - All ids (id, dadid, momid, famid) are not empty (`!= ""`)
#'    - All `id` are unique (no duplicated)
#'    - All `dadid` and `momid` are unique in the id column (no duplicated)
#'    - id is not the same as dadid or momid
#'    - Either have both parents or none
#'
#' ## On sex
#'    - All sex code are either `male`, `female`, `terminated` or `unknown`.
#'    - No parents are steril
#'    - All fathers are male
#'    - All mothers are female
#'
#' @param ped_df A data.frame with the individuals informations.
#' The minimum columns required are:
#'
#'     - `indID` individual identifiers -> `id`
#'     - `fatherId` biological fathers identifiers -> `dadid`
#'     - `motherId` biological mothers identifiers -> `momdid`
#'     - `gender` sex of the individual -> `sex`
#'     - `family` family identifiers -> `famid`
#'
#' The `family` column, if provided, will be merged to the *ids* field
#' separated by an underscore using the [upd_famid_id()] function.
#'
#' The following columns are also recognize and will be transformed with the
#' [vect_to_binary()] function:
#'
#'     - `sterilisation` status -> `steril`
#'     - `available` status -> `avail`
#'     - `vitalStatus`, is the individual dead -> `status`
#'     - `affection` status -> `affected`
#'
#' The values recognized for those columns are `1` or `0`, `TRUE` or `FALSE`.
#' @param na_strings Vector of strings to be considered as NA values.
#' @param try_num Boolean defining if the function should try to convert
#' all the columns to numeric.
#' @inheritParams Ped
#'
#' @return A dataframe with different variable correctly standardized
#' and with the errors identified in the `error` column
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
#' tryCatch(
#'      norm_ped(df),
#'      error = function(e) print(e)
#' )
#'
#' @seealso [Ped()], [Ped-class], [Pedigree()]
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

    ped_df$family[is.na(ped_df$family)] <- missid

    if (nrow(ped_df) > 0) {
        ped_df <- mutate_if(
            ped_df, is.character, ~replace(., . %in% na_strings, NA_character_)
        )

        #### Id #### Check id type
        for (id in c("indId", "fatherId", "motherId")) {
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
        ped_df$id <- upd_famid_id(ped_df$indId, ped_df$famid, missid)
        ped_df$dadid <- upd_famid_id(ped_df$fatherId, ped_df$famid, missid)
        ped_df$momid <- upd_famid_id(ped_df$motherId, ped_df$famid, missid)

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
            ped_df$steril <- vect_to_binary(
                ped_df$sterilisation, logical = TRUE
            )
            ped_df$sex[
                ped_df$steril == 1 & !is.na(ped_df$steril) &
                    !is_father & !is_mother
            ] <- "terminated"
            err$sexErrTer[
                (ped_df$sex == "terminated" |
                        ped_df$steril == 1 & !is.na(ped_df$steril)
                )  & (is_father | is_mother)
            ] <- "isSterilButIsParent"
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

#' Normalise a Rel object dataframe
#'
#' @description Normalise a dataframe and check for columns correspondance
#' to be able to use it as an input to create a Ped object.
#'
#' @details
#' The `famid` column, if provided, will be merged to the *ids* field
#' separated by an underscore using the [upd_famid_id()] function.
#' The `code` column will be transformed with the [rel_code_to_factor()].
#' Multiple test are done and errors are checked.
#'
#' A number of checks are done to ensure the dataframe is correct:
#'
#' ## On identifiers:
#'    - All ids (id1, id2) are not empty (`!= ""`)
#'    - `id1` and `id2` are not the same
#'
#' ## On code
#'   - All code are recognised as either "MZ twin", "DZ twin", "UZ twin" or
#'  "Spouse"
#'
#' @inheritParams norm_ped
#' @inheritParams Pedigree
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

    if (is.matrix(rel_df)) {
        rel_df <- as.data.frame(rel_df)
        colnames(rel_df) <- c(
            "id1", "id2", "code", "famid"
        )[seq_len(ncol(rel_df))]
    }

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
    rel_df$famid[is.na(rel_df$famid)] <- missid
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
        rel_df$id1 <- upd_famid_id(rel_df$id1, rel_df$famid, missid)
        rel_df$id2 <- upd_famid_id(rel_df$id2, rel_df$famid, missid)

        rel_df <- mutate_at(rel_df, c("id1", "id2", "famid"),
            ~replace(., . %in% c(na_strings, missid), NA_character_)
        )

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
