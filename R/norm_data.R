#' @importFrom plyr revalue
#' @importFrom dplyr mutate %>% across mutate_at mutate_if
#' @importFrom tidyr unite
#' @importFrom stringr str_remove_all
NULL

#' Compute id with family id
#'
#' @description Compute id with family id if the family id available
#'
#' @param family_id The family id
#' @param ind_id The individual id
#' @inheritParams is_parent
#'
#' @return The id with the family id merged
#' @export
prefix_famid <- function(family_id, ind_id, missid = "0") {
    if (length(family_id) > 1 && length(family_id) != length(ind_id)) {
        stop("family_id and ind_id must have the same length.")
    }

    pre_famid <- ifelse(
        is.na(family_id) | is.null(family_id),
        "", paste0(as.character(family_id), "_")
    )
    ifelse(ind_id == missid, missid, paste0(pre_famid, as.character(ind_id)))
}

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
#' @param ped_df A data.frame with the individuals informations.
#' The minimum columns required are `id`, `dadid`, `momid` and `sex`.
#' The `family` column can also be used to specify the family of the
#' individuals and will be merge to the `id` field separated by an
#' underscore.
#' The following columns are also recognize `steril`, `avail`, `status`,
#' `affected`. They respectively correspond to the sterilisation status,
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
#' @export
#' @include utils.R
norm_ped <- function(
    ped_df, na_strings = c("NA", ""), missid = "0", try_num = FALSE
) {
    err_cols <- c(
        "sexErrMoFa", "sexErrFa", "sexErrMo", "sexErrTer", "sexNA",
        "sexError", "idErrFa", "idErrMo", "idErrSelf", "idErrOwnParent",
        "idErrBothParent", "idError", "error"
    )
    err <- data.frame(matrix(NA, nrow = nrow(ped_df), ncol = length(err_cols)))
    colnames(err) <- err_cols
    cols_need <- c("indId", "fatherId", "motherId", "gender")
    cols_used <- c("sex", "avail", "id", "dadid", "momid", "error")
    cols_to_use <- c("steril", "available", "family", "status", "affected")

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
            if (!is.numeric(ped_df[[id]])) {
                ped_df[[id]] <- as.character(ped_df[[id]])
                if (length(grep("^ *$", ped_df[[id]])) > 0) {
                    stop(
                        "A blank or empty string is not allowed as the ",
                        id, " variable"
                    )
                }
            }
        }

        ## Make a new id from the family and subject pair
        ped_df$id <- prefix_famid(ped_df$family, ped_df$indId, missid)
        ped_df$dadid <- prefix_famid(ped_df$family, ped_df$fatherId, missid)
        ped_df$momid <- prefix_famid(ped_df$family, ped_df$motherId, missid)

        ped_df <- mutate_at(ped_df, c("id", "dadid", "momid"),
            ~replace(., . %in% na_strings, missid)
        )

        #### Sex ####
        ped_df$sex <- sex_to_factor(ped_df$gender)

        is_father <- ped_df$id %in% ped_df$dadid & !is.na(ped_df$id)
        is_mother <- ped_df$id %in% ped_df$momid & !is.na(ped_df$id)

        ## Add missing sex due to parenthood
        ped_df$sex[is.na(ped_df$gender) & is_father] <- "male"
        ped_df$sex[is.na(ped_df$gender) & is_mother] <- "female"

        ## Add terminated for sterilized individuals that is neither dad nor mom
        if ("steril" %in% colnames(ped_df)) {
            ped_df$steril <- as.logical(ped_df$steril)
            ped_df$sex[
                ped_df$steril == "TRUE" & !is.na(ped_df$steril) &
                    !is_father & !is_mother
            ] <- "terminated"
            err$sexErrTer[
                (ped_df$sex == "terminated" |
                        ped_df$steril == "TRUE" & !is.na(ped_df$steril)
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
            (ped_df$dadid == missid & ped_df$momid != missid) |
                (ped_df$dadid != missid & ped_df$momid == missid)
        ] <- "oneParentMissing"

        ## Unite all id errors in one column
        err <- unite(
            err, "idError", c(
                "idErrFa", "idErrMo", "idErrSelf",
                "idErrOwnParent", "idErrBothParent"
            ), na.rm = TRUE, sep = "_", remove = TRUE
        )
        err$idError[err$idError == ""] <- NA

        #### Available ####
        if ("available" %in% colnames(ped_df)) {
            ped_df$avail <- as.numeric(ped_df$available)
            ped_df$avail[!is.na(ped_df$available) & ped_df$available != 0] <- 1
            ped_df$avail[!is.na(ped_df$available) & ped_df$available == 0] <- 0
        }
        #### Status ####
        if ("status" %in% colnames(ped_df)) {
            ped_df$status[!is.na(ped_df$status) & ped_df$status != 0] <- 1
            ped_df$status[is.na(ped_df$status) | ped_df$status == 0] <- 0
        }

        #### Convert to num ####
        if (try_num) {
            col_to_num <- colnames(ped_df)[
                !colnames(ped_df) %in%
                    c(cols_need, cols_to_use)
            ]
            for (i in col_to_num) {
                is_num <- sapply(ped_df[[i]], check_num_na, na_as_num = TRUE)
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
#' @description Normalise relationship dataframe for pedigree object
#'
#' @inheritParams norm_ped
#' @inheritParams pedigree
#' @inheritParams is_parent
#'
#' @return A dataframe with the errors identified
#' @export
norm_rel <- function(rel_df, na_strings = c("NA", ""), missid = "0") {
    #### Check columns ####
    err_cols <- c("codeErr", "sameIdErr", "id1Err", "id2Err", "error")
    err <- data.frame(matrix(NA, nrow = nrow(rel_df), ncol = length(err_cols)))
    colnames(err) <- err_cols
    cols_needed <- c("indId1", "indId2", "code")
    cols_used <- c("id1", "id2", "error")
    cols_to_use <- c("family")
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
            mutate(across(c("indId1", "indId2"), as.character))

        ## Check for non null ids
        len1 <- nchar(rel_df$indId1)
        len2 <- nchar(rel_df$indId2)
        err$id1Err[is.na(len1) | len1 == missid] <- "indId1length0"
        err$id2Err[is.na(len2) | len2 == missid] <- "indId2length0"

        ## Compute id with family id
        rel_df$id1 <- prefix_famid(rel_df$family, rel_df$indId1, missid)
        rel_df$id2 <- prefix_famid(rel_df$family, rel_df$indId2, missid)

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
