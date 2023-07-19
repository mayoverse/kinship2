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
#' The `steril` column need to be a boolean either TRUE, FALSE or "NA".
#' Will be considered available any individual with no "NA" values in the
#' `available` column.
#' Duplicated `indId` will nullify the relationship of the individual.
#' All individuals with errors will be remove from the dataframe and will
#' be transfered to the error dataframe.
#'
#' @param df The dataframe to process
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
norm_ped <- function(
  df, na_strings = c("NA", ""), missid = "0", try_num = FALSE) {
  print("Bal: norm_ped")
  err_cols <- c(
    "sexErrMoFa", "sexErrFa", "sexErrMo", "sexErrTer", "sexNA", "sexError",
    "idErrFa", "idErrMo", "idErrSelf", "idErrParent", "idError", "error"
  )
  cols_need <- c("indId", "fatherId", "motherId", "gender")
  cols_used <- c("sex", "avail", "id", "dadid", "momid", err_cols, "family")
  cols_to_use <- c("steril", "available", "familyId", "status")

  df <- check_columns(df, cols_need, cols_used, cols_to_use,
    others_cols = TRUE, cols_to_use_init = TRUE, cols_used_init = TRUE)

  df <- dplyr::mutate_if(df, is.character, ~replace(., . %in% na_strings, NA))

  #### Id ####
  # Check id type
  for (id in c("indId", "fatherId", "motherId", "familyId")) {
    if (!is.numeric(df[[id]])) {
      df[[id]] <- as.character(df[[id]])
      if (length(grep("^ *$", df[[id]])) > 0) {
        stop("A blank or empty string is not allowed as the ", id, " variable")
      }
    }
  }

  # Make a new id from the family and subject pair
  pre_famid <- df$familyId
  pre_famid[!is.na(pre_famid)] <- paste0(pre_famid[!is.na(pre_famid)], "_")
  pre_famid[is.na(pre_famid)] <- ""

  df$id <- paste0(as.character(pre_famid), as.character(df$indId))
  df$dadid <- paste0(as.character(pre_famid), as.character(df$fatherId))
  df$momid <- paste0(as.character(pre_famid), as.character(df$motherId))

  # Get duplicated id key
  id_duplicated <- df$id[base::duplicated(df$id)]

  df$dadid[df$dadid %in% id_duplicated] <- NA
  df$momid[df$momid %in% id_duplicated] <- NA

  # Set NA id to missid
  df$id[df$id == "NA"] <- missid
  df$dadid[df$dadid == "NA"] <- missid
  df$momid[df$momid == "NA"] <- missid

  #### OwnParent####
  id_own_parent <- df$id[df$id == df$dadid | df$id == df$momid]

  # Register errors
  df$idErrFa[df$dadid %in% id_duplicated &
    !is.na(df$dadid)] <- "Fatherid_duplicated"
  df$idErrMo[df$momid %in% id_duplicated &
    !is.na(df$momid)] <- "Motherid_duplicated"
  df$idErrSelf[df$id %in% id_duplicated &
    !is.na(df$id)] <- "Selfid_duplicated"
  df$idErrParent[df$id %in% id_own_parent] <- "IsItsOwnParent"

  # Unite all id errors in one column
  df <- tidyr::unite(df, "idError",
    c("idErrFa", "idErrMo", "idErrSelf", "idErrParent"),
    na.rm = TRUE, sep = "_", remove = TRUE
  )
  df$idError[df$idError == ""] <- NA

  #### Sex ####
  if (is.factor(df$gender) | is.numeric(df$gender)) {
    df$gender <- as.character(df$gender)
  }
  # Normalized difference notations for sex
  sex_equiv <- c(
    "f" = "female", "m" = "male",
    "woman" = "female", "man" = "male",
    "female" = "female", "male" = "male",
    "2" = "female", "1" = "male", "3" = "unknown",
    "4" = "terminated"
  )
  df$sex <- as.character(plyr::revalue(
    as.factor(casefold(df$gender, upper = FALSE)),
    sex_equiv
  ))
  sex_codes <- c("male", "female", "unknown", "terminated")
  df$sex[!df$sex %in% sex_codes] <- "unknown"

  is_father <- df$id %in% df$dadid & !is.na(df$id)
  is_mother <- df$id %in% df$momid & !is.na(df$id)

  # Add missing sex due to parenthood
  df$sex[is.na(df$gender) & is_father] <- "male"
  df$sex[is.na(df$gender) & is_mother] <- "female"

  # Add terminated for sterilized individuals
  if ("steril" %in% colnames(df)) {
    df$sex[df$steril == "TRUE" & !is.na(df$steril) &
      !is_father & !is_mother] <- "terminated"
    df$sexErrTer[df$steril == "TRUE" & !is.na(df$steril) &
      (is_father | is_mother)] <- "IsSterelisedButIsParent"
    nb_steril_parent <- nrow(df[df$sexErrTer == "IsSterelisedButIsParent" &
      !is.na(df$sexErrTer), ])
    message(paste(nb_steril_parent, "steril corrected"))
    df$steril[df$steril == "TRUE" & !is.na(df$steril) &
      (is_father | is_mother)] <- FALSE
  }

  # Check for gender proportion
  ## Doc:  Errors2
  if (all(df$sex %in% c("unknown", "terminated"))) {
    stop("Invalid values for 'sex'")
  } else if (mean(df$sex == "unknown") > 0.25) {
    warning("More than 25% of the gender values are 'unknown'")
  }

  # Check error between sex and parentality
  df$sexNA[!df$sex %in% c("male", "female", "terminated")] <- "SexNotRecognise"
  df$sexErrMoFa[is_father & is_mother] <- "IsMotherAndFather"
  df$sexErrFa[is_father & df$sex != "male"] <- "IsFatherButNotMale"
  df$sexErrMo[is_mother & df$sex != "female"] <- "IsMotherButNotFemale"

  # Unite all sex errors in one column
  df <- tidyr::unite(df, "sexError",
    c("sexNA", "sexErrMoFa", "sexErrMo", "sexErrFa", "sexErrTer"),
    na.rm = TRUE, sep = "_", remove = TRUE
  )
  df$sexError[df$sexError == ""] <- NA
  #### Available ####
  if ("available" %in% colnames(df)) {
    df$avail[!is.na(df$available) &  df$available != 0] <- 1
    df$avail[is.na(df$available) | df$available == 0] <- 0
  } else {
    df$avail <- NA
  }

  #### Status ####
  if ("status" %in% colnames(df)) {
    df$status[!is.na(df$status) &  df$status != 0] <- 1
    df$status[is.na(df$status) | df$status == 0] <- 0
  } else {
    df$status <- NA
  }

  #### Family ####
  df_noerr <- df[is.na(df$idError), c("id", "dadid", "momid")]
  id <- df_noerr$id
  dadid <- df_noerr$dadid
  momid <- df_noerr$momid
  df_noerr$family <- with(df_noerr, makefamid(id, dadid, momid))
  df <- merge(
    df[!colnames(df) == "family"],
    df_noerr[c("id", "family")],
    by = "id", all.x = TRUE)

  #### Convert to num ####
  if (try_num) {
    message("Converting to numeric if possible")
    col_to_num <- colnames(df)[!colnames(df) %in% c(cols_need, cols_to_use)]
    for (i in col_to_num) {
      is_num <- sapply(df[[i]], check_num_na, na_as_num = TRUE)
      if (all(is_num)) {
        df[i] <- as.numeric(df[[i]])
      }
    }
  }

  df$error[df$idError != "" | df$sexError != ""] <- 1

  df
}



norm_rel <- function(df, na_strings = c("NA", ""), missid = "0") {
  print("Bal: norm_rel")
  #### Check columns ####
  err_cols <- c(
    "codeErr", "sameIdErr", "idError", "id1Err", "id2Err"
  )
  col_needed <- c("id1", "id2", "code")
  col_to_use <- c("family")
  df <- check_columns(df, col_needed, err_cols, col_to_use,
    others_cols = FALSE)

  df <- dplyr::mutate_if(df, is.character, ~replace(., . %in% na_strings, NA))

  #### Check for code erros ####
  df$code <- as.character(df$code)
  code_vali <- c("1", "2", "3", "4")
  df$codeErr[!df$code %in% code_vali] <- "CodeNotRecognise"

  #### Check for id errors ####
  ## Set ids as characters
  df[c("id1", "id2")] <- as.character(df[c("id1", "id2")])

  ## Compute id with family id
  pre_famid <- df$family
  pre_famid[!is.na(pre_famid)] <- paste0(pre_famid[!is.na(pre_famid)], "_")
  pre_famid[is.na(pre_famid)] <- ""

  df$id1 <- paste0(pre_famid, df$id1)
  df$id2 <- paste0(pre_famid, df$id2)

  df$sameIdErr[df$id1 == df$id2] <- "SameId"
  len1 <- nchar(df$id1)
  len2 <- nchar(df$id2)
  df$id1Err[is.na(len1) | len1 == missid] <- "Id1length0"
  df$id2Err[is.na(len2) | len2 == missid] <- "Id2length0"

  # Unite all id errors in one column
  df <- tidyr::unite(df, "idError",
    c("sameIdErr", "id1Err", "id2Err"),
    na.rm = TRUE, sep = "_", remove = TRUE
  )
  df$idError[df$idError == ""] <- NA
  df$error[df$idError != ""] <- 1
  df
}
