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
#' norm_data(ped_data)
#'
#'@export norm_data
norm_data <- function(df, na_strings = "NA") {
  print("Bal: norm_data")
  err_cols <- c(
    "sexErrMoFa", "sexErrFa", "sexErrMo", "sexErrTer", "sexNA", "sexError",
    "idErrFa", "idErrMo", "idErrSelf", "idErrAncestor", "idError"
  )
  cols_need <- c("indId", "fatherId", "motherId", "gender")
  cols_used <- c("sex", "avail", "id", "dadid", "momid", err_cols)
  cols_to_use <- c("steril", "available")

  df <- check_columns(df, cols_need, cols_used, cols_to_use, others_cols = TRUE)

  df <- dplyr::mutate_if(df, is.character, ~replace(., . %in% na_strings, NA))

  #### Set as id position in df ####
  df$id <- seq_len(dim(df)[1])

  #### Initialise all errors columns ####
  df[err_cols] <- NA

  #### Sex ####
  # Normalized difference notations for sex
  sex_equiv <- c(
    "f" = "female", "m" = "male",
    "woman" = "female", "man" = "male",
    "female" = "female", "male" = "male",
    "2" = "female", "1" = "male"
  )
  df$sex <- as.character(plyr::revalue(
    as.factor(casefold(df$gender, upper = FALSE)),
    sex_equiv
  ))

  is_father <- df$indId %in% df$fatherId & !is.na(df$indId)
  is_mother <- df$indId %in% df$motherId & !is.na(df$indId)

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

  #### available####
  if ("available" %in% colnames(df)) {
    df$avail[!is.na(df$available)] <- 1
    df$avail[is.na(df$available)] <- 0
  } else {
    df$avail <- NA
  }

  #### ParentsId####
  # Get duplicated id key
  id_duplicated <- df$indId[base::duplicated(df$indId)]

  # Set id parents for individuals with no duplicated parents
  dadid <- df$id[match(df$fatherId, df$indId)]
  dadid[df$fatherId %in% id_duplicated] <- NA
  df$dadid <- dadid
  momid <- df$id[match(df$motherId, df$indId)]
  momid[df$motherId %in% id_duplicated] <- NA
  df$momid <- momid

  #### OwnAncestor####
  id_own_ancestor <- df$indId[df$indId == df$fatherId | df$indId == df$motherId]

  # Register errors
  df$idErrFa[df$fatherId %in% id_duplicated &
    !is.na(df$fatherId)] <- "Fatherid_duplicated"
  df$idErrMo[df$motherId %in% id_duplicated &
    !is.na(df$motherId)] <- "Motherid_duplicated"
  df$idErrSelf[df$indId %in% id_duplicated &
    !is.na(df$indId)] <- "Selfid_duplicated"
  df$idErrAncestor[df$indId %in% id_own_ancestor] <- "IsItsOwnAncestor"

  # Unite all id errors in one column
  df <- tidyr::unite(df, "idError",
    c("idErrFa", "idErrMo", "idErrSelf", "idErrAncestor"),
    na.rm = TRUE, sep = "_", remove = TRUE
  )

  #### Report####
  errors <- df[df$idError != "" | df$sexError != "", ]

  # Deletion of all individuals with errors
  df <- df[df$idError == "" & df$sexError == "", ]

  #### Convert to num ####
  message("Converting to numeric if possible")
  col_to_num <- colnames(df)[!colnames(df) %in% c(cols_need, cols_to_use)]
  for (i in col_to_num) {
    is_num <- sapply(df[[i]], check_num_na, na_as_num = TRUE)
    if (all(is_num)) {
      df[i] <- as.numeric(df[[i]])
    }
  }
  list(norm = df, errors = errors)
}



norm_rel <- function(df){
  print("Bal: norm_rel")
  #### Check columns ####
  err_cols <- c(
    "codeErr", "sameIdErr", "idError", "id1Err", "id2Err"
  )
  col_needed <- c("id1", "id2", "code")
  col_to_use <- c("family")
  df <- check_columns(df, col_needed, err_cols, col_to_use,
    others_cols = FALSE)

  #### Check for code erros ####
  df$code <- as.character(df$code)
  code_vali <- c("1", "2", "3", "4")
  df$codeErr[!df$code %in% code_vali] <- "CodeNotRecognise"

  #### Check for id erros ####
  df[c("id1", "id2")] <- as.character(df[c("id1", "id2")])
  df$sameIdErr[df$id1 == df$id2] <- "SameId"
  len1 <- nchar(df$id1)
  len2 <- nchar(df$id2)
  df$id1Err[is.na(len1) | len1 == 0] <- "Id1length0"
  df$id2Err[is.na(len2) | len2 == 0] <- "Id2length0"

  # Unite all id errors in one column
  df <- tidyr::unite(df, "idError",
    c("sameIdErr", "id1Err", "id2Err"),
    na.rm = TRUE, sep = "_", remove = TRUE
  )

  #### Report####
  errors <- df[df$idError != "" | df$sexError != "", ]

  # Deletion of all individuals with errors
  df <- df[df$idError == "" & df$codeErr == "", ]

  list(norm = df, errors = errors)
}
