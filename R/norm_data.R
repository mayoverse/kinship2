usethis::use_package("dplyr")
## Normalize the date for the library and get the errors
#' Normalise dataframe
#'
#' @description Normalise dataframe for pedigree object
#'
#' @details Normalise a dataframe and check for columns correspondance
#' to be able to use it as an input to create pedigree object.
#' Multiple test are done and errors are checked.
#' Sex is calculated based in the `Gender` column the following notations
#' are accepted: f, woman, female, 2 and m, man, male, 1.
#' The `Sterilisation` column need to be a boolean either TRUE, FALSE or "NA".
#' Will be considered available any individual with no "NA" values in the
#' `Availability` column.
#' Duplicated `IndID` will nullify the relationship of the individual.
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
  err_cols <- c(
    "sexErrMoFa", "sexErrFa", "sexErrMo", "sexErrTer","sexNA", "sexError",
    "idErrFa", "idErrMo", "idErrSelf", "idErrAncestor", "idError"
  )
  cols_need <- c("IndID", "FatherID", "MotherID", "Gender")
  cols_used <- c("sex", "avail", "id", "dadid", "momid", err_cols)
  cols_to_use <- c("Sterilisation", "Availability")

  df <- check_columns(df, cols_need, cols_used, cols_to_use, others_cols = TRUE)

  df <- dplyr::mutate_if(df, is.character, ~replace(., . %in% na_strings, NA))

  #### Set as id position in df ####
  df$id <- seq_len(dim(df)[1])

  #### Sex ####
  # Normalized difference notations for sex
  sex_equiv <- c(
    "f" = "female", "m" = "male",
    "woman" = "female", "man" = "male",
    "female" = "female", "male" = "male",
    "2" = "female", "1" = "male"
  )
  df$sex <- as.character(plyr::revalue(
    as.factor(casefold(df$Gender, upper = FALSE)),
    sex_equiv
    ))

  is_father <- df$IndID %in% df$FatherID & !is.na(df$IndID)
  is_mother <- df$IndID %in% df$MotherID & !is.na(df$IndID)

  # Add missing sex due to parenthood
  df$sex[is.na(df$Gender) & is_father] <- "male"
  df$sex[is.na(df$Gender) & is_mother] <- "female"

  # Add terminated for sterilized individuals
  if ("Sterilisation" %in% colnames(df)) {
    df$sex[df$Sterilisation == "TRUE" & !is.na(df$Sterilisation) &
      !is_father & !is_mother] <- "terminated"
    df$sexErrTer[df$Sterilisation == "TRUE" & !is.na(df$Sterilisation) &
      (is_father | is_mother)] <- "IsSterelisedButIsParent"
    nb_steril_parent <- nrow(df[df$sexErrTer == "IsSterelisedButIsParent" &
      !is.na(df$sexErrTer), ])
    message(paste(nb_steril_parent, "sterilisation corrected"))
    df$Sterilisation[df$Sterilisation == "TRUE" & !is.na(df$Sterilisation) &
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

  #### Availability####
  df$avail[!is.na(df$Availability)] <- 1
  df$avail[is.na(df$Availability)] <- 0

  #### ParentsId####
  # Get duplicated id key
  id_duplicated <- df$IndID[duplicated(df$IndID)]

  # Set id parents for individuals with no duplicated parents
  dadid <- df$id[match(df$FatherID, df$IndID)]
  dadid[df$FatherID %in% id_duplicated] <- NA
  df$dadid <- dadid
  momid <- df$id[match(df$MotherID, df$IndID)]
  momid[df$MotherID %in% id_duplicated] <- NA
  df$momid <- momid

  #### OwnAncestor####
  id_own_ancestor <- df$IndID[df$IndID == df$FatherID | df$IndID == df$MotherID]

  # Register errors
  df$idErrFa[df$FatherID %in% id_duplicated &
    !is.na(df$FatherID)] <- "Fatherid_duplicated"
  df$idErrMo[df$MotherID %in% id_duplicated &
    !is.na(df$MotherID)] <- "Motherid_duplicated"
  df$idErrSelf[df$IndID %in% id_duplicated &
    !is.na(df$IndID)] <- "Selfid_duplicated"
  df$idErrAncestor[df$IndID %in% id_own_ancestor] <- "IsItsOwnAncestor"

  # Unite all id errors in one column
  df <- tidyr::unite(df, "idError",
    c("idErrFa", "idErrMo", "idErrSelf", "idErrAncestor"),
    na.rm = TRUE, sep = "_", remove = TRUE
  )

  #### Report####
  errors <- df[df$idError != "" | df$sexError != "", ]

  # Deletion of all individuals with errors
  df <- df[df$idError == "" & df$sexError == "", ]

  #### Convert to num####
  col_to_num <- colnames(df)[!colnames(df) %in% c(cols_need, cols_to_use)]
  for (i in col_to_num) {
    is_num <- sapply(df[[i]], check_num_na, na_as_num = TRUE)
    if (all(is_num)) {
      df[i] <- as.numeric(df[[i]])
    }
  }
  list(norm = df, errors = errors)
}
