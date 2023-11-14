#' @importFrom dplyr group_by summarise mutate rename left_join rowwise join_by
#' @importFrom tidyr pivot_longer
NULL

#' Number of childs
#'
#' @description Compute the number of childs per individual
#'
#' @details Compute the number of direct child but also the number
#' of indirect child given by the ones related with the linked spouses.
#' If a relation ship dataframe is given, then even if no children is present
#' between 2 spouses, the indirect childs will still be added.
#'
#' @inheritParams Ped
#' @inheritParams Pedigree
#'
#' @return
#' ## When obj is a vector
#' A dataframe with the columns `num_child_dir`, `num_child_ind` and
#' `num_child_tot` giving respectively the direct, indirect and total number
#' of child.
#'
#' ## When obj is a Pedigree object
#' An updated Pedigree object with the columns `num_child_dir`,
#' `num_child_ind` and `num_child_tot` added to the
#' Pedigree `ped` slot.
#' @include AllClass.R
#' @export
#' @usage NULL
setGeneric("num_child", signature = "obj",
    function(obj, ...) standardGeneric("num_child")
)

#' @rdname num_child
#' @examples
#'
#' num_child(
#'   obj = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10"),
#'   dadid = c("3", "3", "6", "8", "0", "0", "0", "0", "0", "0"),
#'   momid = c("4", "5", "7", "9", "0", "0", "0", "0", "0", "0"),
#'   rel_df = data.frame(
#'       id1 = "10",
#'       id2 = "3",
#'       code = "Spouse"
#'   )
#' )
#' @export
setMethod("num_child", "character_OR_integer", function(obj, dadid, momid,
    rel_df = NULL, missid = NA_character_
) {
    id <- obj

    if (length(dadid) != length(id)) {
        stop("The length of dadid should be equal to the length of id")
    }
    if (length(momid) != length(id)) {
        stop("The length of momid should be equal to the length of id")
    }

    # Create dummy vectors for the check() function
    child <- NULL
    num_child_dir <- NULL
    idmin <- NULL
    idmax <- NULL
    childs <- NULL
    num_child_tot <- NULL
    child_min <- NULL
    child_max <- NULL
    childs_all <- NULL

    df <- data.frame(id, dadid, momid, stringsAsFactors = FALSE)
    if (nrow(df) == 0) {
        df <- data.frame(id = character(),
            num_child_dir = integer(),
            num_child_ind = integer(),
            num_child_tot = integer(),
            stringsAsFactors = FALSE
        )
        return(df)
    }

    spouse_rel <- unique(df[(!df$dadid %in% missid) &
                (!df$momid %in% missid), c("dadid", "momid")
        ]
    )
    colnames(spouse_rel) <- c("id1", "id2")

    if (!is.null(rel_df)) {
        rel_df <- norm_rel(rel_df, missid = missid)
        spouse_rel <- rbind(spouse_rel,
            rel_df[rel_df$code == "Spouse", c("id1", "id2")]
        )
    }
    spouse_rel$idmin <- pmin(spouse_rel$id1, spouse_rel$id2)
    spouse_rel$idmax <- pmax(spouse_rel$id1, spouse_rel$id2)
    spouse_rel <- unique(spouse_rel[c("idmin", "idmax")])

    if (nrow(spouse_rel) > 0) {
        dad_child <- df[(!df$dadid %in% missid), c("dadid", "id")] %>%
            group_by(dadid) %>%
            summarise(child = list(id)) %>%
            mutate(num_child_dir = lengths(child)) %>%
            rename(id = dadid)
        mom_child <- df[(!df$momid %in% missid), c("id", "momid")] %>%
            group_by(momid) %>%
            summarise(child = list(id)) %>%
            mutate(num_child_dir = lengths(child)) %>%
            rename(id = momid)
        id_child <- rbind(dad_child, mom_child)

        # Number of direct child per individual
        df$num_child_dir <- id_child$num_child_dir[match(df$id, id_child$id)]

        # Number of total childs per individual
        spouse_child <- spouse_rel %>%
            left_join(id_child, by = c("idmin" = "id")) %>%
            left_join(id_child, by = c("idmax" = "id"),
                suffix = c("_min", "_max")
            )
        rel_child <- spouse_child %>%
            rowwise() %>%
            mutate(childs = list(unique(unlist(
                list(child_min, child_max)
            )))) %>%
            select(c(idmin, idmax, childs)) %>%
            tidyr::pivot_longer(cols = -childs, names_to = "order",
                values_to = "id"
            ) %>%
            group_by(id) %>%
            summarise(childs_all = list(unique(unlist(childs)))) %>%
            mutate(num_child_tot = lengths(childs_all))

        df$num_child_tot <- rel_child$num_child_tot[match(df$id, rel_child$id)]

        df <- df %>%
            mutate(across(c(num_child_dir, num_child_tot),
                ~replace(., is.na(.), 0)
            ))

        df$num_child_ind <- df$num_child_tot - df$num_child_dir

        df
    } else {
        df$num_child_dir <- 0
        df$num_child_ind <- 0
        df$num_child_tot <- 0
        df
    }
})

#' @rdname num_child
#' @param reset If TRUE, the `num_child_tot`, `num_child_ind` and
#' the `num_child_dir` columns are reset.
#' @examples
#'
#' data(sampleped)
#' ped1 <- Pedigree(sampleped[sampleped$famid == "1",])
#' ped1 <- num_child(ped1, reset = TRUE)
#' summary(ped(ped1))
#' @export
setMethod("num_child", "Pedigree", function(obj, reset = FALSE) {
    df <- num_child(id(ped(obj)), dadid(ped(obj)), momid(ped(obj)),
        rel_df = as.data.frame(rel(obj))
    )

    if (!reset) {
        check_columns(as.data.frame(ped(obj)), NULL,
            c("num_child_tot", "num_child_ind", "num_child_dir"),
            NULL, others_cols = TRUE
        )
    }

    obj@ped@num_child_tot <- df$num_child_tot
    obj@ped@num_child_ind <- df$num_child_ind
    obj@ped@num_child_dir <- df$num_child_dir

    validObject(obj)
    obj
})
