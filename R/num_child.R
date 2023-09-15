#' @importFrom dplyr group_by summarise mutate rename left_join rowwise join_by
#' @importFrom tidyr pivot_longer
NULL

#' Number of child
#'
#' @description Compute the number of child per individual
#'
#' @details Compute the number of direct child but also the number
#' of indirect child given by the ones related with the linked spouses.
#' If a relation ship matrix is given, then even if no children is present
#' between 2 spouses, the indirect childs will still be added.
#'
#' @inheritParams kinship
#' @inheritParams pedigree
#' @inheritParams is_parent
#'
#' @return
#' ## When obj is a vector
#' A dataframe with the columns `num_child_dir`, `num_child_ind` and
#' `num_child_tot` giving respectively the direct, indirect and total number
#' of child.
#'
#' ## When obj is a pedigree object
#' An updated pedigree object with the columns `num_child_dir`, `num_child_ind`
#' and `num_child_tot` added to the pedigree `ped` slot.
#'
#' @include pedigreeClass.R
#' @export
setGeneric("num_child", signature = "obj",
    function(obj, ...) standardGeneric("num_child")
)

#' @export
#' @rdname num_child
#' @aliases num_child,character
#' @docType methods
setMethod("num_child", "character", function(obj, dadid, momid,
    rel_df = NULL, missid = "0"
) {
    id <- obj

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

    spouse_rel <- unique(df[df$dadid != missid &
                df$momid != missid, c("dadid", "momid")
        ]
    )
    colnames(spouse_rel) <- c("id1", "id2")

    if (!is.null(rel_df)) {
        cols_needed <- c("id1", "id2", "code")
        rel_df <- check_columns(rel_df, cols_needed, "", "",
            others_cols = FALSE
        )
        spouse_rel <- rbind(spouse_rel,
            rel_df[rel_df$code == "Spouse", c("id1", "id2")]
        )
    }
    spouse_rel$idmin <- pmin(spouse_rel$id1, spouse_rel$id2)
    spouse_rel$idmax <- pmax(spouse_rel$id1, spouse_rel$id2)
    spouse_rel <- unique(spouse_rel[c("idmin", "idmax")])

    dad_child <- df[df$dadid != missid, c("dadid", "id")] %>%
        group_by(dadid) %>%
        summarise(child = list(id)) %>%
        mutate(num_child_dir = lengths(child)) %>%
        rename(id = dadid)
    mom_child <- df[df$momid != missid, c("id", "momid")] %>%
        group_by(momid) %>%
        summarise(child = list(id)) %>%
        mutate(num_child_dir = lengths(child)) %>%
        rename(id = momid)
    id_child <- rbind(dad_child, mom_child)

    # Number of direct child per individual
    df$num_child_dir <- id_child$num_child_dir[match(df$id, id_child$id)]

    # Number of total childs per individual
    rel_child <- spouse_rel %>%
        left_join(id_child, by = c("idmin" = "id")) %>%
        left_join(id_child, by = c("idmax" = "id"),
            suffix = c("_min", "_max")
        ) %>%
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
})

#' @export
#' @rdname num_child
#' @aliases num_child,Pedigree
#' @docType methods
#' @param reset If TRUE, the `num_child_tot`, `num_child_ind` and
#' the `num_child_dir` columns are reset.
setMethod("num_child", "Pedigree", function(obj, reset = FALSE) {
    df <- num_child(obj$ped$id, obj$ped$dadid, obj$ped$momid,
        rel_df = obj$rel
    )

    if (!reset) {
        check_columns(obj$ped, NULL,
            c("num_child_tot", "num_child_ind", "num_child_dir"),
            NULL, others_cols = TRUE
        )
    }

    obj$ped <- merge(obj$ped,
        df[c("id", "num_child_tot", "num_child_ind", "num_child_dir")],
        by = "id", sort = FALSE
    )

    obj
})
TRUE
