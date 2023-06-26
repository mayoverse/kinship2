#' Number of child
#'
#' @description Compute the number of child per individual
#'
#' @details Compute the number of direct child but also the number
#' of indirect child given by the ones related with the linked spouses.
#' If a relation ship matrix is given, then even if no children is present
#' between 2 spouses, the indirect childs will still be added.
#'
#' @param df Dataframe processed for pedigree computation with the columns
#' `id`, `dadid` and `momid` available
#' @param relation A matrix with 3 required columns (id1, id2, code) specifying
#' special relationship between pairs of individuals.
#' Codes: 1=Monozygotic twin, 2=Dizygotic twin, 3=Twin of unknown zygosity,
#' 4=Spouse.
#'
#' @return A dataframe with the columns `num_child_dir`, `num_child_ind` and
#' `num_child_tot` giving respectively the direct, indirect and total number
#' of child.
#'
#' @export num_child
num_child <- function(df, relation = NULL) {
  require(dplyr)
  require(tidyr)
  cols_needed <- c("id", "dadid", "momid")
  cols_used <- c("num_child_dir", "num_child_ind", "num_child_tot")

  df <- check_columns(df, cols_needed, cols_used, "", others_cols = TRUE)

  spouse_rel <- unique(df[df$dadid != 0 & df$momid != 0, c("dadid", "momid")])
  colnames(spouse_rel) <- c("id1", "id2")

  if (!is.null(relation)) {
    cols_needed <- c("id1", "id2", "code")
    relation <- check_columns(relation, cols_needed, "", "",
        others_cols = FALSE)
    spouse_rel <- rbind(
        spouse_rel,
        relation[relation$code == 4, c("id1", "id2")])
  }
  spouse_rel$idmin <- pmin(spouse_rel$id1, spouse_rel$id2)
  spouse_rel$idmax <- pmax(spouse_rel$id1, spouse_rel$id2)
  spouse_rel <- unique(spouse_rel[c("idmin", "idmax")])

  dad_child <- df[df$dadid != 0, c("dadid", "id")] %>%
      group_by(dadid) %>%
      summarise(child = list(id)) %>%
      mutate(num_child_dir = lengths(child)) %>%
      rename(id = dadid)
  mom_child <- df[df$momid != 0, c("id", "momid")] %>%
      group_by(momid) %>%
      summarise(child = list(id)) %>%
      mutate(num_child_dir = lengths(child)) %>%
      rename(id = momid)
  id_child <- rbind(dad_child, mom_child)

  # Number of direct child per individual
  df$num_child_dir <- merge(df, id_child, by = "id",
    all.x = TRUE, all.y = FALSE, suffixes = c("", "_new"))$num_child_dir

  message(paste(
    length(df$id[!is.na(df$num_child_dir)]),
    "individuals have at least one child"
  ))

  # Number of total childs per individual
  rel_child <- spouse_rel %>%
    left_join(id_child, by = join_by(idmin == id)) %>%
    left_join(id_child, by = join_by(idmax == id),
        suffix = c("_min", "_max")) %>%
    rowwise %>%
    mutate(childs = list(unique(unlist(list(child_min, child_max))))) %>%
    select(c(idmin, idmax, childs)) %>%
    pivot_longer(cols = -childs, names_to = "order", values_to="id") %>%
    group_by(id) %>%
    summarise(childs_all = list(unique(unlist(childs)))) %>%
    mutate(num_child_tot = lengths(childs_all))

  df$num_child_tot <- merge(df, rel_child[c("id", "num_child_tot")], by = "id",
    all.x = TRUE, all.y = FALSE)$num_child_tot

  df <- df %>%
    mutate(across(c(num_child_dir, num_child_tot), ~replace(., is.na(.), 0)))

  df$num_child_ind <- df$num_child_tot - df$num_child_dir
  message(paste(
    length(df$id[df$num_child_ind > 0]),
    "individuals have at least one indirected child"
  ))

  df
}
