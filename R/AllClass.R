#' S4 class to represent a hints object.
#'
#' A hints object is a list of two elements used
#' to order the individuals in the pedigree plot.
#'
#' @slot order A numeric vector with one element per subject in the
#' Pedigree.  It determines the relative order of subjects within a sibship, as
#' well as the relative order of processing for the founder couples. (For this
#' latter, the female founders are ordered as though they were sisters).
#' @slot spouse A matrix with one row per hinted marriage, usually
#' only a few marriages in a Pedigree will need an added hint, for
#' instance reverse the plot order of a husband/wife pair.
#' Each row contains the index of the left spouse, the right hand spouse,
#' and the anchor (i.e : `1` = left, `2` = right, `0` = either).
#'
#' @return A Hints object.
#' @seealso [Pedigree()]
#' @docType class
#' @name Hints-class
#' @rdname Hints-class
#' @export
setClass("Hints",
    representation(
        order = "numeric",
        spouse = "matrix"
    )
)

setValidity("Hints", is_valid_hints)

#' S4 class to represent the scales of a Pedigree.
#'
#' A scales object is a list of two data.frame used
#' to represent the affection and the availability status
#' of the individuals in the pedigree plot.
#'
#' @slot fill A data.frame with the informations for the affection status.
#' The columns needed are: 'column_values', 'column_mods', 'mods', 'labels',
#' 'affected', 'fill', 'density' and 'angle'.
#' @slot border A data.frame with the informations for the availability status.
#' The columns needed are: 'column', 'mods', 'labels' and 'border'.
#' @return A Hints object.
#' @seealso [Pedigree()]
#' @docType class
#' @name Hints-class
#' @rdname Hints-class
#' @export
setClass("Scales",
    representation(
        fill = "data.frame",
        border = "data.frame"
    )
)

setValidity("Scales", is_valid_scales)

#' S4 class to represent the identity informations in a Pedigree.
#'
#' A Ped object is a list of identity informations
#' of the individuals in the pedigree.
#' It is used to create a Pedigree object.
#' The minimal needed informations are `id`, `dadid`, `momid` and `sex`.
#' If a `family` is provided, the individuals `id` will be aggregated
#' to the `family` character to ensure the uniqueness of the `id`.
#' The other slots are used to store recognized informations.
#' Additional columns can be added to the Ped object and will be
#' stored in the `meta` slot of the Ped object.
#'
#' @slot id A character vector with the id of the individuals.
#' @slot dadid A character vector with the id of the father of the individuals.
#' @slot momid A character vector with the id of the mother of the individuals.
#' @slot sex A factor vector for the sex of the individuals (i.e. `male`,
#' `female`, `unknown` or `terminated`).
#' @slot family A character vector with the family of the individuals.
#' @slot steril A numeric vector with the sterilisation status of the
#' individuals (i.e. `0` = not sterilised, `1` = sterilised, `NA` = unknown).
#' @slot status A numeric vector with the affection status of the
#' individuals (i.e. `0` = alive, `1` = dead, `NA` = unknown).
#' @slot avail A numeric vector with the availability status of the
#' individuals (i.e. `0` = not available, `1` = available, `NA` = unknown).
#' @slot affected A numeric vector with the affection status of the
#' individuals (i.e. `0` = not affected, `1` = affected, `NA` = unknown).
#' @slot useful A numeric vector with the usefulness status of the
#' individuals (i.e. `0` = not useful, `1` = useful).
#' @slot kin A numeric vector with minimal kinship value between the
#' individuals and the useful individuals.
#' @slot num_child_total A numeric vector with the total number of children
#' of the individuals.
#' @slot num_child_direct A numeric vector with the number of children
#' of the individuals.
#' @slot num_child_indirect A numeric vector with the number of children
#' of the individuals.
#'
#' @return A Ped object.
#' @seealso [Pedigree()]
#' @docType class
#' @name Ped-class
#' @rdname Ped-class
#'
#' @export
setClass("Ped",
    contains = c("Vector"),
    representation(
        id = "character",
        dadid = "character",
        momid = "character",
        sex = "factor",
        family = "character",
        steril = "numeric",
        status = "numeric",
        avail = "numeric",
        affected = "numeric",
        useful = "numeric",
        kin = "numeric",
        num_child_total = "numeric",
        num_child_direct = "numeric",
        num_child_indirect = "numeric"
    )
)

setMethod("parallel_slot_names", "Ped",
    function(x) {
        c(
            "id", "momid", "dadid", "sex", "family",
            "steril", "status", "avail", "affected",
            "kin", "useful", "num_child_total",
            "num_child_direct", "num_child_indirect",
            callNextMethod()
        )
    }
)

setValidity("Ped", is_valid_ped)


#' S4 class to represent the special relationships in a Pedigree.
#'
#' A Rel object is a list of special relationships
#' between individuals in the pedigree.
#' It is used to create a Pedigree object.
#' The minimal needed informations are `id1`, `id2` and `code`.
#' If a `family` is provided, the individuals `id` will be aggregated
#' to the `family` character to ensure the uniqueness of the `id`.
#'
#' @slot id1 A character vector with the id of the first individual.
#' @slot id2 A character vector with the id of the second individual.
#' @slot code A character vector with the code of the special relationship.
#' (i.e. `MZ twin`, `DZ twin`, `UZ twin` or `Spouse`).
#' @slot family A character vector with the family of the individuals.
#'
#' @return A Rel object.
#' @seealso [Pedigree()]
#' @docType class
#' @name Rel-class
#' @rdname Rel-class
#' @export
setClass("Rel",
    contains = c("Vector"),
    representation(
        id1 = "character",
        id2 = "character",
        code = "character",
        family = "character"
    )
)

setValidity("Rel", is_valid_rel)

setMethod("parallel_slot_names", "Rel",
    function(x) {
        c(
            "id1", "id2", "code", "family",
            callNextMethod()
        )
    }
)

#' S4 class to represent a pedigree.
#'
#' A pedigree is a ensemble of individuals linked to each other into
#' a family tree.
#'
#' They are created from a data.frame containing the individuals informations
#' and a relation ship data.frame for the special links between individuals.
#' A list of scales can be provided to create a legend.
#' To create a Pedigree object, use the function
#' [Pedigree()].
#'
#' @slot ped A data.frame with the individuals informations. The minimum
#' columns required are 'id', 'dadid', 'momid' and 'sex'. Other columns can be
#' added to the data.frame and will be recognised by the functions. Some
#' errors can be detected by the validity function and some of them can be
#' corrected and others will be added to a dedicated column.
#' @slot rel A data.frame for the special relationship between
#' individuals.
#' The minimum columns required are 'id1', 'id2' and 'code'.
#' @slot scales A data.frame to use for the affection status.
#' This data.frame is generated by the function
#' [generate_aff_inds()] followed by
#' [generate_colors()].
#' @slot hints List of two elements.
#' - **order** is a numeric vector with one element per subject in the
#' Pedigree.  It determines the relative order of subjects within a sibship, as
#' well as the relative order of processing for the founder couples. (For this
#' latter, the female founders are ordered as though they were sisters).
#' - **spouse** is a matrix with one row per hinted marriage, usually
#' only a few marriages in a Pedigree will need an added hint, for
#' instance reverse the plot order of a husband/wife pair.
#' Each row contains the index of the left spouse, the right hand spouse,
#' and the anchor (i.e : `1` = left, `2` = right, `0` = either).
#'
#' @return A Pedigree object.
#' @seealso [Pedigree()]
#' @docType class
#' @name Pedigree-class
#' @rdname Pedigree-class
#' @include pedigreeValidity.R
#' @include Pedigree.R
#' @export
setClass("Pedigree",
    contains = c("RectangularData"),
    representation(
        ped = "Ped",              # identity data
        rel = "Rel",              # special relationships
        scales = "Scales",        # scales for the plot
        hints = "Hints"           # hints for the plot
    )
)

setValidity("Pedigree", is_valid_pedigree)