#' Compute family id
#'
#' @description
#' Construct a family identifier from pedigree information
#'
#' @details
#' Create a vector of length n, giving the family 'tree' number of each
#' subject.  If the Pedigree is totally connected, then everyone will end up in
#' tree 1, otherwise the tree numbers represent the disconnected subfamilies.
#' Singleton subjects give a zero for family number.
#'
#' @inheritParams Ped
#'
#' @return
#' ## When used with a character vector
#' An integer vector giving family groupings
#'
#' ## When used with a Pedigree object
#' An updated Pedigree object with the family id added
#' and with all ids updated
#'
#' @seealso [kinship()]
#' @export
#' @usage NULL
setGeneric("make_famid", signature = "obj",
    function(obj, ...) standardGeneric("make_famid")
)

#' @rdname make_famid
#' @examples
#'
#' make_famid(
#'      c("A", "B", "C", "D", "E", "F"),
#'      c("C", "D", "0", "0", "0", "0"),
#'      c("E", "E", "0", "0", "0", "0")
#' )
setMethod("make_famid", "character",
    function(obj, dadid, momid) {
        id <- obj
        n <- length(id)
        mid <- c(match(momid, id, nomatch = n + 1), n + 1)
        did <- c(match(dadid, id, nomatch = n + 1), n + 1)
        mid2 <- sort(unique(mid))
        did2 <- sort(unique(did))
        famid <- seq_len(n + 1)
        # The key idea of the algorithm: 1. iteratively set the family id of
        # parent/child sets to the minimum value of the set 2.
        # Add a subject 'n+1', who is the parent of all orphans, and also of
        # himself, to make the father/mother/child vectors all be the same
        # length

        # Run the depth routine to check for impossible parentage loops, which
        # would lead to infinite iterations.  And even then, give it an upper
        # limit of n iterations (it should never even come close to this).
        # You might think it would finish in max(depth) iterations, but assume
        # 2 families of depth 3 who intermarry at the last generation: the final
        # id propogates down one tree and then up the other.
        # Chains can take even longer (child of A marries child of B, second
        # child of B marries child of C, second child of C marries child of D,
        # ...).
        # However, at each iteration the final number must get propogated to at
        # least one child or at least 2 parents, giving a limit of n-1
        for (i in seq_len(n)) {
            # set children = min(self, parents)
            newid <- pmin(famid, famid[mid], famid[did])
            # mom = min(mon, children) dad = min(dad, children)
            newid[mid2] <- pmin(newid[mid2], tapply(newid, mid, min))
            newid[n + 1] <- n + 1  # preserve the 'no parent' code
            newid[did2] <- pmin(newid[did2], tapply(newid, did, min))
            newid[n + 1] <- n + 1  # preserve the 'no parent' code

            if (all(newid == famid)) {
                break
            } else if (i < n) {
                famid <- newid
            }
        }

        if (all(newid == famid)) {
            # renumber the results : family 0 for uniques, else small integers
            famid <- famid[seq_len(n)]  # toss the 'n+1' obs
            xx <- table(famid)
            if (any(xx == 1)) {
                singles <- as.integer(names(xx[xx == 1]))  # famid of singletons
                famid[!is.na(match(famid, singles))] <- 0  # set singletons to 0
                as.character(match(famid, sort(unique(famid))) - 1) # renumber
            } else {
                as.character(match(famid, sort(unique(famid))))
            }  # renumber, no zeros
        } else {
            stop("Bug in routine: seem to have found an infinite loop")
        }
    }
)

#' @rdname make_famid
#' @examples
#'
#' data(sampleped)
#' ped1 <- Pedigree(sampleped[,-1])
#' make_famid(ped1)
setMethod("make_famid", "Pedigree",
    function(obj) {
        famid <- make_famid(
            id(ped(obj)), dadid(ped(obj)), momid(ped(obj))
        )
        obj@ped@famid <- famid

        fam_id1 <- famid[match(id1(rel(obj)), id(ped(obj)))]
        fam_id2 <- famid[match(id2(rel(obj)), id(ped(obj)))]

        if (any(fam_id1 != fam_id2)) {
            stop("The two individuals in the relationship",
                "are not in the same family"
            )
        }

        obj@rel@famid <- fam_id1
        obj <- upd_famid_id(obj)
        validObject(obj)
        obj
    }
)

#' Update family prefix in individuals id
#'
#' Update the family prefix in the individuals identifiers.
#' Individuals identifiers are constructed as follow **famid**_**id**.
#' Therefore to update their family prefix the ids are split by the
#' first underscore and the first part is overwritten by **famid**.
#'
#' If famid is *missing*, then the `famid()` function will be called on the
#' object.
#'
#' @param obj Ped or Pedigree object or a character vector of individual ids
#' @inheritParams Ped
#'
#' @return A character vector of individual ids with family prefix
#' updated
#'
#' @export
#' @usage NULL
setGeneric("upd_famid_id",
    function(obj, famid, ...) standardGeneric("upd_famid_id")
)

#' @rdname upd_famid_id
#' @examples
#'
#' upd_famid_id(c("1", "2", "B_3"), c("A", "B", "A"))
#' upd_famid_id(c("1", "B_2", "C_3", "4"), c("A", NA, "A", NA))
#' @export
setMethod("upd_famid_id", "character",
    function(obj, famid, missid = NA_character_) {
        if (length(obj) != length(famid)) {
            stop("id and famid must have the same length")
        }
        id <- obj[!obj %in% missid]
        famid <- famid[!obj %in% missid]
        if (! is.character(id)) {
            stop("id must be a character vector")
        }
        id[!str_detect(id, "_")] <- paste0("_", id[!str_detect(id, "_")])
        ids <- str_split_fixed(id, "_", 2)
        ids[, 1] <- famid
        new_ids <- ifelse(
            ids[, 1] %in% missid,
            ids[, 2],
            paste(ids[, 1], ids[, 2], sep = "_")
        )
        obj[!obj %in% missid] <- new_ids
        obj
    }
)

#' @rdname upd_famid_id
#' @export
setMethod("upd_famid_id",
    signature(obj = "Ped", famid = "character_OR_integer"),
    function(obj, famid) {
        obj@id <- upd_famid_id(id(obj), famid)
        obj@dadid <- upd_famid_id(dadid(obj), famid)
        obj@momid <- upd_famid_id(momid(obj), famid)
        obj@famid <- famid
        validObject(obj)
        obj
    }
)

#' @rdname upd_famid_id
setMethod("upd_famid_id",
    signature(obj = "Ped", famid = "missing"),
    function(obj) {
        obj@id <- upd_famid_id(id(obj), famid(obj))
        obj@dadid <- upd_famid_id(dadid(obj), famid(obj))
        obj@momid <- upd_famid_id(momid(obj), famid(obj))
        validObject(obj)
        obj
    }
)

#' @rdname upd_famid_id
setMethod("upd_famid_id",
    signature(obj = "Rel", famid = "character_OR_integer"),
    function(obj, famid) {
        obj@id1 <- upd_famid_id(id1(obj), famid)
        obj@id2 <- upd_famid_id(id2(obj), famid)
        obj@famid <- famid
        validObject(obj)
        obj
    }
)

#' @rdname upd_famid_id
setMethod("upd_famid_id",
    signature(obj = "Rel", famid = "missing"),
    function(obj) {
        obj@id1 <- upd_famid_id(id1(obj), famid(obj))
        obj@id2 <- upd_famid_id(id2(obj), famid(obj))
        validObject(obj)
        obj
    }
)

#' @rdname upd_famid_id
#' @examples
#'
#' data(sampleped)
#' ped1 <- Pedigree(sampleped[,-1])
#' id(ped(ped1))
#' new_fam <- make_famid(id(ped(ped1)), dadid(ped(ped1)), momid(ped(ped1)))
#' id(ped(upd_famid_id(ped1, new_fam)))
setMethod("upd_famid_id",
    signature(obj = "Pedigree", famid = "character_OR_integer"),
    function(obj, famid) {
        old_id <- id(ped(obj))
        obj@ped <- upd_famid_id(ped(obj), famid)
        fid1 <- famid[match(id1(rel(obj)), old_id)]
        fid2 <- famid[match(id2(rel(obj)), old_id)]
        if (any(fid1 != fid2)) {
            stop("The two individuals in the relationship",
                "are not in the same family"
            )
        }
        obj@rel <- upd_famid_id(rel(obj), fid1)
        validObject(obj)
        obj
    }
)

#' @rdname upd_famid_id
#' @examples
#'
#' data(sampleped)
#' ped1 <- Pedigree(sampleped[,-1])
#' make_famid(ped1)
setMethod("upd_famid_id",
    signature(obj = "Pedigree", famid = "missing"),
    function(obj) {
        obj@ped <- upd_famid_id(ped(obj))
        obj@rel <- upd_famid_id(rel(obj))
        validObject(obj)
        obj
    }
)