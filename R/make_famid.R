# $Id: make_famid.s,v 1.7 2003/01/07 15:47:08 therneau Exp

#' Get family id
#'
#' @description
#' Construct a family id from Pedigree information
#'
#' @details
#' Create a vector of length n, giving the family 'tree' number of each
#' subject.  If the Pedigree is totally connected, then everyone will end up in
#' tree 1, otherwise the tree numbers represent the disconnected subfamilies.
#' Singleton subjects give a zero for family number.
#'
#' @inheritParams kinship
#'
#' @return
#' ## When used with a character vector
#' An integer vector giving family groupings
#'
#' ## When used with a Pedigree object
#' An updated Pedigree object with the family id added
#'
#' @seealso [kinship()]
#' @examples
#' data(sampleped)
#' ped1 <- Pedigree(sampleped[,-1])
#' make_famid(ped1)
#' @export
setGeneric("make_famid", signature = "obj",
    function(obj, ...) standardGeneric("make_famid")
)

#' @export
#' @rdname make_famid
#' @aliases make_famid,character
#' @docType methods
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
                match(famid, sort(unique(famid))) - 1  # renumber
            } else {
                match(famid, sort(unique(famid)))
            }  # renumber, no zeros
        } else {
            stop("Bug in routine: seem to have found an infinite loop")
        }
    }
)

#' @export
#' @rdname make_famid
#' @aliases make_famid,Pedigree
#' @docType methods
#' @include AllClass.R
setMethod("make_famid", "Pedigree",
    function(obj) {
        ped <- obj
        famid <- make_famid(
            ped(ped, "id"), ped(ped, "dadid"), ped(ped, "momid")
        )
        ped(obj, "famid") <- famid

        fam_id1 <- famid[match(rel(ped, "id1"), ped(ped, "id"))]
        fam_id2 <- famid[match(rel(ped, "id2"), ped(ped, "id"))]

        if (any(fam_id1 != fam_id2)) {
            stop("The two individuals in the relationship",
                "are not in the same family"
            )
        }

        rel(ped, "famid") <- fam_id1
        validObject(ped)
        ped
    }
)