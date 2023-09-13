## Extracted from checks.Rnw

#' Detect hints inconsistencies
#'
#' @description
#' This routine tries to detect inconsistencies in spousal hints.
#'
#' @details
#' These arise in `auto_hint()` with complex pedigrees.
#' One can have ABA (subject A is on both the left and the right of B),
#' cycles, etc.
#' Users can introduce problems as well if they modify the hints.
#'
#' @param sex A vector with the sex of all the individuals
#' @inheritParams align
#'
#' @examples
#' data(sampleped)
#' ped1 <- pedigree(sampleped[sampleped$family == "1",])
#' ht1 <- auto_hint(ped1)
#' check_hints(ht1, ped1$ped$sex)
#'
#' @seealso [auto_hint()], [best_hint()]
#' @keywords internal
#' @export
check_hints <- function(hints, sex) {
    if (is.null(hints$order)) {
        stop("Order component must be present in hints")
    }
    if (!is.numeric(hints$order)) {
        stop("Order component must be numeric")
    }
    n <- length(sex)
    if (length(hints$order) != n) {
        stop("Length for order component should be equal to sex length")
    }

    spouse <- hints$spouse
    if (!is.null(spouse)) {
        lspouse <- spouse[, 1]
        rspouse <- spouse[, 2]
        if (any(lspouse < 1 | lspouse > n | rspouse < 1 | rspouse > n)) {
            stop("Invalid spouse value, should be between 1 and", n)
        }

        temp1 <- (sex[lspouse] == "female" & sex[rspouse] == "male")
        temp2 <- (sex[rspouse] == "female" & sex[lspouse] == "male")
        if (!all(temp1 | temp2)) {
            stop("A marriage is not male/female")
        }

        hash <- n * pmax(lspouse, rspouse) + pmin(lspouse, rspouse)
        # Turn off this check for now - is set off if someone is married to two
        # siblings if (any(duplicated(hash))) stop('Duplicate marriage')

        # TODO Break any loops: A left of B, B left of C, C left of A.  Not yet
        # done
    }
}
