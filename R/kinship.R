#' @importFrom methods as
#' @importFrom Matrix forceSymmetric bdiag
NULL

#' Kinship matrix
#'
#' @description
#' Compute the kinship matrix for a set of related autosomal subjects.
#' The function is generic, and can accept a Pedigree, a Ped or a vector as
#' the first argument.
#'
#' @details
#' The function will usually be called with a Pedigree.
#' The call with a Ped or a vector is provided for backwards compatibility
#' with an earlier release of the library that was less capable.
#' Note that when using with a Ped or a vector, any information on
#' twins is not available to the function.
#'
#' When called with a Pedigree, the routine
#' will create a block-diagonal-symmetric sparse matrix object of class
#' `dsCMatrix`.  Since the `[i, j]` value of the result is 0 for any two
#' unrelated individuals i and j and a `Matrix` utilizes sparse
#' representation, the resulting object is often orders of magnitude smaller
#' than an ordinary matrix.
#'
#' Two genes G1 and G2 are identical by descent (IBD) if they are both physical
#' copies of the same ancestral gene; two genes are identical by state if they
#' represent the same allele.  So the brown eye gene that I inherited from my
#' mother is IBD with hers; the same gene in an unrelated individual is not.
#'
#' The kinship coefficient between two subjects is the probability that a
#' randomly selected allele from a locus will be IBD between them. It is
#' obviously 0 between unrelated individuals. For an autosomal site and no
#' inbreeding it will be 0.5 for an individual with themselves, .25 between
#' mother and child, .125 between an uncle and neice, etc.
#'
#' The computation is based on a recursive algorithm described in Lange, which
#' assumes that the founder alleles are all independent.
#'
#' @param obj A Pedigree or Ped object or a vector of subject identifiers.
#' @param chrtype chromosome type.  The currently supported types are
#' 'autosome' and 'X' or 'x'.
#' @inheritParams Ped
#'
#' @return
#' ## When obj is a vector
#' A matrix of kinship coefficients.
#' ## When obj is a Pedigree
#' A matrix of kinship coefficients ordered by families present
#' in the Pedigree object.
#'
#' @section References:
#' K Lange, Mathematical and Statistical Methods for
#' Genetic Analysis, Springer-Verlag, New York, 1997.
#' @seealso [make_famid()], [kindepth()]
#' @include AllClass.R
#' @include utils.R
#' @export
#' @usage NULL
setGeneric("kinship", signature = "obj",
    function(obj, ...) standardGeneric("kinship")
)

#' @rdname kinship
#' @export
setMethod("kinship", "Ped",
    function(obj, chrtype = "autosome"){
        kinship(
            id(obj), dadid(obj), momid(obj),
            sex(obj), chrtype = chrtype
        )
    }
)

#' @rdname kinship
#' @examples
#'
#' kinship(c("A", "B", "C", "D", "E"), c("C", "D", "0", "0", "0"),
#'     c("E", "E", "0", "0", "0"), sex = c(1, 2, 1, 2, 1))
#' kinship(c("A", "B", "C", "D", "E"), c("C", "D", "0", "0", "0"),
#'     c("E", "E", "0", "0", "0"), sex = c(1, 2, 1, 2, 1),
#'    chrtype = "x"
#' )
#' @export
setMethod("kinship", "character",
    function(obj, dadid, momid, sex, chrtype = "autosome") {
        id <- obj
        chrtype <- match.arg(casefold(chrtype), c("autosome", "x"))
        if (any(duplicated(id))) {
            stop("All id values must be unique")
        }
        n <- length(id)
        pdepth <- kindepth(id, dadid, momid)
        mom_row <- match(momid, id, nomatch = n + 1)  # row number of the mother
        dad_row <- match(dadid, id, nomatch = n + 1)  # row number of the dad
        if (chrtype == "autosome") {
            if (n == 1) {
                return(matrix(0.5, 1, 1, dimnames = list(id, id)))
            }
            kmat <- diag(c(rep(0.5, n), 0))  # founders
            ## When all unrelateds, pdepth all=0.  Put c(1,) to make guard from
            ## iter 1:0
            for (depth in seq_len(max(c(1, pdepth)))) {
                for (j in (seq_len(n))[pdepth == depth]) {
                    kmatv <- (kmat[mom_row[j], ] + kmat[dad_row[j], ]) / 2
                    kmat[, j] <- kmat[j, ] <- kmatv
                    kmat[j, j] <- (1 + kmat[mom_row[j], dad_row[j]]) / 2
                }
            }
        } else if (chrtype == "x") {
            if (missing(sex) || length(sex) != n) {
                stop("invalid sex vector")
            }
            # 1 = female, 2=male
            sex <- as.numeric(sex_to_factor(sex))
            if (n == 1) {
                return(
                    matrix(ifelse(sex > 2, sex / 2, NA), 1, 1,
                        dimnames = list(id, id)
                    )
                )
            }
            kmat <- diag(ifelse(sex > 2, NA, c((3 - sex) / 2, 0)))
            for (depth in seq_len(max(c(1, pdepth)))) {
                for (j in (seq_len(n))[pdepth == depth]) {
                    if (sex[j] == 1) {
                        kmat[, j] <- kmat[j, ] <- kmat[mom_row[j], ]
                        kmat[j, j] <- 1
                    } else if (sex[j] == 2) {
                        kmat[, j] <- kmat[j, ] <-
                            (kmat[mom_row[j], ] + kmat[dad_row[j], ]) / 2
                        kmat[j, j] <- (1 + kmat[mom_row[j], dad_row[j]]) / 2
                    } else {
                        kmat[, j] <- kmat[j, ] <- NA
                        kmat[j, j] <- NA
                    }
                }
            }
        }
        kmat <- kmat[seq_len(n), seq_len(n)]
        dimnames(kmat) <- list(id, id)
        as(kmat, "CsparseMatrix")
    }
)

#' @include kindepth.R
#' @rdname kinship
#' @examples
#'
#' data(sampleped)
#' ped <- Pedigree(sampleped)
#' kinship(ped)
#' @export
setMethod("kinship", "Pedigree",
    function(obj, chrtype = "autosome") {
        famlist <- unique(famid(obj))
        nfam <- length(famlist)
        matlist <- vector("list", nfam)
        ## The possibly reorderd list of id values
        idlist <- vector("list", nfam)

        for (i_fam in seq_along(famlist)) {
            if (is.na(famlist[i_fam])) { # If no family provided
                tped <- obj[is.na(famid(obj))]
            } else {
                ## Pedigree for this family
                tped <- obj[famid(obj) == famlist[i_fam]]
            }
            temp <- try({
                chrtype <- match.arg(casefold(chrtype), c("autosome", "x"))
                n <- length(id(ped(tped)))
                pdepth <- kindepth(tped)
                mom_row <- match(
                    momid(ped(tped)),
                    id(ped(tped)),
                    nomatch = n + 1
                )
                dad_row <- match(
                    dadid(ped(tped)),
                    id(ped(tped)),
                    nomatch = n + 1
                )
                # Are there any MZ twins to worry about?
                have_mz <- FALSE
                if (length(rel(tped)) > 0 &&
                        any(code(rel(tped)) == "MZ twin")
                ) {
                    have_mz <- TRUE
                    ## Doc: MakeMZIndex
                    temp <- which(code(rel(tped)) == "MZ twin")
                    ## drop=FALSE added in case only one MZ twin set
                    id1x <- match(
                        id1(rel(tped)),
                        id(ped(tped)),
                        nomatch = NA
                    )
                    id2x <- match(
                        id2(rel(tped)),
                        id(ped(tped)),
                        nomatch = NA
                    )
                    if (any(is.na(id1x)) | any(is.na(id2x))) {
                        stop("All individuals in relationship matrix",
                            "should be present in the pedigree informations"
                        )
                    }
                    mzmat <- matrix(
                        c(id1x, id2x), ncol = 2
                    )[temp, , drop = FALSE]
                    ## everyone starts in their own group
                    mzgrp <- seq_len(max(mzmat))
                    ## The loop below will take k-1 iterations for a set labeled
                    ## as (k-1):k, ..., 4:3, 3:2, 2:1; this is the worst case.
                    while (1) {
                        # Wait for all MZ to be in the same group
                        if (all(mzgrp[mzmat[, 1]] == mzgrp[mzmat[, 2]])) {
                            break
                        }
                        for (i in seq_len(nrow(mzmat))) {
                            mzgrp[mzmat[i, 1]] <- mzgrp[mzmat[i, 2]] <-
                                min(mzgrp[mzmat[i, ]])
                        }
                    }
                    ## Now make a matrix that has a row for every possible pair.
                    ## Finally, remove the rows that are identical.
                    ## The result is a set of all pairs of observations in the
                    ## matrix that correspond to monozygotic pairs.
                    mzindex <- cbind(unlist(tapply(mzmat, mzgrp[mzmat],
                        function(x) {
                            z <- unique(x)
                            rep(z, length(z))
                        }
                    )),
                    unlist(tapply(mzmat, mzgrp[mzmat], function(x) {
                        z <- unique(x)
                        rep(z, each = length(z))
                    })))
                    mzindex <- mzindex[mzindex[, 1] != mzindex[, 2], ]
                }

                if (chrtype == "autosome") {
                    if (n == 1) {
                        kmat <- matrix(0.5, 1, 1,
                            dimnames = list(id(ped(tped)), id(ped(tped)))
                        )
                    } else {
                        kmat <- diag(c(rep(0.5, n), 0))  # founders
                        for (depth in seq_len(max(pdepth))) {
                            indx <- which(pdepth == depth)
                            kmat[indx, ] <- (
                                kmat[mom_row[indx], ] + kmat[dad_row[indx], ]
                            ) / 2
                            kmat[, indx] <- (
                                kmat[, mom_row[indx]] + kmat[, dad_row[indx]]
                            ) / 2
                            for (j in indx) {
                                kmat[j, j] <- (
                                    1 + kmat[mom_row[j], dad_row[j]]
                                ) / 2
                            }
                            if (have_mz) {
                                kmat[mzindex] <- (diag(kmat))[mzindex[, 1]]
                            }
                        }
                    }
                } else if (chrtype == "x") {
                    sex <- as.numeric(sex(ped(tped)))  # 1=female, 2=male
                    if (n == 1) {
                        kmat <- matrix(sex / 2, 1, 1,
                            dimnames = list(id(ped(tped)), id(ped(tped)))
                        )
                    } else {
                        ## 1 for males, 1/2 for females
                        kmat <- diag(c((3 - sex) / 2, 0))
                        for (depth in seq_len(max(pdepth))) {
                            for (j in (seq_len(n))[pdepth == depth]) {
                                if (sex[j] == 1) {
                                    kmat[, j] <- kmat[j, ] <- kmat[mom_row[j], ]
                                    kmat[j, j] <- 1
                                } else if (sex[j] == 2) {
                                    kmatv <- (kmat[mom_row[j], ] +
                                            kmat[dad_row[j], ]
                                    ) / 2
                                    kmat[, j] <- kmat[j, ] <- kmatv
                                    kmat[j, j] <- (
                                        1 + kmat[dad_row[j], mom_row[j]]
                                    ) / 2
                                } else {
                                    kmat[, j] <- kmat[j, ] <- NA
                                    kmat[j, j] <- NA
                                }
                                if (have_mz)
                                    kmat[mzindex] <- (diag(kmat))[mzindex[, 1]]
                            }
                        }
                    }
                }
                if (n > 1) {
                    kmat <- kmat[seq_len(n), seq_len(n)]
                    dimnames(kmat) <- list(id(ped(tped)), id(ped(tped)))
                }
                kmat
            }, silent = TRUE)
            if ("try-error" %in% class(temp)) {
                stop("In family", famlist[i_fam], ":", temp)
            } else {
                matlist[[i_fam]] <- temp
            }
            ## deprecated in Matrix: as(forceSymmetric(temp), 'dsCMatrix')
            idlist[[i_fam]] <- id(ped(tped))
        }
        if (length(famlist) == 1) {
            as(matlist[[1]],
                "CsparseMatrix"
            )
        } else {
            for (i_fam in seq_along(famlist)) {
                matlist[[i_fam]] <- as(as(
                    forceSymmetric(matlist[[i_fam]]),
                    "symmetricMatrix"
                ), "CsparseMatrix")
            }
            result <- bdiag(matlist)
            temp <- unlist(idlist)
            dimnames(result) <- list(temp, temp)
            result
        }
    }
)
