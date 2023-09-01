#' @importFrom methods as

usethis::use_package("Matrix")
#' Compute a kinship matrix
#'
#' @description
#' Compute the kinship matrix for a set of related autosomal subjects.  The
#' function is generic, and can accept a pedigree, pedigreeList, or vector as
#' the first argument.
#'
#' @details
#' The function will usually be called with a pedigree or pedigreeList; the
#' third form is provided for backwards compatability with an earlier release
#' of the library that was less capable.  The first argument is named `id`
#' for the same reason.  Note that when using the third form any information on
#' twins is not available to the function.
#'
#' When called with a pedigreeList, i.e., with multiple families, the routine
#' will create a block-diagonal-symmetric sparse matrix object of class
#' `dsCMatrix`.  Since the [i,j] value of the result is 0 for any two
#' unrelated individuals i and j and a `Matrix` utilizes sparse
#' representation, the resulting object is often orders of magnitude smaller
#' than an ordinary matrix.  When `kinship` is called with a single
#' pedigree an ordinary matrix is returned.
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
#' @aliases kinship kinship.default kinship.pedigree kinship.pedigreeList
#' @param id either a pedigree object, pedigreeList object, or a vector of
#' subject identifiers.  Subject identifiers may be numeric or character.
#' @param dadid for each subject, the identifier of the biological father.
#' This is only used if `id` is a vector.
#' @param momid for each subject, the identifier of the biological mother.
#' This is only used if `id` is a vector.
#' @param sex vector of sex values coded as 1=male, 2=female
#' @param chrtype chromosome type.  The currently supported types are
#' 'autosome' and 'X' or 'x'.
#' @param ... Any number of optional arguments
#'
#' @return a matrix of kinship coefficients.
#'
#' @examples
#'
#' test1 <- data.frame(
#'   id = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14),
#'   mom = c(0, 0, 0, 0, 2, 2, 4, 4, 6, 2, 0, 0, 12, 13),
#'   dad = c(0, 0, 0, 0, 1, 1, 3, 3, 3, 7, 0, 0, 11, 10),
#'   sex = c(0, 1, 0, 1, 0, 1, 0, 1, 0, 0, 0, 1, 1, 1)
#' )
#' tped <- with(test1, pedigree(id, dad, mom, sex))
#' round(8 * kinship(tped))
#'
#' @section References: K Lange, Mathematical and Statistical Methods for
#' Genetic Analysis, Springer-Verlag, New York, 1997.
#' @seealso `pedigree`, `makekinship`, `make_famidb`
#' @keywords genetics
#' @include pedigreeClass.R
#' @export
setGeneric("kinship", signature = "obj",
    function(obj, ...) standardGeneric("kinship")
)

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
            for (depth in 1:max(c(1, pdepth))) {
                for (j in (1:n)[pdepth == depth]) {
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
            if (n == 1) {
                return(
                    matrix(ifelse(sex > 2, sex / 2, NA), 1, 1,
                        dimnames = list(id, id)
                    )
                )
            }
            sex <- as.numeric(sex)
            kmat <- diag(ifelse(sex > 2, NA, c((3 - sex) / 2, 0)))
            for (depth in 1:max(c(1, pdepth))) {
                for (j in (1:n)[pdepth == depth]) {
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
        kmat <- kmat[1:n, 1:n]
        dimnames(kmat) <- list(id, id)
        as(kmat, "CsparseMatrix")
    }
)

#' @include kindepth.R
#' @export
setMethod("kinship", "Pedigree",
    function(obj, chrtype = "autosome") {
        famlist <- unique(obj$ped$family)
        nfam <- length(famlist)
        matlist <- vector("list", nfam)
        ## The possibly reorderd list of id values
        idlist <- vector("list", nfam)

        for (i_fam in seq_along(famlist)) {
            if (is.na(famlist[i_fam])) { # If no family provided
                tped <- obj[is.na(obj$ped$family)]
            } else {
                ## Pedigree for this family
                tped <- obj[obj$ped$family == famlist[i_fam]]
            }
            temp <- try({
                chrtype <- match.arg(casefold(chrtype), c("autosome", "x"))
                n <- length(tped$ped$id)
                pdepth <- kindepth(tped)
                mom_row <- match(tped$ped$momid, tped$ped$id, nomatch = n + 1)
                dad_row <- match(tped$ped$dadid, tped$ped$id, nomatch = n + 1)
                # Are there any MZ twins to worry about?
                have_mz <- FALSE
                if (!is.null(tped$rel) && any(tped$rel$code == "MZ twin")) {
                    have_mz <- TRUE
                    ## Doc: MakeMZIndex
                    temp <- which(tped$rel$code == "MZ twin")
                    ## drop=FALSE added in case only one MZ twin set
                    id1x <- match(tped$rel$id1, tped$ped$id, nomatch = NA)
                    id2x <- match(tped$rel$id2, tped$ped$id, nomatch = NA)
                    if (any(is.na(id1x)) | any(is.na(id2x))) {
                        stop("All individuals in relationship matrix",
                            "should be present in the pedigree informations"
                        )
                    }
                    mzmat <- matrix(
                        c(id1x, id2x), ncol = 2
                    )[temp, , drop = FALSE]
                    mzgrp <- 1:max(mzmat)  # everyone starts in their own group
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
                    mzindex <- cbind(unlist(tapply(mzmat, mzgrp[mzmat], function(x) {
                        z <- unique(x)
                        rep(z, length(z))
                    })), unlist(tapply(mzmat, mzgrp[mzmat], function(x) {
                        z <- unique(x)
                        rep(z, each = length(z))
                    })))
                    mzindex <- mzindex[mzindex[, 1] != mzindex[, 2], ]
                }

                if (chrtype == "autosome") {
                    if (n == 1) {
                        kmat <- matrix(0.5, 1, 1,
                            dimnames = list(tped$ped$id, tped$ped$id)
                        )
                    } else {
                        kmat <- diag(c(rep(0.5, n), 0))  # founders
                        for (depth in 1:max(pdepth)) {
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
                    sex <- as.numeric(tped$ped$sex)  # 1=female, 2=male
                    if (n == 1) {
                        kmat <- matrix(sex / 2, 1, 1,
                            dimnames = list(tped$ped$id, tped$ped$id)
                        )
                    } else {
                        ## 1 for males, 1/2 for females
                        kmat <- diag(c((3 - sex) / 2, 0))
                        for (depth in 1:max(pdepth)) {
                            for (j in (1:n)[pdepth == depth]) {
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
                    kmat <- kmat[1:n, 1:n]
                    dimnames(kmat) <- list(tped$ped$id, tped$ped$id)
                }
                kmat
            }, silent = TRUE)
            if ("try-error" %in% class(temp)) {
                stop(paste("In family", famlist[i_fam], ":", temp))
            } else {
                matlist[[i_fam]] <- temp
            }
            ## deprecated in Matrix: as(forceSymmetric(temp), 'dsCMatrix')
            idlist[[i_fam]] <- tped$ped$id
        }
        if (length(famlist) == 1) {
            as(matlist[[1]],
                "CsparseMatrix"
            )
        } else {
            for (i_fam in seq_along(famlist)) {
                matlist[[i_fam]] <- as(as(
                    Matrix::forceSymmetric(matlist[[i_fam]]),
                    "symmetricMatrix"
                ), "CsparseMatrix")
            }
            result <- Matrix::bdiag(matlist)
            temp <- unlist(idlist)
            dimnames(result) <- list(temp, temp)
            result
        }
    }
)
