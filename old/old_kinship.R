famlist <- unique(id$famid)
nfam <- length(famlist)
matlist <- vector("list", nfam)
idlist <- vector("list", nfam)  # the possibly reorderd list of id values

for (i in seq_along(famlist)) {
    tped <- id[i]  # pedigree for this family
    temp <- try(kinship(tped, chrtype = chrtype, ...), silent = TRUE)
    if ("try-error" %in% class(temp)) {
        stop(paste("In family", famlist[i], ":", temp))
    } else {
        matlist[[i]] <- as(as(Matrix::forceSymmetric(temp),
            "symmetricMatrix"),
            "CsparseMatrix")
    }
    ## deprecated in Matrix: as(forceSymmetric(temp), 'dsCMatrix')
    idlist[[i]] <- tped$id
}

result <- Matrix::bdiag(matlist)
if (any(duplicated(obj$ped$id))) {
    temp <- paste(rep(famlist, sapply(idlist, length)),
        unlist(idlist), sep = "/")
} else {
    temp <- unlist(idlist)
}

dimnames(result) <- list(temp, temp)
result