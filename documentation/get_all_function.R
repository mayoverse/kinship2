## How to get all function in a package

allf <- getNamespaceExports("kinship2")
allff <- allf[!str_detect(allf, ".__")]

allS4 <- allf[str_detect(allf, ".__T__")]
allS4 <- allS4[str_detect(allS4, ":kinship2")]
allS4 <- str_remove_all(allS4, ":kinship2")
allS4 <- str_remove_all(allS4, ".__T__")

allff <- allff[!allff %in% allS4]
allArg <- lapply(allff, function(x){names(formals(x))})
names(allArg) <- allff

idl <- c("descendants")
allS4 <- allS4[!allS4 %in% idl]

allS4Arg <- lapply(allS4, function(x){names(S4formals(x, "character"))})
names(allS4Arg) <- allS4

othArg <- lapply(idl, function(x){names(S4formals(x, c("character", "character")))})
names(othArg) <- idl


allArg <- c(allArg, allS4Arg, othArg)
allArg

Arg <- unique(unlist(allArg))

df <- lapply(names(allArg), function(x) {
    ifelse(Arg %in% allArg[[x]], 1, 0)
}) %>% as.data.frame()
colnames(df) <- names(allArg)
rownames(df) <- Arg

heatmap(as.matrix(df[!rowSums(df) == 1, ]), cexRow = 1, cexCol = 0.8)

OnlyArg <- df[rowSums(df) == 1, ]
OnlyArg[colSums(OnlyArg) > 0]
