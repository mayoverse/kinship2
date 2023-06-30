library(kinship2)

data(testped1)

df <- testped1
colnames(df)
cols_ren <- list(indId = "id", fatherId = "father",
    motherId = "mother", gender = "sex")
data.table::setnames(df,
                old = as.vector(unlist(cols_ren)),
                new = names(cols_ren))
colnames(df)

df_norm <- check_data(df)

col_classes <- "character"
quote = "'"
header = TRUE
sep = "/t"
read.csv("C:/Users/llenezet/Documents/EnCours/kinship2/data/testped1.tab", quote = quote,
                header = header, sep = sep, fill = TRUE,
                colClasses = col_classes, row.names = 1)
utils::read.table("C:/Users/llenezet/Documents/EnCours/kinship2/data/testped1.tab", quote = "",
                header = header)
