# The pedigree S4 object

The pedigree S4 object is a class that contains all the information about a pedigree.
It is composed of 2 slots:

    - `ped_df` which is a data.frame containing the pedigree information with at least 4 columns: `id`, `fid`, `mid`, `sex`.
    - `rel_mat` which is a matrix containing the relationship between each individual of the pedigree with at least 3 columns: `id1`, `id2`, `code`.

The methods available for this class are the following:

    - autoplot()
    - plot()
    - summary()
    - print()
    - show()
    - as.data.frame()
    - setAs()
    - [
    - [<-
    - [[
    - [[<-
    - $
    - $<-
    - c()
    - rbind()
    - cbind()
    - dim()
    - dimnames()
    - names()
    - colnames()
    - rownames()
    - nrow()
