# Norm ped

    Code
      ped_df
    Output
         indId fatherId motherId gender sterilisation available NumOther AffMod
      1      1        3        4      2          TRUE      <NA>        1   <NA>
      2      2        0        0      1          TRUE         1        2      A
      3      3        8        7    man         FALSE         0        2      E
      4      4        6        5  woman         FALSE         A        3      A
      5      5        0        0      f         FALSE      <NA>        7      E
      6      6     <NA>        0      m          TRUE         0     <NA>      D
      7      7        0        0      1         FALSE      <NA>        6      A
      8      8        0        0      1         FALSE         0        3      D
      9      8        2        0      2         FALSE      <NA>        3      A
      10     9        9        8      3         FALSE        Ab        5      B
                sex steril status avail id dadid momid famid
      1  terminated   TRUE     NA    NA  1     3     4  <NA>
      2        male  FALSE     NA  TRUE  2     0     0  <NA>
      3        male  FALSE     NA FALSE  3     8     7  <NA>
      4      female  FALSE     NA    NA  4     6     5  <NA>
      5      female  FALSE     NA    NA  5     0     0  <NA>
      6        male  FALSE     NA FALSE  6  <NA>     0  <NA>
      7      female  FALSE     NA    NA  7     0     0  <NA>
      8      female  FALSE     NA FALSE  8     0     0  <NA>
      9      female  FALSE     NA    NA  8     2     0  <NA>
      10       male  FALSE     NA    NA  9     9     8  <NA>
                                                         error affected family
      1                                                   <NA>       NA   <NA>
      2                                    isSterilButIsParent       NA   <NA>
      3                                     fatherIdDuplicated       NA   <NA>
      4                                                   <NA>       NA   <NA>
      5                                                   <NA>       NA   <NA>
      6                   oneParentMissing_isSterilButIsParent       NA   <NA>
      7                                                   <NA>       NA   <NA>
      8  selfIdDuplicated_isMotherAndFather_isFatherButNotMale       NA   <NA>
      9  selfIdDuplicated_isMotherAndFather_isFatherButNotMale       NA   <NA>
      10                     motherIdDuplicated_isItsOwnParent       NA   <NA>
         vitalStatus affection
      1           NA        NA
      2           NA        NA
      3           NA        NA
      4           NA        NA
      5           NA        NA
      6           NA        NA
      7           NA        NA
      8           NA        NA
      9           NA        NA
      10          NA        NA

# Norm rel

    Code
      rel_df
    Output
         id1 id2    code famid                          error
      1    1   2 MZ twin  <NA>                           <NA>
      2    1   3 DZ twin  <NA>                           <NA>
      3    2   3 UZ twin  <NA>                           <NA>
      4    1   2  Spouse  <NA>                           <NA>
      5    3   4 MZ twin  <NA>                           <NA>
      6    6   7    <NA>  <NA>               CodeNotRecognise
      7    8   8  Spouse  <NA>                         SameId
      8    9   0  Spouse  <NA>                           <NA>
      9 <NA>   B    <NA>  <NA> indId1length0_CodeNotRecognise

---

    Code
      norm_rel(rel_df, missid = "0")
    Output
            id1   id2    code famid            error
      1       1     2 MZ twin  <NA>             <NA>
      2       3     2 DZ twin  <NA>             <NA>
      3       3     1 DZ twin  <NA>             <NA>
      4       3     4 MZ twin  <NA>             <NA>
      5       7 Other    <NA>  <NA> CodeNotRecognise
      6 spo Use     9    <NA>  <NA> CodeNotRecognise

