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
                sex steril avail id dadid momid
      1  terminated      1    NA  1     3     4
      2        male      0     1  2     0     0
      3        male      0     0  3     8     7
      4      female      0    NA  4     6     5
      5      female      0    NA  5     0     0
      6        male      0     0  6  <NA>     0
      7        male      0    NA  7     0     0
      8        male      0     0  8     0     0
      9      female      0    NA  8     2     0
      10    unknown      0    NA  9     9     8
                                                                          error
      1                                                                    <NA>
      2                                                     isSterilButIsParent
      3                                                      fatherIdDuplicated
      4                                                                    <NA>
      5                                                                    <NA>
      6                                                     isSterilButIsParent
      7                                                    isMotherButNotFemale
      8                 selfIdDuplicated_isMotherAndFather_isMotherButNotFemale
      9  selfIdDuplicated_oneParentMissing_isMotherAndFather_isFatherButNotMale
      10                   motherIdDuplicated_isItsOwnParent_isFatherButNotMale
         affected status family vitalStatus affection
      1        NA     NA   <NA>          NA        NA
      2        NA     NA   <NA>          NA        NA
      3        NA     NA   <NA>          NA        NA
      4        NA     NA   <NA>          NA        NA
      5        NA     NA   <NA>          NA        NA
      6        NA     NA   <NA>          NA        NA
      7        NA     NA   <NA>          NA        NA
      8        NA     NA   <NA>          NA        NA
      9        NA     NA   <NA>          NA        NA
      10       NA     NA   <NA>          NA        NA

# Norm rel

    Code
      rel_df
    Output
        indId1 indId2    code family  id1 id2                          error
      1      1      2 MZ twin      1  1_1 1_2                           <NA>
      2      1      3 DZ twin      1  1_1 1_3                           <NA>
      3      2      3 UZ twin      1  1_2 1_3                           <NA>
      4      1      2  Spouse      2  2_1 2_2                           <NA>
      5      3      4 MZ twin      2  2_3 2_4                           <NA>
      6      6      7    <NA>      2  2_6 2_7               CodeNotRecognise
      7      8      8  Spouse      2  2_8 2_8                         SameId
      8      9      0  Spouse      1  1_9   0                           <NA>
      9   <NA>      B    <NA>      1 <NA> 1_B indId1length0_CodeNotRecognise

