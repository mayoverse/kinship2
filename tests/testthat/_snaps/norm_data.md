# Norm ped

    Code
      ped_df
    Output
         indId fatherId motherId gender steril available NumOther AffMod        sex
      1      1        3        4      2   TRUE      <NA>        1   <NA> terminated
      2      2        0        0      1  FALSE         1        2      A       male
      3      3        8        7    man  FALSE         0        2      E       male
      4      4        6        5  woman  FALSE         A        3      A     female
      5      5        0        0      f  FALSE      <NA>        7      E     female
      6      6     <NA>        0      m  FALSE         0     <NA>      D       male
      7      7        0        0      1  FALSE      <NA>        6      A       male
      8      8        0        0      1  FALSE         0        3      D       male
      9      8        2        0      2  FALSE      <NA>        3      A     female
      10     9        9        8      3  FALSE        Ab        5      B    unknown
         avail id dadid momid
      1     NA  1     3     4
      2      1  2     0     0
      3      0  3     8     7
      4      1  4     6     5
      5     NA  5     0     0
      6      0  6  <NA>     0
      7     NA  7     0     0
      8      0  8     0     0
      9     NA  8     2     0
      10     1  9     9     8
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
         family status
      1    <NA>      0
      2    <NA>      0
      3    <NA>      0
      4    <NA>      0
      5    <NA>      0
      6    <NA>      0
      7    <NA>      0
      8    <NA>      0
      9    <NA>      0
      10   <NA>      0

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

