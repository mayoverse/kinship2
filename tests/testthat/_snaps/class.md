# Class ped work

    Code
      ped2
    Output
      Ped object with 2 individuals and 0 metadata columns:
                   id       dadid       momid                      sex       famid
          <character> <character> <character> <c("ordered", "factor")> <character>
      ID5         ID5        <NA>        <NA>                   female        <NA>
      ID4         ID4        <NA>        <NA>                     male        <NA>
             steril    status     avail  affected num_child_total num_child_direct
          <numeric> <numeric> <numeric> <numeric>       <numeric>        <numeric>
      ID5      <NA>      <NA>      <NA>      <NA>               0                0
      ID4      <NA>      <NA>      <NA>      <NA>               0                0
          num_child_indirect
                   <numeric>
      ID5                  0
      ID4                  0

# Rel class works

    Code
      rel2
    Output
      Rel object with 2 relationshipswith 1 MZ twin, 0 DZ twin, 0 UZ twin, 1 Spouse:
                id1         id2                     code       famid |         A
        <character> <character> <c("ordered", "factor")> <character> | <numeric>
      1         ID3         ID5                  MZ twin        <NA> |         1
      2         ID2         ID4                   Spouse        <NA> |         2

# Scales class works

    invalid class "Scales" object: 1: affected column(s) must be logical
    invalid class "Scales" object: 2: angle column(s) must be numeric
    invalid class "Scales" object: 3: order column(s) must be numeric
    invalid class "Scales" object: 4: mods column(s) must be numeric
    invalid class "Scales" object: 5: column_mods column(s) must be character

---

    invalid class "Scales" object: 1: labels column(s) must be character
    invalid class "Scales" object: 2: mods column(s) must be numeric

