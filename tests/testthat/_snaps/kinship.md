# kinship works with X chromosoms

    Code
      kinship(ped2)
    Message <simpleMessage>
      The following `from` values were not present in `x`: 1, 0
      The following `from` values were not present in `x`: Affected are None, Healthy are None
    Output
      [1] "Bal: generate_colors, fill_scale"
      [1] "Bal: generate_colors: keep_full_scale = FALSE"
    Message <simpleMessage>
      The following `from` values were not present in `x`: FALSE, TRUE
      The following `from` values were not present in `x`: FALSE, TRUE
    Output
             1     2      3      4     5     6       7       8       9      10
      1  0.500 0.000 0.2500 0.2500 0.000 0.000 0.12500 0.12500 0.12500 0.12500
      2  0.000 0.500 0.2500 0.2500 0.000 0.000 0.12500 0.12500 0.12500 0.12500
      3  0.250 0.250 0.5000 0.2500 0.000 0.000 0.25000 0.12500 0.12500 0.18750
      4  0.250 0.250 0.2500 0.5000 0.000 0.000 0.12500 0.25000 0.25000 0.18750
      5  0.000 0.000 0.0000 0.0000 0.500 0.000 0.25000 0.00000 0.00000 0.12500
      6  0.000 0.000 0.0000 0.0000 0.000 0.500 0.00000 0.25000 0.25000 0.12500
      7  0.125 0.125 0.2500 0.1250 0.250 0.000 0.50000 0.06250 0.06250 0.28125
      8  0.125 0.125 0.1250 0.2500 0.000 0.250 0.06250 0.50000 0.50000 0.28125
      9  0.125 0.125 0.1250 0.2500 0.000 0.250 0.06250 0.50000 0.50000 0.28125
      10 0.125 0.125 0.1875 0.1875 0.125 0.125 0.28125 0.28125 0.28125 0.53125

---

    Code
      kinship(ped2, chr = "X")
    Message <simpleMessage>
      The following `from` values were not present in `x`: 1, 0
      The following `from` values were not present in `x`: Affected are None, Healthy are None
    Output
      [1] "Bal: generate_colors, fill_scale"
      [1] "Bal: generate_colors: keep_full_scale = FALSE"
    Message <simpleMessage>
      The following `from` values were not present in `x`: FALSE, TRUE
      The following `from` values were not present in `x`: FALSE, TRUE
    Output
            1    2     3      4     5 6      7      8      9     10
      1  1.00 0.00 0.000 0.5000 0.000 0 0.0000 0.5000 0.5000 0.2500
      2  0.00 0.50 0.500 0.2500 0.000 0 0.2500 0.2500 0.2500 0.2500
      3  0.00 0.50 1.000 0.2500 0.000 0 0.5000 0.2500 0.2500 0.3750
      4  0.50 0.25 0.250 0.5000 0.000 0 0.1250 0.5000 0.5000 0.3125
      5  0.00 0.00 0.000 0.0000 0.500 0 0.2500 0.0000 0.0000 0.1250
      6  0.00 0.00 0.000 0.0000 0.000 1 0.0000 0.0000 0.0000 0.0000
      7  0.00 0.25 0.500 0.1250 0.250 0 0.5000 0.1250 0.1250 0.3125
      8  0.50 0.25 0.250 0.5000 0.000 0 0.1250 1.0000 1.0000 0.5625
      9  0.50 0.25 0.250 0.5000 0.000 0 0.1250 1.0000 1.0000 0.5625
      10 0.25 0.25 0.375 0.3125 0.125 0 0.3125 0.5625 0.5625 0.5625

