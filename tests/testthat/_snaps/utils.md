# var_to_factor works

    Code
      var_fact
    Output
       [1] Sup to 0.5 Inf to 0.5 Inf to 0.5 Sup to 0.5 Inf to 0.5 Inf to 0.5
       [7] Inf to 0.5 Inf to 0.5 Sup to 0.5 Inf to 0.5
      Levels: Inf to 0.5 Sup to 0.5

# df_cont_table

    Code
      df_ct1
    Output
              var1 Freq
      1 Inf to 0.5    7
      2 Sup to 0.5    3

---

    Code
      df_ct2
    Output
              var1          var2 Freq
      1 Inf to 0.5 [0.0519,0.25]    2
      2 Sup to 0.5 [0.0519,0.25]    0
      3 Inf to 0.5    (0.25,0.5]    3
      4 Sup to 0.5    (0.25,0.5]    1
      5 Inf to 0.5    (0.5,0.75]    1
      6 Sup to 0.5    (0.5,0.75]    2
      7 Inf to 0.5  (0.75,0.836]    1
      8 Sup to 0.5  (0.75,0.836]    0

