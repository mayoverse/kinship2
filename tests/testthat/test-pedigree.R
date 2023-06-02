test_that("pedigree fails to line up", {
  # Here is a case where the levels fail to line up properly
  data(sample.ped)
  df1 <- sample.ped[sample.ped$ped == 1, ]
  ped1 <- with(df1, pedigree(id, father, mother, sex, affected))
  expect_doppelganger("ped1", plot(ped1))

  # With reordering it's better
  df1reord <- df1[c(35:41, 1:34), ]
  ped1reord <- with(df1reord, pedigree(id, father, mother,
    sex,
    affected = affected
  ))
  expect_doppelganger("ped1reorder", plot(ped1reord))
})

test_that("pedigree subscripting", {
  data(minnbreast)

  minnped <- with(minnbreast, pedigree(id, fatherid, motherid, sex,
    affected = cancer, famid = famid
  ))
  ped8 <- minnped["8"] # a modest sized family

  # Subjects 150, 152, 154, 158 are children,
  # and 143, 162, 149 are parents and a child
  droplist <- c(150, 152, 154, 158, 143, 162, 149)

  keep1 <- !(ped8$id %in% droplist) # logical
  keep2 <- which(keep1) # numeric
  keep3 <- as.character(ped8$id[keep1]) # character
  keep4 <- factor(keep3)

  test1 <- ped8[keep1]
  test2 <- ped8[keep2]
  test3 <- ped8[keep3]
  test4 <- ped8[keep4]
  expect_equal(test1, test2)
  expect_equal(test1, test3)
  expect_equal(test1, test4)
})

test_that("pedigree other test", {
  ped2mat <- matrix(c(
    1, 1, 0, 0, 1,
    1, 2, 0, 0, 2,
    1, 3, 1, 2, 1,
    1, 4, 1, 2, 2,
    1, 5, 0, 0, 2,
    1, 6, 0, 0, 1,
    1, 7, 3, 5, 2,
    1, 8, 6, 4, 1,
    1, 9, 6, 4, 1,
    1, 10, 8, 7, 2
  ), ncol = 5, byrow = TRUE)

  ped2df <- as.data.frame(ped2mat)
  names(ped2df) <- c("fam", "id", "dad", "mom", "sex")
  ## 1 2  3 4 5 6 7 8 9 10,11,12,13,14,15,16
  ped2df$disease <- c(NA, NA, 1, 0, 0, 0, 0, 1, 1, 1)
  ped2df$smoker <- c(0, NA, 0, 0, 1, 1, 1, 0, 0, 0)
  ped2df$availstatus <- c(0, 0, 1, 1, 0, 1, 1, 1, 1, 1)
  ped2df$vitalstatus <- c(1, 1, 1, 0, 1, 0, 0, 0, 0, 0)

  ped2 <- with(ped2df, pedigree(id, dad, mom, sex,
    status = vitalstatus,
    affected = cbind(disease, smoker, availstatus),
    relation = matrix(c(8, 9, 1), ncol = 3),
  ))
  expect_doppelganger("OtherPed with twin", ped2)
})
