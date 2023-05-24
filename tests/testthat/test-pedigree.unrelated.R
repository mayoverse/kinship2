test_that("Pedigree unrelated", {
  data(sample.ped)

  pedAll <- pedigree(sample.ped$id, sample.ped$father, sample.ped$mother,
    sample.ped$sex,
    famid = sample.ped$ped,
    affected = cbind(sample.ped$affected, sample.ped$avail)
  )

  ped1 <- pedAll["1"]
  ped2 <- pedAll["2"]

  ## to see plot:
  expect_doppelganger("Pedigree unrelated 1", plot.pedigree(ped1, align = FALSE))
  set.seed(10)
  expect_equal(
    pedigree.unrelated(ped1, avail = ped1$affected[, 2]),
    c("109", "113", "133", "141")
  )

  set.seed(10)
  expect_equal(
    pedigree.unrelated(ped2, avail = ped2$affected[, 2]),
    c("203", "206")
  )
})
