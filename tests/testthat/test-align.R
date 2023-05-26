test_that("align.pedigree works", {
  data("sample.ped")
  ped <- with(sample.ped, pedigree(id, father, mother, sex))
  withr::local_options(width = 50)
  expect_snapshot(align.pedigree(ped))
  align <- align.pedigree(ped)
  expect_equal(align$n, c(8, 19, 22, 8))
})
