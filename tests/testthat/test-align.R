test_that("align.pedigree works", {
  data("sample.ped")
  ped <- with(sample.ped, pedigree(id, father, mother, sex))
  withr::local_options(width = 50)
  expect_snapshot(align.pedigree(ped))
  align <- align.pedigree(ped)
  expect_equal(align$n, c(8, 19, 22, 8))
})

test_that("test autohint works", {
  data("sample.ped")
  ped <- with(sample.ped, pedigree(id, father, mother, sex))
  newhint <- autohint(ped)  #this fixes up marriages and such
  plist <- align.pedigree(ped, packed=TRUE, align=TRUE,
                          width=8, hints=newhint)
  expect_snapshot(plist)
})
