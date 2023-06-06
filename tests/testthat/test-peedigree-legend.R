test_that("pedigree.legend works", {
  data("sample.ped")
  ped <- with(sample.ped, pedigree(id, father, mother, sex,
    affected = cbind(affected, avail)))
  withr::local_options(width = 50)

  expect_doppelganger("Ped legend bottom right", {
    plot(ped)
    pedigree.legend(ped, location = "bottomright",
        radius = 0.8)
    }
    )
  expect_doppelganger("Ped legend c(2,2)", {
    plot(ped)
    pedigree.legend(ped, location = c(2, 2),
        radius = 0.8)
    }
    )
})

test_that("legendPlot works", {
  data("sample.ped")
  ped <- with(sample.ped, pedigree(id, father, mother, sex,
    affected = cbind(affected, avail)))
  expect_doppelganger("Legend plot", legendPlot(ped,
    affected.label = c("cancer", "available")))
})
