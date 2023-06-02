## example data and test steps from pedigree.shrink
## Jason Sinnwell

test_that("Pedigree shrink works", {
  data(minnbreast)
  pedMN <- with(minnbreast, pedigree(id, fatherid, motherid, sex,
    famid = famid,
    affected = cbind(cancer, bcpc, proband)
  ))

  ## this pedigree as one person with cancer. The pedigree is not informative
  ## if they are the only available, so pedigree.shrink trims all.
  ## This caused an error in pedigree.shrink before kinship2. v1.2.8. Now fixed
  mn2 <- pedMN[2]
  expect_doppelganger("pedigree shrink 1", plot(mn2))

  ## breaks in pedigree.trim
  shrink.mn2 <- pedigree.shrink(mn2,
    avail = ifelse(is.na(mn2$affected[, 1]), 0, mn2$affected[, 1])
  )

  expect_equal(shrink.mn2$idList$unavail,
               c(44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59,
                 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75,
                 76, 77, 78, 79, 26050, 26051))

  mnf8 <- pedMN["8"]
  expect_doppelganger("pedigree shrink 2", plot(mnf8))

  shrink.mnf8 <- pedigree.shrink(mnf8,
    avail = ifelse(is.na(mnf8$affected[, 1]), 0, mnf8$affected[, 1])
  )

  expect_equal(shrink.mnf8$idList$unavail,
               c(137, 138, 139, 140, 144, 145, 146, 147, 148, 150, 151, 152,
               153, 154, 155, 156, 157, 158, 159, 160, 163, 164, 165, 166, 167,
               168, 169, 170, 171, 172, 173, 174))
})

test_that("Pedigree shrink error if missing info", {
  ## use sample.ped from the package
  data(sample.ped)

  pedAll <- pedigree(sample.ped$id, sample.ped$father, sample.ped$mother,
                     sample.ped$sex,
                     affected = cbind(sample.ped$affected, sample.ped$avail),
                     famid = sample.ped$ped
  )
  ped2 <- pedAll["2"]
  ped2$sex[c(13, 12)] <- c("unknown", "terminated")

  ## set 2nd col of affected to NA
  ped2$affected[c(7, 9), 2] <- NA
  expect_error(pedigree.shrink(ped = ped2, avail = ped2$affected[, 2], maxBits = 32))
})

test_that("Pedigree shrink avail test", {

  ## use sample.ped from the package
  data(sample.ped)

  pedAll <- pedigree(sample.ped$id, sample.ped$father, sample.ped$mother,
    sample.ped$sex,
    affected = cbind(sample.ped$affected, sample.ped$avail),
    famid = sample.ped$ped
  )
  ped1 <- pedAll["1"]

  set.seed(10)
  shrink1.avail.B32 <- pedigree.shrink(ped = ped1, avail = ped1$affected[, 2], maxBits = 32)

  set.seed(10)
  shrink1.avail.B25 <- pedigree.shrink(ped = ped1, avail = ped1$affected[, 2], maxBits = 25)

  expect_equal(shrink1.avail.B32$idTrimmed,
               c(101, 102, 107, 108, 111, 113, 121, 122, 123, 131, 132, 134, 139))
  
  expect_equal(shrink1.avail.B25$idTrimmed,
               c(101, 102, 107, 108, 111, 113, 121, 122, 123, 131, 132, 134, 139, 140, 141))
})

test_that("Pedigree shrink with character", {
  ## use sample.ped from the package
  data(sample.ped)
  
  pedAll <- pedigree(sample.ped$id, sample.ped$father, sample.ped$mother,
                     sample.ped$sex,
                     affected = cbind(sample.ped$affected, sample.ped$avail),
                     famid = sample.ped$ped
  )
  
  # Select first family
  ped1 <- pedAll["1"]
  ped1df <- as.data.frame(ped1)
  
  # Change id to character
  ped1df$idchar <- gsub("^1", "A-", as.character(ped1df$id))
  ped1df$dadidchar <- gsub("^1", "A-", as.character(ped1df$dadid))
  ped1df$momidchar <- gsub("^1", "A-", as.character(ped1df$momid))
  # ped1df$dadidchar <- ifelse(ped1df$dadidchar=="0", NA, ped1df$dadidchar)
  # ped1df$momidchar <- ifelse(ped1df$momidchar=="0", NA, ped1df$momidchar)
  ped1char <- with(ped1df, pedigree(idchar, dadidchar, momidchar, sex, affected, missid = c("0")))
  
  set.seed(100)
  shrink1.p1char.B32 <- pedigree.shrink(ped = ped1char, avail = ped1char$affected[, 2], maxBits = 32)
  expect_equal(shrink1.p1char.B32$idTrimmed,
              c("A-01", "A-02", "A-07", "A-08", "A-11", "A-13", "A-21", "A-22",
                "A-23", "A-31", "A-32", "A-34", "A-39"))
  
  set.seed(100)
  shrink1.p1char.B25 <- pedigree.shrink(ped = ped1char, avail = ped1char$affected[, 2], maxBits = 25)
  expect_equal(shrink1.p1char.B25$idTrimmed,
               c("A-01", "A-02", "A-07", "A-08", "A-11", "A-13", "A-21", "A-22",
                 "A-23", "A-31", "A-32", "A-34", "A-39", "A-33", "A-41"))
})

test_that("pedigree.shrink.plot works", {
  data(sample.ped)

  fam2 <- sample.ped[sample.ped$ped == 2, ]
  ped2 <- pedigree(fam2$id, fam2$father, fam2$mother, fam2$sex,
                  fam2$affected, fam2$avail)

  shrink2 <- pedigree.shrink(ped2, avail = fam2$avail)

  expect_doppelganger("Shrinked ped 1",
    plot.pedigree.shrink(shrink2, title = "Sample Pedigree 2"))
  expect_doppelganger("Shrinked ped 2",
    plot.pedigree.shrink(shrink2, bigped = TRUE, title = "Sample Pedigree 2"))
})