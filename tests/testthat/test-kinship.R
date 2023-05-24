test_that("kinship works", {
  twindat <- c(
    1, 3, 4, 2,
    2, 0, 0, 1,
    3, 8, 7, 1,
    4, 6, 5, 2,
    5, 0, 0, 2,
    6, 0, 0, 1,
    7, 0, 0, 2,
    8, 0, 0, 1,
    100, 3, 4, 1,
    101, 3, 4, 2,
    102, 3, 4, 2,
    103, 3, 4, 2,
    104, 3, 4, 2,
    105, 3, 4, 2,
    106, 3, 4, 2,
    107, 0, 0, 1,
    108, 0, 0, 1,
    201, 2, 1, 1,
    202, 2, 1, 1,
    203, 2, 1, 1,
    204, 2, 1, 1,
    205, 107, 102, 1,
    206, 108, 103, 2
  )
  twindat <- matrix(twindat, ncol = 4, byrow = TRUE)
  dimnames(twindat) <- list(NULL, c("id", "dadid", "momid", "sex"))
  twindat <- data.frame(twindat)

  relate <- data.frame(
    id1 = c(101, 102, 101, 104, 203),
    id2 = c(102, 103, 103, 105, 204),
    code = c(1, 1, 1, 2, 1)
  )
  
  tped <- with(twindat, pedigree(id, dadid, momid, sex,
                                 relation=relate))
  
  expect_doppelganger("Twin pedigree",
                      plot(tped))
  
  kmat <- kinship(tped)
  
  ## should show kinship coeff of 0.5 for where MZ twins are
  ## ids: 102-103 and 203-204
  expect_true(all(kmat[c("102","101","103"),c("102","101","103")]==0.5))
  expect_true(all(kmat[c("203","204"),c("203","204")]==0.5))
  
  # Renumber everyone as 1,2,....; makes the all.equal checks easier
  indx <- sort(unique(unlist(twindat[, 1:3])))
  twindat$id <- match(twindat$id, indx) - 1
  twindat$dadid <- match(twindat$dadid, indx) - 1
  twindat$momid <- match(twindat$momid, indx) - 1
  relate$id1 <- match(relate$id1, indx) - 1
  relate$id2 <- match(relate$id2, indx) - 1

  # Build the pedigree and kinship
  tped <- with(twindat, pedigree(id, dadid, momid, sex,
    relation = relate
  ))
  kmat <- kinship(tped)
  
  truth <- matrix(
    c(
      5, 6, 0,
      5, 4, .25, # parent child
      10, 11, .5, # mz twins
      22, 12, .25, # aunt, mz with mother
      22, 13, .125, # aunt, dz
      13, 14, .25, # dz twins
      20, 21, .5, # mz twins
      19, 16, 0, # marry in uncle
      19, 11, .125, # aunt who is a twin
      19, 3, .125
    ), # grandmother
    byrow = TRUE, ncol = 3
  )
  expect_equal(kmat[truth[, 1:2]], truth[, 3])
})

test_that("Kinship Claus Ekstrom 09/2012", {
  ## simple test case for kinship of MZ twins from Claus Ekstrom, 9/2012
  mydata <- data.frame(id=1:4, dadid=c(NA, NA, 1, 1),
                       momid=c(NA, NA, 2, 2), sex=c("male", "female", "male", "male"),
                       famid=c(1,1,1,1))
  relation <- data.frame(id1=c(3), id2=c(4), famid=c(1), code=c(1))
  
  ped <- pedigree(id=mydata$id, dadid=mydata$dadid, momid=mydata$momid, sex=mydata$sex, relation=relation)
  
  expect_doppelganger("Twin pedigree 2", plot(ped))
  
  kmat <- kinship(ped)
  expect_true(all(kmat[3:4,3:4]==0.5))
})




test_that("kinship works with X chromosoms", {
  ## test pedigree from bioinformatics manuscript
  ## try x-chrom kinship
  ## also has inbreeding and twins, for quick check
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
    affected = cbind(disease, smoker, availstatus), relation = matrix(c(8, 9, 1), ncol = 3)
  ))

  ## regular kinship matrix
  expect_snapshot(kinship(ped2))
  expect_snapshot(kinship(ped2, chr = "X"))

  ped3 <- ped2
  ped3$sex[9] <- "unknown"

  ## regular again, should be same as above
  expect_equal(kinship(ped2), kinship(ped3))

  ## now with unknown sex, gets NAs
  k3 <- kinship(ped3, chrtype = "X")
  expect_true(all(is.na(k3[9, ])))

  # all descendants of sex=unknown to be NAs as well
  ped3$sex[8] <- "unknown"
  k4 <- kinship(ped3, chr = "X")
  expect_true(all(is.na(k4[8:10, ])))
})

test_that("Kinship with 2 different family", {
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

  ## testing kinship2 on pedigreeList when only one subject in a family
  peddf <- rbind(ped2df, c(2, 1, 0, 0, 1, 1, 0, 1, 0))

  peds <- with(peddf, pedigree(id, dad, mom, sex,
    status = vitalstatus, fam = fam,
    affected = cbind(disease, smoker, availstatus)
  ))
  kinfam <- kinship(peds)
  expect_true(all(kinfam["2/1", 1:10] == 0))

  ## now add two more for ped2, and check again
  peddf <- rbind(peddf, c(2, 2, 0, 0, 2, 1, 0, 1, 0), c(2, 3, 1, 2, 1, 1, 0, 1, 0))
  peds <- with(peddf, pedigree(id, dad, mom, sex,
    status = vitalstatus, fam = fam,
    affected = cbind(disease, smoker, availstatus)
  ))
  kin2fam <- kinship(peds)
  expect_true(all(kin2fam[11:13, 1:10] == 0))
})
