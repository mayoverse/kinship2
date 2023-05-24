test_that("makefamid works", {
  id <- 1:20
  mom <- c(0,0,0,2,2,2,0,2,0, 0,2,2,0,2,0,2, 7,7, 11,14)
  dad <- c(0,0,0,1,1,1,0,1,0, 0,3,3,0,3,0,3, 8,8, 10,13)
  famid <- c(1,1,1,1,1,1,1,1,0,1,1,1,1,1,0,1,1,1,1,1)
  temp<- makefamid(id, mom, dad)
  expect_equal(temp, famid)
})
