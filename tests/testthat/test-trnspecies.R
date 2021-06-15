test_that("Checking if transect species are correct", {
  
  chk <- anyNA(rstrndat$taxa)
  
  expect_false(chk)
  
})
