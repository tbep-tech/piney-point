test_that("Check if there are ranges for comparison for all sites", {
  
  chk <- any(is.na(rswqdat$nrmrng))
  
  expect_false(chk)
  
})
