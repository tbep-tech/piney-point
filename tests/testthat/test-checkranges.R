test_that("Check if there are ranges for comparison for all sites", {
  
  chk <- any(grepl('NA', rswqdat$nrmrng))
  
  expect_false(chk)
  
})
