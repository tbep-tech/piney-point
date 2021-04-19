test_that("Checking levels in phytoplankton data", {
  
  chk <- levels(rsphydat$valqual)
  
  expect_equal(chk, c('Very low', 'Low', 'Medium'))
  
})
