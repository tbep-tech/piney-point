test_that("Checking levels in phytoplankton data", {
  
  chk <- levels(rsphydat$valqual)
  chk <- any(!chk %in% c('Very low', 'Low', 'Medium', 'High'))
  
  expect_false(chk)
  
})
