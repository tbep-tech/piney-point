test_that("Checking if phytoplankton sources are in rsstatloc", { # required for rsphydat1 reactive
  
  srcdat <- rsphydat$source %>% 
    unique
  
  chk <- any(!srcdat %in% rsstatloc$source)
  
  expect_false(chk)
  
})
