test_that("Checking if phytoplankton stations are in rsphypts", {
  
  stas <- rsphydat$station %>% 
    unique 
  
  chk <- any(!stas %in% rsphypts$station)
  
  expect_false(chk)
  
})
