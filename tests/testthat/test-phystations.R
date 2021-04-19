test_that("Checking if phytoplankton stations are in rsstatloc", {
  
  stas <- rsphydat$station %>% 
    unique 
  
  chk <- any(!stas %in% rsstatloc$station)
  
  expect_false(chk)
  
})
