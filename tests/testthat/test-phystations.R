test_that("Checking if phytoplankton stations are in rsphypts", {
  
  stasdat <- rsphydat$station %>% 
    unique 
  
  staspts <- rsphypts$station %>% 
    unique
  
  chk <- setdiff(stasdat, staspts) %>% 
    length %>% 
    `>`(0)
  
  expect_false(chk)
  
})
