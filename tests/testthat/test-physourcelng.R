test_that("Checking if phytoplankton long sources are present",{
  
  srcdat <- rsphypts$source_lng %>% 
    unique
  
  chk <- anyNA(srcdat)
  
  expect_false(chk)
  
})

