test_that("Checking if benthic stations are in rsbntpts", {
  
  stas <- rsbntdat$station %>% 
    unique 
  
  chk <- any(!stas %in% rsbntpts$station)
  
  expect_false(chk)
  
})
