test_that("Checking if transect stations are in rstrnpts", {
  
  stas <- rstrndat$station %>% 
    unique 
  
  chk <- any(!stas %in% rstrnpts$station)
  
  expect_false(chk)
  
})
