test_that("Checking if macroalgae stations are in rstrnpts", {
  
  stas <- rstrndatmcr$station %>% 
    unique 
  
  chk <- any(!stas %in% rstrnpts$station)
  
  expect_false(chk)
  
})
