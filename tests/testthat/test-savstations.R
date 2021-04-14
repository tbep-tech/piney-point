test_that("Checking if sav stations are in rstrnpts", {
  
  stas <- rstrndatsav$station %>% 
    unique 
  
  chk <- any(!stas %in% rstrnpts$station)
  
  expect_false(chk)
  
})
