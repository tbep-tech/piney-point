test_that("Checking if transect stations for weight data are in rstrnpts", {
  
  stas <- rstrnwts$station %>% 
    unique 
  
  chk <- any(!stas %in% c(bstransect$Transect, rstrnpts$station))
  
  expect_false(chk)
  
})
