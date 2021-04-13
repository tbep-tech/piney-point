test_that("Checking if stations in rswqdat in rsstatloc", {
  
  stas <- rswqdat$station %>% 
    unique 
  
  chk <- any(!stas %in% rsstatloc$station)
  
  expect_false(chk)
    
})
