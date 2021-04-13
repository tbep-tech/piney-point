test_that("Checking if variables in rswqdat in parms", {
  
  vars <- rswqdat$var%>% 
    unique 
  
  chk <- any(!vars %in% parms$var)
  
  expect_false(chk)
  
})
