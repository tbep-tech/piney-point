test_that("Checking if any source names in rswqdat not in rsstatloc", {
  
  src <- rswqdat$source %>% 
    unique 
  
  chk <- any(!src %in% rsstatloc$source)
  
  expect_false(chk)
  
})
