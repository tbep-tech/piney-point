test_that("Checking if units in rswqdat match those in parms", {
  
  vars <- rswqdat %>% 
    select(var, uni) %>% 
    unique %>% 
    full_join(parms, by = c('var', 'uni'))
  
  chk <- anyNA(vars)
  
  expect_false(chk)
  
})
