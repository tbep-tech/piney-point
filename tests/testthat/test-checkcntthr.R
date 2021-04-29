test_that("Checking if all parameters in rscntthr are in rscntdat", {
  
  vars <- rscntdat$var %>% 
    unique 
  
  chk <- any(!vars %in% rscntthr$var)
  
  expect_false(chk)
  
})
