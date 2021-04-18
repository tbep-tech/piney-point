test_that("Checking if stations in rscntdat in rsstatloc", {
  
  stas <- rscntdat$station %>% 
    unique 
  
  chk <- any(!stas %in% rsstatloc$station)
  
  expect_false(chk)
  
})
