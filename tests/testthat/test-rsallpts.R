test_that("Check if any stations in rsallpts are not in others", {

  stas <- rsallpts$station
  
  trnstas <- rstrnpts %>% 
    pull(station) %>% 
    unique
  wqstas <- rsstatloc %>% # same as contaminants
    pull(station) %>% 
    unique
  phystas <- rsphypts %>% 
    pull(station) %>% 
    unique
  bntstas <- rsbntpts %>% 
    pull(station) %>% 
    unique
  
  otherstas <- c(trnstas, wqstas, phystas, bntstas) %>% 
    unique
  
  chk <- setdiff(stas, otherstas) %>% 
    length %>% 
    `>`(0)
  
  expect_false(chk)
    
})
