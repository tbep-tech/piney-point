test_that("Checking for duplicate stations by source in rsstatloc", {
  
  dups <- rsstatloc %>% 
    select(source, station) %>% 
    unique %>% 
    group_by(source) %>% 
    nest() %>% 
    tibble::deframe() %>% 
    lapply(duplicated) %>% 
    tibble::enframe() %>% 
    unnest(value)
    
  chk <- any(dups$value)
  
  expect_false(chk)
  
})
