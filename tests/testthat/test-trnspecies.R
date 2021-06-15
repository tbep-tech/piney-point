test_that("Checking if transect species are correct", {
  
  chk <- anyNA(rstrndatsav$sav_species)
  
  expect_false(chk)
  
})
