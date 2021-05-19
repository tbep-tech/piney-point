test_that("Checking if sav species are correct", {
  
  chk <- anyNA(rstrndatsav$sav_species)
  
  expect_false(chk)
  
})
