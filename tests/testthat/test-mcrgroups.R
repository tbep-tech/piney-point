test_that("Checking if mcr groups are correct", {
  
  chk <- anyNA(rstrndatmcr$macroalgae_group)
  
  expect_false(chk)
  
})
