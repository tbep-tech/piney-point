test_that("Check if 999999 in rswqdat value", {
  
  chk <- any(999999 %in% rswqdat$val)
  
  expect_false(chk)
  
})

test_that("Check if NA in rswqdat value", {
  
  chk <- anyNA(rswqdat$val)
  
  expect_false(chk)
  
})

