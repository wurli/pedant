test_that("code styling works", {
  
  styled <- style_code(test_code1, c("base", "dplyr"))
  
  expect_snapshot(cat(as.character(styled)))
  
})
