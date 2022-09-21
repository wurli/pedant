test_that("code styling works", {
  
  code <- '
  
  library(dplyr)
  
  data <- starwars %>% 
    filter(hair_color == "blond" | row_number() %in% 1:3) %>% 
    mutate(across(where(is.character), ~ stringr::str_count(.x))) %>% 
    count(name)
  
  data
  
  1 %or% NULL
  
  '
  
  styled <- style_code(code, c("base", "dplyr"))
  
  expect_snapshot(cat(as.character(styled)))
  
})
