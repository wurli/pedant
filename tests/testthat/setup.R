test_code1 <- '

library(dplyr)

data <- starwars %>% 
  filter(hair_color == "blond" | row_number() %in% 1:3) %>% 
  mutate(across(where(is.character), ~ stringr::str_count(.x))) %>% 
  count(name)

data

1 %or% NULL

'