# code styling works

    Code
      cat(as.character(styled))
    Output
      
      
      base::library(dplyr)
      
      data <- starwars %>% 
        dplyr::filter(hair_color == "blond" | dplyr::row_number() %in% 1:3) %>% 
        dplyr::mutate(dplyr::across(where(is.character), ~ stringr::str_count(.x))) %>% 
        dplyr::count(name)
      
      data
      
      1 %or% NULL

