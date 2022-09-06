notify_n_replacements <- function(pd) {
  
  msg_data <- count(pd, "package", "text", "new_text")
  
  msg_data$s <- ifelse(msg_data$n == 1, " ", "s")
  
  msg <- glue::glue_data(
    msg_data,
    "{comma(n, TRUE)} occurrence{s} {justify(text, 'right')} -> {new_text}"
  )
  
  msg
  
}

