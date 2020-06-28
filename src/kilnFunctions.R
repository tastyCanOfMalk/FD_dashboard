index_max_temp <- function(df = df){
  # MUST have (avg_kiln_temp) column
  return(median(which(max(df$avg_kiln_temp, na.rm=TRUE) == df$avg_kiln_temp)))
}

index_splice_beginning <- function(df = df,
                                   threshold = 100,
                                   lookahead = 40){
  # MUST have (setpoint, avg_kiln_temp) columns
  i <- df %>% 
    mutate(detect = ifelse( 
      (setpoint - avg_kiln_temp > threshold) & 
        (lead(setpoint, lookahead) - lead(avg_kiln_temp,lookahead) > threshold) & 
        (time < index_max_temp(df))
      ,TRUE, FALSE)) %>% 
    dplyr::filter(detect == TRUE) %>% 
    slice(1) %>% 
    dplyr::select(time)
  
  if(length(i[[1]]) == 0){return(1)}
  else(return(i[[1]]))
}

get_start_temp <- function(df = df){
  return(df$avg_kiln_temp[[1]])
}

index_splice_end <- function(df = df,
                             temp_threshold = 100,
                             setpoint_threshold = 1000){
  # MUST have (setpoint, avg_kiln_temp) columns
  i <- df %>% 
    mutate(detect = ifelse( 
      (avg_kiln_temp <= (get_start_temp(df) + temp_threshold)) & 
        (time > index_max_temp(df)) & 
        (setpoint < setpoint_threshold)
      ,TRUE ,FALSE)) %>% 
      dplyr::filter(detect == TRUE) %>% 
      slice(1) %>% 
      dplyr::select(time)
  
  if(length(i[[1]]) == 0){return(nrow(df))}
  else(return(i[[1]]))
  
}


