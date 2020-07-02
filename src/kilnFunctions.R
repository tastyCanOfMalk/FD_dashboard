index_max_temp <- function(df = df){
  # MUST have (avg_kiln_temp) column
  return(median(which(max(df$avg_kiln_temp, na.rm=TRUE) == df$avg_kiln_temp)))
}

index_splice_beginning <- function(df = df,
                                   threshold = 100,
                                   lookahead = 40){
  # KILNS: G,H
  # Summary: start time of beginning run seems to occur when the setpoint jumps far above the actual temp,
    # followed by a roughly 1-hour long period of holding that temp
  # threshold = detection limit for (setpoint - avg kiln temp)
  # lookahead = number of points to lookahead to confirm baseline
  # MUST have (setpoint, avg_kiln_temp) columns
  i <- df %>% 
    mutate(detect = ifelse( 
      (setpoint - avg_kiln_temp > threshold) & 
        (lead(setpoint, lookahead) - lead(avg_kiln_temp, lookahead) > threshold) & 
        (time < index_max_temp(df))
      ,TRUE, FALSE)) %>% 
    dplyr::filter(detect == TRUE) %>% 
    slice(1) %>% 
    dplyr::select(time)
  
  if(length(i[[1]]) == 0){return(1)}
  else(return(i[[1]]))
}

index_splice_beginning_F <- function(df = df,
                                     threshold = 5,
                                     lookahead = 40){
  # KILNS: F
  # Summary: start of run generally signalled by prolonged baseline followed by jump in 
    # temperature slope, find jump in slope and label
  # threshold = detection limit for jump in setpoints
  # lookahead = number of points to lookahead to confirm baseline
  # MUST have (setpoint, avg_kiln_temp) columns
  i <- df %>% 
    mutate(detect = ifelse( 
      (setpoint - lead(setpoint, 1) > threshold) & 
        (time < index_max_temp(df))
      ,TRUE, FALSE)) %>% 
    dplyr::filter(detect == TRUE) %>% 
    slice(1) %>% 
    dplyr::select(time)
  
  if(length(i[[1]]) == 0){return(1)}
  else(return(i[[1]]))
}

df




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

get_data <- function(files = "files1"){
  
  df <- ldply(unlist(files), get_files) %>%
    plyr::mutate(LOTNO = lotno) %>%
    plyr::mutate(new_date_B = as.Date(date, format = "%m/%d/%Y")) %>%
    plyr::mutate(new_date_A = as.Date(date, format = "%Y-%m-%d")) %>%
    plyr::mutate(date = (ifelse(is.na(new_date_B), as.character(new_date_A), as.character(new_date_B)))) %>%
    dplyr::select(-c(new_date_B, new_date_A)) %>% 
    as_tibble()
  
  return(df)
  
}
