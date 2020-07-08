update_time <- function(df = df){
  # Mutate in a new time column
  # 
  # Args:
  #   df: dataframe to mutate
  # 
  # Returns:
  #   original df with additional time column
  return(
    df %>% 
      dplyr::mutate(time = seq(1:nrow(df)))
  )
}
  
index_max_temp <- function(df = df){
  # Finds index of max temperature value
  #   MUST have (avg_kiln_temp) column
  #
  # Args:
  #   df: dataframe to search
  #
  # Returns: 
  #   index of max temperature value
  
  return(
    median(
      which(
        max(df$avg_kiln_temp, na.rm=TRUE) == df$avg_kiln_temp
        )
      )
    )
}

get_start_temp <- function(df = df){
  # Finds beginning temperature value 
  #   MUST have (avg_kiln_temp) column
  #
  # Args:
  #   df: dataframe to search
  #
  # Returns: 
  #   temperature value at of first datapoint
  
  return(df$avg_kiln_temp[[1]])
}

get_data <- function(files = "files1"){
  # Compile csv log data contained in the 'files' variable
  #   add lotno and fix date columns
  #
  # Args:
  #   files: list of CSV files to read
  #
  # Returns: 
  #   tibble of data with new date and lotno columns
  
  df <- ldply(unlist(files), get_files) %>%
    plyr::mutate(LOTNO = lotno) %>%
    plyr::mutate(new_date_B = as.Date(date, format = "%m/%d/%Y")) %>%
    plyr::mutate(new_date_A = as.Date(date, format = "%Y-%m-%d")) %>%
    plyr::mutate(date = (ifelse(is.na(new_date_B), as.character(new_date_A), as.character(new_date_B)))) %>%
    dplyr::select(-c(new_date_B, new_date_A)) %>% 
    as_tibble()
  
  return(df)
}

index_max_setpoint_change <- function(df = df,
                                      threshold = 5,
                                      lookahead = 1,
                                      lookahead2 = 1,
                                      lookahead3 = 1){
  # Return first maximum value of a change in temperature setpoint
  # 
  # Args: 
  #   df: the dataframe
  #   threshold: increase in setpoint required to trigger
  #   lookahead: number of datapoints to look ahead when calculation setpoint change
  #   lookahead2: second lookahead to ensure data is rising (rather than blip)
  # 
  # Returns: 
  #   index of the first change in setpoint
  
  i <- df %>% 
    mutate(
      detect = ifelse(
        (time < index_max_temp(df)) &
        (setpoint - lead(setpoint, lookahead ) < -threshold) & 
        (setpoint - lead(setpoint, lookahead2) < -threshold) & 
        (setpoint - lead(setpoint, lookahead3) < -threshold), 
        TRUE, FALSE)) %>% 
    dplyr::filter(detect == TRUE) %>% 
    slice(1) %>% 
    dplyr::select(time)
  
  if(length(i[[1]]) == 0){return(1)}
  else(return(i[[1]]))
}

index_splice_end <- function(df = df,
                             temp_threshold = 100,
                             setpoint_threshold = 1000,
                             setpoint_change_lookahead = 1,
                             setpoint_change_threshold = 1){
  # MUST have (setpoint, avg_kiln_temp) columns
  # 
  # Returns index when kiln temp and setpoint temp are below threshold values, 
  #   and when the setpoint is no longer changing
  # 
  # Args: 
  #   df: the dataframe
  #   temp_threshold: how close to the start temp needed in order to trigger
  #   setpoint_threshold: setpoint must be below this value
  #   setpoint_change_lookahead: number of points to lookahead for change in setpoint value
  #   setpoint_change_threshold: change required between setpoints
  # 
  # Returns: 
  #   index, after maximum temp index, of when kiln temp is below (start
  #   temp + threshold) and below (setpoint threshold) and
  #   when (setpoint - next setpoints) are below certain value
  #   or returns last index if none found
  
  i <- df %>% 
    mutate(detect = ifelse( 
      (time > (index_max_temp(df) + 50)) & 
      (avg_kiln_temp <= (get_start_temp(df) + temp_threshold)) & 
      (setpoint < setpoint_threshold) & 
      (setpoint - lead(setpoint, setpoint_change_lookahead) < setpoint_change_threshold),
      TRUE, FALSE)) %>% 
    dplyr::filter(detect == TRUE) %>% 
    slice(1) %>% 
    dplyr::select(time)
  
  if(length(i[[1]]) == 0){return(nrow(df))}
  else(return(i[[1]]))
  
}

select_mutate <- function(df = kilns_AB){
  return(
    df %>% 
      dplyr::select(date,kiln,LOTNO,time,avg_kiln_temp,setpoint,
                    everything()) %>% 
      dplyr::mutate(date = as.Date(date), 
                    kiln = as.factor(kiln), 
                    LOTNO = as.factor(LOTNO))
  )
}


add_plot_splices <- function(df = df){
  # returns df with columsn including beginning and end splices for easy plotting
  return(
    df %>% 
      dplyr::mutate(
        splice_a = splice_beginning,
        splice_b = splice_end
      )
  )
}

plot_range <- function(df = kilns_AB,
                       start = 1,
                       end = 25,
                       filter = NA,
                       plotly_on = FALSE,
                       splices = FALSE){
  
  lotno_levels = levels(unlist(df %>% dplyr::select(LOTNO)))
  
  if(is.na(filter) & !splices & !plotly_on){
    df %>% 
      dplyr::filter(LOTNO %in% lotno_levels[start:end]) %>% 
      group_by(LOTNO) %>% 
      ggplot(aes(x=time, y=avg_kiln_temp, color = LOTNO))+
      geom_point(size = .8)+
      geom_line(aes(y= setpoint), color = 'black')+
      theme(legend.position = "none")+
      facet_wrap(~LOTNO, scales = "free")
  }
  else if(is.na(filter) & splices & !plotly_on){
    df %>% 
      dplyr::filter(LOTNO %in% lotno_levels[start:end]) %>% 
      group_by(LOTNO) %>% 
      ggplot(aes(x=time, y=avg_kiln_temp, color = LOTNO))+
      geom_point(size = .8)+
      geom_line(aes(y= setpoint), color = 'black')+
      geom_vline(aes(xintercept=splice_a),color='red')+
      geom_vline(aes(xintercept=splice_b),color='blue')+
      theme(legend.position = "none")+
      facet_wrap(~LOTNO, scales = "free")
  }
  else if(!is.na(filter) & !splices & !plotly_on){
    df %>% 
      dplyr::filter(LOTNO == filter) %>% 
      group_by(LOTNO) %>% 
      ggplot(aes(x=time, y=avg_kiln_temp, color = LOTNO))+
      geom_point(size = .8)+
      geom_line(aes(y= setpoint), color = 'black')+
      theme(legend.position = "none")+
      facet_wrap(~LOTNO, scales = "free")
  }
  else if(!is.na(filter) & splices & !plotly_on){
    df %>% 
      dplyr::filter(LOTNO == filter) %>% 
      group_by(LOTNO) %>% 
      ggplot(aes(x=time, y=avg_kiln_temp, color = LOTNO))+
      geom_point(size = .8)+
      geom_line(aes(y= setpoint), color = 'black')+
      theme(legend.position = "none")+
      geom_vline(aes(xintercept=splice_a),color='red')+
      geom_vline(aes(xintercept=splice_b),color='blue')+
      facet_wrap(~LOTNO, scales = "free")
  }
  else if(!is.na(filter) & plotly_on & !splices){
    ggplotly(
      df %>% 
        dplyr::filter(LOTNO == filter) %>% 
        group_by(LOTNO) %>% 
        ggplot(aes(x=time, y=avg_kiln_temp, color = LOTNO))+
        geom_point(size = .8)+
        geom_line(aes(y= setpoint), color = 'black')+
        theme(legend.position = "none")
    )
  }
  else if(!is.na(filter) & plotly_on & splices){
    ggplotly(
      df %>% 
        dplyr::filter(LOTNO == filter) %>%
        group_by(LOTNO) %>% 
        ggplot(aes(x=time, y=avg_kiln_temp, color = LOTNO))+
        geom_point(size = .8)+
        geom_line(aes(y= setpoint), color = 'black')+
        geom_vline(aes(xintercept=splice_a),color='red')+
        geom_vline(aes(xintercept=splice_b),color='blue')+
        theme(legend.position = "none")
    )
  }
  else(
    print("invalid flag combo")
  )
}

