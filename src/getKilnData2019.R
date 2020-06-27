source("src/cleanCombine.R")
source("src/userFunctions.R")

library(plotly)

# KILNS A, B ---------------------------------------------------------------------
kilns_dir <- "data/kiln/Kiln Run Data_2019"
kilns <- list.dirs(kilns_dir, recursive = FALSE)

mylist <- list()
kilns_AB <- tibble()

for(kiln in kilns[1:2]){
  # loop thru each primrary kiln folder
  kiln_name <- str_split(str_split(kiln, "/")[[1]][length(str_split(kiln, "/")[[1]])], "-")[[1]][1]
  
  for(sub_dir in list.dirs(kiln)[2:length(list.dirs(kiln))]){
    # loop thru each lot in kiln folder
    lotno <- str_split(str_split(sub_dir, "/")[[1]][length(str_split(sub_dir, "/")[[1]])], " ")[[1]][2]
    lotno <- paste0(lotno, kiln_name)
    
    # print(paste0(sub_dir, "/", list.files(sub_dir, pattern = "CSV")))
    # print("--------------------")
    files <- list(paste0(sub_dir, "/", list.files(sub_dir, pattern = "CSV")))
    
    # paste lotno, convert dates
    df <- ldply(unlist(files), get_files) %>% 
      mutate(LOTNO = lotno) %>% 
      mutate(new_date_B = as.Date(date, format = "%m/%d/%Y")) %>%
      mutate(new_date_A = as.Date(date, format = "%Y-%m-%d")) %>% 
      mutate(date = (ifelse(is.na(new_date_B), as.character(new_date_A), as.character(new_date_B)))) %>% 
      dplyr::select(-c(new_date_B, new_date_A))
    
    # get setpoint diffs, store max
    df <- df %>% 
      arrange(date, time) %>%
      mutate(
        diff_setpoint = temp_setpoint - lag(temp_setpoint),
        max_diff_setpoint = max(diff_setpoint, na.rm = TRUE)
      )

    # assign first splice location based on max_diff_setpoint, handle exceptions
    if( lotno == "120519A" ){ splice_start = 324 }
    if( lotno == "012019B" ){ splice_start = 70}
    else { splice_start <- which(df$diff_setpoint == df$max_diff_setpoint) }

    # splice beginning
    df <- df[splice_start:nrow(df),]

    # find second splice location based on reaching beginning temp
    df <- df %>% 
      mutate(start_temp = kilntemp[1],
             max_temp = max(kilntemp, na.rm=TRUE),
             find_end_temp = abs(kilntemp - start_temp))
    
    max_temp_index <- which(df$kilntemp == df$max_temp)[1]
    temp_index     <- which.min(df$find_end_temp[max_temp_index:nrow(df)])

    # find splice end, handle exceptions
    if( lotno == "012019B" ){ splice_end = 1693 }
    else if( lotno == "012319A" ){ splice_end = 2525 }
    else if( lotno == "020219B" ){ splice_end = 1673 }
    else if( lotno == "020919B" ){ splice_end = 1668 }
    else {splice_end <- max_temp_index + temp_index }
    
    df <- df[1:splice_end,]

    # produce new time axis
    df <- df %>% 
      mutate(new_time = seq(1:nrow(df))) %>% 
      mutate(kiln = kiln_name) %>% 
      as_tibble()
    
    # bind individual lot df to parent df
    kilns_AB <- bind_rows(kilns_AB, df)
    
  }
}
 
# names(kilns_AB)
kilns_AB <- kilns_AB %>% 
  dplyr::select(c(LOTNO,kiln,date,time,new_time,everything())) %>% 
  dplyr::select(-c(diff_setpoint, max_diff_setpoint, start_temp, find_end_temp)) %>%
  mutate(LOTNO = as.factor(LOTNO))
# names(kilns_AB)

# testing
# levels(kilns_AB$LOTNO)

# view many plots
# t <- kilns_AB %>% 
#   # dplyr::filter(LOTNO %in% levels(kilns_AB$LOTNO)) %>%
#   dplyr::filter(LOTNO %in% levels(kilns_AB$LOTNO)[101:120]) %>%
#   # dplyr::filter(LOTNO %in% levels(kilns_AB$LOTNO)[1:25]) %>%
#   # dplyr::filter(LOTNO %in% levels(kilns_AB$LOTNO)[26:50]) %>%
#   # dplyr::filter(LOTNO %in% levels(kilns_AB$LOTNO)[51:55]) %>%
#   group_by(LOTNO) %>% 
#   ggplot(aes(x=new_time, y = kilntemp, color = LOTNO))+
#   # ggplot(aes(x=new_time, y = temp_setpoint, color = LOTNO))+
#   # ggplot(aes(x=new_time, y = kilnpress, color = LOTNO))+
#   geom_point(size = .8)+
#   # geom_vline(aes(xintercept = splice_end))+
#   theme(legend.position = "none")+
#   facet_wrap(~LOTNO, scales = "free")
# 
# t
# ggplotly(t)


# KILNS C ------------------------------------------------------------------
# mylist <- list()
kilns_C <- tibble()

for(kiln in kilns[c(3)]){
  # loop thru each primrary kiln folder
  kiln_name <- str_split(str_split(kiln, "/")[[1]][length(str_split(kiln, "/")[[1]])], "-")[[1]][1]
    
  for(sub_dir in list.dirs(kiln)[2:length(list.dirs(kiln))]){
    # loop thru each lot in kiln folder
    lotno <- str_split(sub_dir, "/")[[1]][length(str_split(sub_dir, "/")[[1]])]
    lotno <- paste0(lotno, kiln_name)
    
    # print(paste0(sub_dir, "/", list.files(sub_dir, pattern = "1.CSV")))
    # print("--------------------")
    files1 <- list(paste0(sub_dir, "/", list.files(sub_dir, pattern = "1.CSV")))
    files2 <- list(paste0(sub_dir, "/", list.files(sub_dir, pattern = "2.CSV")))

    # paste lotno, convert dates
    df1 <- ldply(unlist(files1), get_files) %>%
      mutate(LOTNO = lotno) %>%
      mutate(new_date_B = as.Date(date, format = "%m/%d/%Y")) %>%
      mutate(new_date_A = as.Date(date, format = "%Y-%m-%d")) %>%
      mutate(date = (ifelse(is.na(new_date_B), as.character(new_date_A), as.character(new_date_B)))) %>%
      dplyr::select(-c(new_date_B, new_date_A)) %>% 
      as_tibble()
    
    df2 <- ldply(unlist(files2), get_files) %>%
      mutate(LOTNO = lotno) %>%
      mutate(new_date_B = as.Date(date, format = "%m/%d/%Y")) %>%
      mutate(new_date_A = as.Date(date, format = "%Y-%m-%d")) %>%
      mutate(date = (ifelse(is.na(new_date_B), as.character(new_date_A), as.character(new_date_B)))) %>%
      dplyr::select(-c(new_date_B, new_date_A)) %>% 
      as_tibble
    
    df <- left_join(df1,df2,by=c("LOTNO", "date", "time"))
    
    df <- df %>% 
      mutate(diff_setpoint = temp_sp - lag(temp_sp),
             max_diff_setpoint = max(diff_setpoint, na.rm=TRUE))
    df <- df %>% 
      mutate(diff_setpoint = ifelse(is.na(diff_setpoint), 0, diff_setpoint))
    
    # find splice beginning
    splice_beginning <- which(df$diff_setpoint > 20)[1]
    df <- df[splice_beginning:nrow(df),]
      
    df <- df %>% 
      mutate(new_time = seq(1:nrow(df)),
             avg_kiln_temp = rowMeans(select(., t_c_51a,t_c_51b,t_c_52a,t_c_52b)),
             start_temp = avg_kiln_temp[1],
             max_temp = max(avg_kiln_temp),
             temp_slope = avg_kiln_temp - lag(avg_kiln_temp),
             diff_start = start_temp - avg_kiln_temp,
             kiln = kiln_name)
    
    # find splice end
    max_temp_index <- which(df$avg_kiln_temp == df$max_temp)[[1]]

    temp_list <- list()
    for(i in seq(max_temp_index, nrow(df))){

      slope      <- df$temp_slope[i]
      diff_start <- df$diff_start[i]

      if( (slope > -10) & (diff_start > -40) ){
        temp_list <- append(temp_list, i)
      }
    }
    
    if(lotno == "090919C"){ splice_end = 3793 }
    else{ splice_end <- temp_list[[1]] }
    
    df <- df[1:splice_end,]
    
    # join lot df to primary df
    
    kilns_C <- bind_rows(kilns_C, df)
  }
}

# names(kilns_C)
kilns_C <- kilns_C %>% 
  dplyr::select(c(LOTNO,kiln,date,time,new_time,everything())) %>% 
  dplyr::select(-c(start_temp,temp_slope,diff_start,max_diff_setpoint,diff_setpoint)) %>% 
  mutate(LOTNO = as.factor(LOTNO))
# names(kilns_C)

# levels(kilns_C$LOTNO)

# view many plots
# kilns_C %>% 
#   # dplyr::filter(LOTNO %in% levels(kilns_C$LOTNO)) %>%
#   # dplyr::filter(LOTNO %in% levels(kilns_C$LOTNO)[1]) %>%
#   # dplyr::filter(LOTNO %in% levels(kilns_C$LOTNO)[101:120]) %>%
#   # dplyr::filter(LOTNO %in% levels(kilns_C$LOTNO)[1:25]) %>%
#   # dplyr::filter(LOTNO %in% levels(kilns_C$LOTNO)[26:50]) %>%
#   # dplyr::filter(LOTNO %in% levels(kilns_C$LOTNO)[51:55]) %>%
#   group_by(LOTNO) %>% 
#   # ggplot(aes(x=new_time, y = temp_sp, color = LOTNO))+
#   ggplot(aes(x=new_time, y = avg_kiln_temp, color = LOTNO))+
#   geom_point(size = .8)+
#   theme(legend.position = "none")+
#   facet_wrap(~LOTNO, scales = "free")


# KILNS D -----------------------------------------------------------------

kilns_D <- tibble()

for(kiln in kilns[c(4)]){
  # loop thru each primrary kiln folder
  kiln_name <- str_split(str_split(kiln, "/")[[1]][length(str_split(kiln, "/")[[1]])], "-")[[1]][1]
  
  for(sub_dir in list.dirs(kiln)[2:length(list.dirs(kiln))]){
    # loop thru each lot in kiln folder
    lotno <- str_split(str_split(sub_dir, "/")[[1]][length(str_split(sub_dir, "/")[[1]])], " ")[[1]][2]
    lotno <- paste0(lotno, kiln_name)
    
    # print(paste0(sub_dir, "/", list.files(sub_dir, pattern = "1.CSV")))
    # print("--------------------")
    files1 <- list(paste0(sub_dir, "/", list.files(sub_dir, pattern = "1.CSV")))
    files2 <- list(paste0(sub_dir, "/", list.files(sub_dir, pattern = "2.CSV")))
    
    # paste lotno, convert dates
    df1 <- ldply(unlist(files1), get_files) %>%
      mutate(LOTNO = lotno) %>%
      mutate(new_date_B = as.Date(date, format = "%m/%d/%Y")) %>%
      mutate(new_date_A = as.Date(date, format = "%Y-%m-%d")) %>%
      mutate(date = (ifelse(is.na(new_date_B), as.character(new_date_A), as.character(new_date_B)))) %>%
      dplyr::select(-c(new_date_B, new_date_A)) %>% 
      as_tibble()
    
    df2 <- ldply(unlist(files2), get_files) %>%
      mutate(LOTNO = lotno) %>%
      mutate(new_date_B = as.Date(date, format = "%m/%d/%Y")) %>%
      mutate(new_date_A = as.Date(date, format = "%Y-%m-%d")) %>%
      mutate(date = (ifelse(is.na(new_date_B), as.character(new_date_A), as.character(new_date_B)))) %>%
      dplyr::select(-c(new_date_B, new_date_A)) %>% 
      as_tibble
    
    df <- left_join(df1,df2,by=c("LOTNO", "date", "time"))
    
    df <- df %>%
      mutate(new_time = seq(1:nrow(df)),
             avg_kiln_temp = rowMeans(select(., bot_t_c_1 ,bot_t_c_2 ,top_t_c_3 ,top_t_c_4)),
             start_temp = avg_kiln_temp[1],
             diff_setpoint = setpoint - lag(setpoint),
             kiln = kiln_name,
             
             max_diff_setpoint = max(diff_setpoint, na.rm=TRUE),
             max_temp = max(avg_kiln_temp),
             
             temp_slope = avg_kiln_temp - lag(avg_kiln_temp),
             diff_start = start_temp - avg_kiln_temp,
             )
    
    # find splice beginning
    splice_beginning <- which(df$diff_setpoint > 20)[1]
    df <- df[splice_beginning:nrow(df),]

    # find splice end
    max_temp_index <- which(df$avg_kiln_temp == df$max_temp)[[1]]

    temp_list <- list()
    for(i in seq(max_temp_index, nrow(df))){
        slope      <- df$temp_slope[i]
        diff_start <- df$diff_start[i]

        if( (slope > .75) & (diff_start > -150) ){
          temp_list <- append(temp_list, i)
        }
    }
    
    splice_end <- temp_list[[1]]
    
    df <- df[1:splice_end,]
    
    # bind lot data to collective data
    kilns_D <- bind_rows(kilns_D, df)
  }
  
}

# names(kilns_D)
kilns_D <- kilns_D %>% 
  dplyr::select(c(LOTNO,kiln,date,time,new_time,everything())) %>% 
  dplyr::select(-c(start_temp,diff_setpoint,max_diff_setpoint,temp_slope,diff_start)) %>% 
  mutate(LOTNO = as.factor(LOTNO))
# names(kilns_D)

# kilns_D %>% 
#   # dplyr::filter(LOTNO %in% levels(kilns_D$LOTNO)) %>%
#   # dplyr::filter(LOTNO %in% levels(kilns_D$LOTNO)[1:5]) %>%
#    group_by(LOTNO) %>% 
#   ggplot(aes(x=new_time, y = setpoint ,color=LOTNO))+
#   geom_point(size = .8)+
#   # geom_line(aes(y=temp_slope),color='red')+
#   # geom_line(aes(y=diff_start),color='blue')+
#   theme(legend.position = "none")+
#   facet_wrap(~LOTNO, scales = "free")


# KILNS E -----------------------------------------------------------------

kilns_E <- tibble()

for(kiln in kilns[c(5)]){
  # loop thru each primrary kiln folder
  kiln_name <- str_split(str_split(kiln, "/")[[1]][length(str_split(kiln, "/")[[1]])], "-")[[1]][1]
  
  for(sub_dir in list.dirs(kiln)[2:length(list.dirs(kiln))]){
    # loop thru each lot in kiln folder
    lotno <- str_split(str_split(sub_dir, "/")[[1]][length(str_split(sub_dir, "/")[[1]])], " ")[[1]][2]
    lotno <- paste0(lotno, kiln_name)
    
    # print(paste0(sub_dir, "/", list.files(sub_dir, pattern = "1.CSV")))
    # print("--------------------")
    files1 <- list(paste0(sub_dir, "/", list.files(sub_dir, pattern = "1.CSV")))
    files2 <- list(paste0(sub_dir, "/", list.files(sub_dir, pattern = "2.CSV")))
    
    # paste lotno, convert dates
    df1 <- ldply(unlist(files1), get_files) %>%
      mutate(LOTNO = lotno) %>%
      mutate(new_date_B = as.Date(date, format = "%m/%d/%Y")) %>%
      mutate(new_date_A = as.Date(date, format = "%Y-%m-%d")) %>%
      mutate(date = (ifelse(is.na(new_date_B), as.character(new_date_A), as.character(new_date_B)))) %>%
      dplyr::select(-c(new_date_B, new_date_A)) %>% 
      as_tibble()
    
    df2 <- ldply(unlist(files2), get_files) %>%
      mutate(LOTNO = lotno) %>%
      mutate(new_date_B = as.Date(date, format = "%m/%d/%Y")) %>%
      mutate(new_date_A = as.Date(date, format = "%Y-%m-%d")) %>%
      mutate(date = (ifelse(is.na(new_date_B), as.character(new_date_A), as.character(new_date_B)))) %>%
      dplyr::select(-c(new_date_B, new_date_A)) %>% 
      as_tibble
    
    df <- left_join(df1,df2,by=c("LOTNO", "date", "time"))
    
    df <- df %>%
      mutate(new_time = seq(1:nrow(df)),
             avg_kiln_temp = rowMeans(select(., bot_t_c_1 ,bot_t_c_2 ,top_t_c_3 ,top_t_c_4)),
             start_temp = avg_kiln_temp[1],
             diff_setpoint = setpoint - lag(setpoint),
             kiln = kiln_name,
             
             max_diff_setpoint = max(diff_setpoint, na.rm=TRUE),
             max_temp = max(avg_kiln_temp),
             
             temp_slope = avg_kiln_temp - lag(avg_kiln_temp),
             diff_start = start_temp - avg_kiln_temp,
      )
    
    # find splice beginning
    if( lotno == "091619E"){splice_beginning = 3459 }
    else{ splice_beginning <- which(df$diff_setpoint > 20)[1] }

    df <- df[splice_beginning:nrow(df),]

    # find splice end
    max_temp_index <- which(df$avg_kiln_temp == df$max_temp)[[1]]

    temp_list <- list()
    for(i in seq(max_temp_index, nrow(df))){
      slope      <- df$temp_slope[i]
      diff_start <- df$diff_start[i]

      if( (slope > 1) & (diff_start > -100) ){
        temp_list <- append(temp_list, i)
      }
      else{
        temp_list <- append(temp_list, nrow(df))
      }
    }

    splice_end <- temp_list[[1]]

    df <- df[1:splice_end,]
    
    # bind lot data to collective data
    kilns_E <- bind_rows(kilns_E, df)
  }
}

# names(kilns_E)
kilns_E <- kilns_E %>%
  dplyr::select(c(LOTNO,kiln,date,time,new_time,everything())) %>%
  dplyr::select(-c(start_temp,diff_setpoint,max_diff_setpoint,temp_slope,diff_start)) %>%
  mutate(LOTNO = as.factor(LOTNO))
# # names(kilns_E)
# 
# kilns_E %>%
#   # dplyr::filter(LOTNO %in% levels(kilns_E$LOTNO)) %>%
#   # dplyr::filter(LOTNO %in% levels(kilns_E$LOTNO)[1:5]) %>%
#   group_by(LOTNO) %>%
#   # ggplot(aes(x=new_time, y = setpoint, color=LOTNO))+
#   ggplot(aes(x=new_time, y = avg_kiln_temp, color=LOTNO))+
#   geom_point(size = .8)+
#   # geom_line(aes(y=temp_slope),color='red')+
#   # geom_line(aes(y=diff_start),color='blue')+
#   theme(legend.position = "none")+
#   facet_wrap(~LOTNO, scales = "free")
# # ggplotly(t)
# 
# df %>% 
#   ggplot(aes(x=new_time,y=avg_kiln_temp))+
#   geom_line()


# KILNS F -----------------------------------------------------------------

kilns_F <- tibble()

for(kiln in kilns[c(6)]){
  # loop thru each primrary kiln folder
  kiln_name <- str_split(str_split(kiln, "/")[[1]][length(str_split(kiln, "/")[[1]])], "-")[[1]][1]
  
  for(sub_dir in list.dirs(kiln)[2:length(list.dirs(kiln))]){
    # loop thru each lot in kiln folder
    lotno <- str_split(sub_dir, "/")[[1]][length(str_split(sub_dir, "/")[[1]])]
    lotno <- paste0(lotno, kiln_name)
    
    # print(paste0(sub_dir, "/", list.files(sub_dir, pattern = "1.CSV")))
    # print("--------------------")
    files1 <- list(paste0(sub_dir, "/", list.files(sub_dir, pattern = "1.CSV")))
    files2 <- list(paste0(sub_dir, "/", list.files(sub_dir, pattern = "2.CSV")))
    
    # paste lotno, convert dates
    df1 <- ldply(unlist(files1), get_files) %>%
      mutate(LOTNO = lotno) %>%
      mutate(new_date_B = as.Date(date, format = "%m/%d/%Y")) %>%
      mutate(new_date_A = as.Date(date, format = "%Y-%m-%d")) %>%
      mutate(date = (ifelse(is.na(new_date_B), as.character(new_date_A), as.character(new_date_B)))) %>%
      dplyr::select(-c(new_date_B, new_date_A)) %>% 
      as_tibble()
    
    df2 <- ldply(unlist(files2), get_files) %>%
      mutate(LOTNO = lotno) %>%
      mutate(new_date_B = as.Date(date, format = "%m/%d/%Y")) %>%
      mutate(new_date_A = as.Date(date, format = "%Y-%m-%d")) %>%
      mutate(date = (ifelse(is.na(new_date_B), as.character(new_date_A), as.character(new_date_B)))) %>%
      dplyr::select(-c(new_date_B, new_date_A)) %>% 
      as_tibble
    
    df <- left_join(df1,df2,by=c("LOTNO", "date", "time"))
    
    df <- df %>%
      mutate(new_time = seq(1:nrow(df)),
             avg_kiln_temp = rowMeans(select(., bot_t_c_1 ,bot_t_c_2 ,top_t_c_3 ,top_t_c_4)),
             start_temp = avg_kiln_temp[1],
             kiln = kiln_name,
             
             diff_setpoint = setpoint - lag(setpoint),
             max_diff_setpoint = max(diff_setpoint, na.rm=TRUE),
             max_temp = max(avg_kiln_temp),
             
             temp_slope = avg_kiln_temp - lag(avg_kiln_temp),
             diff_start = start_temp - avg_kiln_temp,
      )
    
    # find splice beginning
    if( lotno == "081619F") { splice_beginning = 525}
    else{splice_beginning <- which(df$diff_setpoint == df$max_diff_setpoint)[1]}

    df <- df[splice_beginning:nrow(df),]
    
    # bind lot data to collective data
    kilns_F <- bind_rows(kilns_F, df)
  }
}

# names(kilns_F)
kilns_F <- kilns_F %>%
  dplyr::select(c(LOTNO,kiln,date,time,new_time,everything())) %>%
  dplyr::select(-c(start_temp,diff_setpoint,max_diff_setpoint,temp_slope,diff_start)) %>%
  mutate(LOTNO = as.factor(LOTNO))
# # names(kilns_F)

# kilns_F %>%
#   # dplyr::filter(LOTNO %in% levels(kilns_F$LOTNO)) %>%
#   # dplyr::filter(LOTNO %in% levels(kilns_F$LOTNO)[31:41]) %>%
#   group_by(LOTNO) %>%
#   # ggplot(aes(x=new_time, y = setpoint, color=LOTNO))+
#   ggplot(aes(x=new_time, y = avg_kiln_temp, color=LOTNO))+
#   geom_point(size = .8)+
#   # geom_line(aes(y=temp_slope),color='red')+
#   # geom_line(aes(y=diff_start),color='blue')+
#   theme(legend.position = "none")+
#   facet_wrap(~LOTNO)
# ggplotly(t)

# KILNS G -----------------------------------------------------------------

kilns_G <- tibble()

for(kiln in kilns[c(7)]){
  # loop thru each primrary kiln folder
  kiln_name <- str_split(str_split(kiln, "/")[[1]][length(str_split(kiln, "/")[[1]])], "-")[[1]][1]
  
  for(sub_dir in list.dirs(kiln)[2:length(list.dirs(kiln))]){
    # loop thru each lot in kiln folder
    lotno <- str_split(sub_dir, "/")[[1]][length(str_split(sub_dir, "/")[[1]])]
    lotno <- paste0(lotno, kiln_name)
    
    # print(paste0(sub_dir, "/", list.files(sub_dir, pattern = "1.CSV")))
    # print("--------------------")
    files1 <- list(paste0(sub_dir, "/", list.files(sub_dir, pattern = "1.CSV")))
    files2 <- list(paste0(sub_dir, "/", list.files(sub_dir, pattern = "2.CSV")))
    files3 <- list(paste0(sub_dir, "/", list.files(sub_dir, pattern = "3.CSV")))
    files4 <- list(paste0(sub_dir, "/", list.files(sub_dir, pattern = "4.CSV")))
    
    # paste lotno, convert dates
    df1 <- ldply(unlist(files1), get_files) %>%
      mutate(LOTNO = lotno) %>%
      mutate(new_date_B = as.Date(date, format = "%m/%d/%Y")) %>%
      mutate(new_date_A = as.Date(date, format = "%Y-%m-%d")) %>%
      mutate(date = (ifelse(is.na(new_date_B), as.character(new_date_A), as.character(new_date_B)))) %>%
      dplyr::select(-c(new_date_B, new_date_A)) %>% 
      as_tibble()
    
    df2 <- ldply(unlist(files2), get_files) %>%
      mutate(LOTNO = lotno) %>%
      mutate(new_date_B = as.Date(date, format = "%m/%d/%Y")) %>%
      mutate(new_date_A = as.Date(date, format = "%Y-%m-%d")) %>%
      mutate(date = (ifelse(is.na(new_date_B), as.character(new_date_A), as.character(new_date_B)))) %>%
      dplyr::select(-c(new_date_B, new_date_A)) %>% 
      as_tibble
    
    df3 <- ldply(unlist(files3), get_files) %>%
      mutate(LOTNO = lotno) %>%
      mutate(new_date_B = as.Date(date, format = "%m/%d/%Y")) %>%
      mutate(new_date_A = as.Date(date, format = "%Y-%m-%d")) %>%
      mutate(date = (ifelse(is.na(new_date_B), as.character(new_date_A), as.character(new_date_B)))) %>%
      dplyr::select(-c(new_date_B, new_date_A)) %>% 
      as_tibble
    
    df4 <- ldply(unlist(files4), get_files) %>%
      mutate(LOTNO = lotno) %>%
      mutate(new_date_B = as.Date(date, format = "%m/%d/%Y")) %>%
      mutate(new_date_A = as.Date(date, format = "%Y-%m-%d")) %>%
      mutate(date = (ifelse(is.na(new_date_B), as.character(new_date_A), as.character(new_date_B)))) %>%
      dplyr::select(-c(new_date_B, new_date_A)) %>% 
      as_tibble
    
    df <- left_join(df1,df2,by=c("LOTNO", "date", "time"))
    df <- left_join(df,df3,by=c("LOTNO", "date", "time"))
    df <- left_join(df,df4,by=c("LOTNO", "date", "time"))
    
    df <- df %>%
      mutate(new_time = seq(1:nrow(df)),
             avg_top_kiln_temp = rowMeans(select(., tc_t1,tc_t2,tc_t3,tc_t4)),
             avg_bot_kiln_temp = rowMeans(select(., tc_b1,tc_b2,tc_b3,tc_b4)),
             avg_kiln_temp     = rowMeans(select(., tc_t1,tc_t2,tc_t3,tc_t4,tc_b1,tc_b2,tc_b3,tc_b4)),
             start_temp = avg_kiln_temp[1],
             diff_setpoint = tmprsp - lag(tmprsp),
             kiln = kiln_name,

             max_diff_setpoint = max(diff_setpoint, na.rm=TRUE),
             max_temp = max(avg_kiln_temp),

             temp_slope = avg_kiln_temp - lag(avg_kiln_temp),
             diff_start = start_temp - avg_kiln_temp,
             
      ) %>% 
      # fix for lotno 120419G bottom TC's failing 
      mutate(avg_kiln_temp = ifelse(is.na(avg_bot_kiln_temp), avg_top_kiln_temp, avg_kiln_temp),
             max_temp = max(avg_kiln_temp),
             temp_slope = avg_kiln_temp - lag(avg_kiln_temp),
             diff_start = start_temp - avg_kiln_temp
             )
      

    # # find splice beginning
    if( lotno == "012219G"){ splice_beginning = 1148 }
    else if( lotno == "031919G"){splice_beginning = 1320 }
    else if( lotno == "050419G"){splice_beginning = 97 }
    else if( lotno == "051419G"){splice_beginning = 833 }
    else if( lotno == "052919G"){splice_beginning = 36 }
    else if( lotno == "062219G"){splice_beginning = 78 }
    else if( lotno == "072319G"){splice_beginning = 1340 }
    else if( lotno == "091819G"){splice_beginning = 21 }
    else if( lotno == "092119G"){splice_beginning = 726 }
    else if( lotno == "120419G"){splice_beginning = 186 }
    else{ splice_beginning <- which(df$diff_setpoint == df$max_diff_setpoint)[1] }

    df <- df[splice_beginning:nrow(df),]
    
    df <- df %>% 
      mutate(new_time = seq(1:nrow(df)))
    
    # find splice end
    max_temp_index <- which(df$avg_kiln_temp == df$max_temp)[[1]]

    temp_list <- list()
    for(i in seq(max_temp_index, nrow(df))){
      slope      <- df$temp_slope[i]
      diff_start <- df$diff_start[i]

      if(lotno %in% c("010319G","030219G","072719G","091419G","100519G","110119G","112919G","120719G")){
        temp_list <- append(temp_list, nrow(df))
      }
      else if( (slope > -1) & (diff_start > -200) ){
        temp_list <- append(temp_list, i)
      }
      # else{
      #   temp_list <- append(temp_list, nrow(df))
      # }
      
    }

    if(lotno == "120419G"){spice_end <- 3500}
    else{splice_end <- temp_list[[1]]}

    df <- df[1:splice_end,]
    
    # bind lot data to collective data
    kilns_G <- bind_rows(kilns_G, df)
  }
}

# names(kilns_G)
kilns_G <- kilns_G %>%
  dplyr::select(c(LOTNO,kiln,date,time,new_time,everything())) %>%
  dplyr::select(-c(start_temp,diff_setpoint,max_diff_setpoint,temp_slope,diff_start)) %>%
  mutate(LOTNO = as.factor(LOTNO))
# # names(kilns_G)
# 
# levels(kilns_G$LOTNO)
# kilns_G %>%
#   # dplyr::filter(LOTNO %in% a) %>%
#   # dplyr::filter(LOTNO %in% levels(kilns_G$LOTNO)) %>%
#   # dplyr::filter(LOTNO %in% levels(kilns_G$LOTNO)[1:25]) %>%
#   # dplyr::filter(LOTNO %in% levels(kilns_G$LOTNO)[1:50]) %>%
#   # dplyr::filter(LOTNO %in% levels(kilns_G$LOTNO)[51:100]) %>%
#   # dplyr::filter(LOTNO %in% levels(kilns_G$LOTNO)[101:150]) %>%
#   group_by(LOTNO) %>%
#   # ggplot(aes(x=new_time, y = setpoint, color=LOTNO))+
#   ggplot(aes(x=new_time, y = avg_kiln_temp, color=LOTNO))+
#   geom_point(size = .8)+
#   # geom_line(aes(y=temp_slope),color='red')+
#   # geom_line(aes(y=diff_start),color='blue')+
#   # geom_line(aes(y=diff_setpoint),color='blue')+
#   theme(legend.position = "none")+
#   facet_wrap(~LOTNO,scales="free")
# ggplotly(t)

# KILNS H -----------------------------------------------------------------

kilns_H <- tibble()

for(kiln in kilns[c(8)]){
  # loop thru each primrary kiln folder
  kiln_name <- str_split(str_split(kiln, "/")[[1]][length(str_split(kiln, "/")[[1]])], "-")[[1]][1]
  
  for(sub_dir in list.dirs(kiln)[2:length(list.dirs(kiln))]){
    # loop thru each lot in kiln folder
    lotno <- str_split(str_split(sub_dir, "/")[[1]][length(str_split(sub_dir, "/")[[1]])], " ")[[1]][2]
    lotno <- paste0(lotno, kiln_name)
    
    # print(paste0(sub_dir, "/", list.files(sub_dir, pattern = "1.CSV")))
    # print("--------------------")
    files1 <- list(paste0(sub_dir, "/", list.files(sub_dir, pattern = "1.CSV")))
    files2 <- list(paste0(sub_dir, "/", list.files(sub_dir, pattern = "2.CSV")))
    files3 <- list(paste0(sub_dir, "/", list.files(sub_dir, pattern = "3.CSV")))
    files4 <- list(paste0(sub_dir, "/", list.files(sub_dir, pattern = "6.CSV")))
    
    # paste lotno, convert dates
    df1 <- ldply(unlist(files1), get_files) %>%
      mutate(LOTNO = lotno) %>%
      mutate(new_date_B = as.Date(date, format = "%m/%d/%Y")) %>%
      mutate(new_date_A = as.Date(date, format = "%Y-%m-%d")) %>%
      mutate(date = (ifelse(is.na(new_date_B), as.character(new_date_A), as.character(new_date_B)))) %>%
      dplyr::select(-c(new_date_B, new_date_A)) %>% 
      as_tibble()
    
    df2 <- ldply(unlist(files2), get_files) %>%
      mutate(LOTNO = lotno) %>%
      mutate(new_date_B = as.Date(date, format = "%m/%d/%Y")) %>%
      mutate(new_date_A = as.Date(date, format = "%Y-%m-%d")) %>%
      mutate(date = (ifelse(is.na(new_date_B), as.character(new_date_A), as.character(new_date_B)))) %>%
      dplyr::select(-c(new_date_B, new_date_A)) %>% 
      as_tibble
    
    df3 <- ldply(unlist(files3), get_files) %>%
      mutate(LOTNO = lotno) %>%
      mutate(new_date_B = as.Date(date, format = "%m/%d/%Y")) %>%
      mutate(new_date_A = as.Date(date, format = "%Y-%m-%d")) %>%
      mutate(date = (ifelse(is.na(new_date_B), as.character(new_date_A), as.character(new_date_B)))) %>%
      dplyr::select(-c(new_date_B, new_date_A)) %>% 
      as_tibble
    
    df4 <- ldply(unlist(files4), get_files) %>%
      mutate(LOTNO = lotno) %>%
      mutate(new_date_B = as.Date(date, format = "%m/%d/%Y")) %>%
      mutate(new_date_A = as.Date(date, format = "%Y-%m-%d")) %>%
      mutate(date = (ifelse(is.na(new_date_B), as.character(new_date_A), as.character(new_date_B)))) %>%
      dplyr::select(-c(new_date_B, new_date_A)) %>% 
      as_tibble
    
    df <- left_join(df1,df2,by=c("LOTNO", "date", "time"))
    df <- left_join(df,df3,by=c("LOTNO", "date", "time"))
    df <- left_join(df,df4,by=c("LOTNO", "date", "time"))
    
    df <- df %>%
      mutate(new_time = seq(1:nrow(df)),
             avg_top_kiln_temp = rowMeans(select(., t_c_1t,t_c_2t,t_c_3t,t_c_4t)),
             avg_bot_kiln_temp = rowMeans(select(., t_c_1b,t_c_2b,t_c_3b,t_c_4b)),
             avg_kiln_temp     = rowMeans(select(., t_c_1t,t_c_2t,t_c_3t,t_c_4t,t_c_1b,t_c_2b,t_c_3b,t_c_4b)),
             start_temp = avg_kiln_temp[1],
             diff_setpoint = setpoint.x - lag(setpoint.x),
             kiln = kiln_name,

             max_diff_setpoint = max(diff_setpoint, na.rm=TRUE),
             max_temp = max(avg_kiln_temp),

             temp_slope = avg_kiln_temp - lag(avg_kiln_temp),
             diff_start = start_temp - avg_kiln_temp,
             
      ) %>% 
      # fix for lotno 020119H bottom TC's failing 
      mutate(avg_kiln_temp = ifelse(is.na(avg_bot_kiln_temp), avg_top_kiln_temp, avg_kiln_temp),
             max_temp = max(avg_kiln_temp),
             temp_slope = avg_kiln_temp - lag(avg_kiln_temp),
             diff_start = start_temp - avg_kiln_temp
      )
    # find splice beginning
    if     ( lotno == "011619H"){splice_beginning = 474 }
    else if( lotno == "020119H"){splice_beginning = 1353 }
    else if( lotno == "020519H"){splice_beginning = 1 }
    else if( lotno == "020819H"){splice_beginning = 1362 }
    else if( lotno == "021219H"){splice_beginning = 1248 }
    else if( lotno == "022019H"){splice_beginning = 1893 }
    else if( lotno == "022419H"){splice_beginning = 1085 }
    else if( lotno == "022819H"){splice_beginning = 1053}
    else if( lotno == "030819H"){splice_beginning = 1007}
    else if( lotno == "031319H"){splice_beginning = 1 }
    else if( lotno == "032519H"){splice_beginning = 1078 }
    else if( lotno == "041919H"){splice_beginning = 1 }
    else if( lotno == "042419H"){splice_beginning = 1 }
    else if( lotno == "042819H"){splice_beginning = 1 }
    else if( lotno == "050219H"){splice_beginning = 1 }
    else if( lotno == "052419H"){splice_beginning = 1182 }
    else if( lotno == "053119H"){splice_beginning = 1 }
    else if( lotno == "060719H"){splice_beginning = 1 }
    else if( lotno == "070419H"){splice_beginning = 1 }
    else if( lotno == "081019H"){splice_beginning = 905 }
    else if( lotno == "081419H"){splice_beginning = 949 }
    else if( lotno == "081819H"){splice_beginning = 962 }
    else if( lotno == "082219H"){splice_beginning = 1021 }
    else if( lotno == "091019H"){splice_beginning = 1024 }
    else if( lotno == "091419H"){splice_beginning = 1035 }
    else if( lotno == "091819H"){splice_beginning = 1064 }
    else if( lotno == "092219H"){splice_beginning = 1070 }
    else if( lotno == "092619H"){splice_beginning = 1083 }
    else if( lotno == "093019H"){splice_beginning = 1128 }
    else if( lotno == "102419H"){splice_beginning = 1162 }
    else if( lotno == "102819H"){splice_beginning = 1135 }
    else if( lotno == "101219H"){splice_beginning = 976 }
    else if( lotno == "120319H"){splice_beginning = 528 }
    else{ splice_beginning <- which(df$diff_setpoint == df$max_diff_setpoint)[1] }
    
    if(is.na(splice_beginning)){splice_beginning = 1}
    
    df <- df[splice_beginning:nrow(df),]

    df <- df %>%
      mutate(new_time = seq(1:nrow(df)))
    
    # bind lot data to collective data
    kilns_H <- bind_rows(kilns_H, df)
  }
}

# names(kilns_H)
kilns_H <- kilns_H %>%
  dplyr::select(c(LOTNO,kiln,date,time,new_time,everything())) %>%
  dplyr::select(-c(start_temp,diff_setpoint,max_diff_setpoint,temp_slope,diff_start)) %>%
  mutate(LOTNO = as.factor(LOTNO))
# # names(kilns_H)
# 
# levels(kilns_H$LOTNO)

# t <- kilns_H %>%
#   dplyr::filter(LOTNO == "112119H") %>%
#   # dplyr::filter(LOTNO %in% levels(kilns_H$LOTNO)) %>%
#   # dplyr::filter(LOTNO %in% levels(kilns_H$LOTNO)[1:50]) %>%
#   dplyr::filter(LOTNO %in% levels(kilns_H$LOTNO)[51:100]) %>%
#   # dplyr::filter(LOTNO %in% levels(kilns_H$LOTNO)[101:150]) %>%
#   group_by(LOTNO) %>%
#   # ggplot(aes(x=new_time, y = setpoint, color=LOTNO))+
#   ggplot(aes(x=new_time, y = avg_kiln_temp))+
#   geom_point(size = 1)+
#   geom_line(aes(y=setpoint.x),size=1,color="green")+
#   # geom_line(aes(y=temp_slope),color='red')+
#   # geom_line(aes(y=diff_start),color='blue')+
#   # geom_line(aes(y=diff_setpoint),color='blue')+
#   theme(legend.position = "none")+
#   facet_wrap(~LOTNO,scales="free")
# 
# ggplotly(t)
