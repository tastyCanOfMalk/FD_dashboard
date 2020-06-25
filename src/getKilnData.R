source("src/cleanCombine.R")
source("src/userFunctions.R")

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
      dplyr::select(-c(diff_setpoint, max_diff_setpoint, start_temp, find_end_temp)) %>%
      mutate(kiln = kiln_name) %>% 
      as_tibble()
    
    # bind individual lot df to parent df
    kilns_AB <- bind_rows(kilns_AB, df)
    
  }
    
}
 
kilns_AB <- kilns_AB %>% 
  # dplyr::select(-c(diff_setpoint, max_diff_setpoint)) %>% 
  mutate(LOTNO = as.factor(LOTNO))

# testing
levels(kilns_AB$LOTNO)

# view many plots
t <- kilns_AB %>% 
  # dplyr::filter(LOTNO %in% levels(kilns_AB$LOTNO)) %>%
  dplyr::filter(LOTNO %in% levels(kilns_AB$LOTNO)[101:120]) %>%
  # dplyr::filter(LOTNO %in% levels(kilns_AB$LOTNO)[1:25]) %>%
  # dplyr::filter(LOTNO %in% levels(kilns_AB$LOTNO)[26:50]) %>%
  # dplyr::filter(LOTNO %in% levels(kilns_AB$LOTNO)[51:55]) %>%
  group_by(LOTNO) %>% 
  ggplot(aes(x=new_time, y = kilntemp, color = LOTNO))+
  # ggplot(aes(x=new_time, y = temp_setpoint, color = LOTNO))+
  # ggplot(aes(x=new_time, y = kilnpress, color = LOTNO))+
  geom_point(size = .8)+
  # geom_vline(aes(xintercept = splice_end))+
  theme(legend.position = "none")+
  facet_wrap(~LOTNO, scales = "free")

ggplotly(t)


# kiln C ------------------------------------------------------------------
mylist <- list()
kilns_C <- tibble()

for(kiln in kilns[3]){
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
             diff_start = start_temp - avg_kiln_temp)
    
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
kilns_C <- kilns_C %>% 
  # dplyr::select(-c(diff_setpoint, max_diff_setpoint)) %>% 
  mutate(LOTNO = as.factor(LOTNO))

levels(kilns_C$LOTNO)

# view many plots
kilns_C %>% 
  # dplyr::filter(LOTNO %in% levels(kilns_C$LOTNO)) %>%
  # dplyr::filter(LOTNO %in% levels(kilns_C$LOTNO)[1]) %>%
  # dplyr::filter(LOTNO %in% levels(kilns_C$LOTNO)[101:120]) %>%
  # dplyr::filter(LOTNO %in% levels(kilns_C$LOTNO)[1:25]) %>%
  # dplyr::filter(LOTNO %in% levels(kilns_C$LOTNO)[26:50]) %>%
  # dplyr::filter(LOTNO %in% levels(kilns_C$LOTNO)[51:55]) %>%
  group_by(LOTNO) %>% 
  # ggplot(aes(x=new_time, y = temp_sp, color = LOTNO))+
  ggplot(aes(x=new_time, y = avg_kiln_temp, color = LOTNO))+
  # ggplot(aes(x=new_time, y = temp_setpoint, color = LOTNO))+
  # ggplot(aes(x=new_time, y = kilnpress, color = LOTNO))+
  geom_point(size = .8)+
  # geom_point(aes(y = diff_setpoint), color='blue', size=.5)+
  # geom_vline(aes(xintercept = 856))+
  theme(legend.position = "none")+
  facet_wrap(~LOTNO, scales = "free")

# play df
t <- kilns_C %>% 
  dplyr::filter(LOTNO == levels(kilns_C$LOTNO)[10])

# t <- df
# index of max temp
max_temp_index <- which(t$avg_kiln_temp == t$max_temp)[[1]]

# t <- t %>% 
#   mutate(diff_start = start_temp - avg_kiln_temp)
# 
tt <- t %>% 
  ggplot(aes(x=new_time, y=avg_kiln_temp))+
  geom_line(color="black")+
  geom_line(aes(y=diff_start), color="red")+
  geom_line(aes(y=temp_slope), color="blue")
ggplotly(tt)

temp_list <- list()
for(i in seq(max_temp_index, nrow(t))){
   # print(t$avg_kiln_temp[i])
  slope      <- t$temp_slope[i]
  diff_start <- t$diff_start[i]
  
  if( (slope > -10) & (diff_start > -40) ){
    temp_list <- append(temp_list, i)
  }
}

t %>% 
  mutate(find_end_temp2 = avg_kiln_temp - lag(avg_kiln_temp, n=2)) %>% 
  ggplot(aes(x=new_time)) +
  geom_point(aes(y=avg_kiln_temp))+
  geom_line(aes(y=diff_start), color='blue')+
  geom_line(aes(y=find_end_temp2*10),color='red')+
  # geom_line(aes(y=find_end_temp*10),color='red')+
  geom_hline(aes(yintercept = start_temp)) +
  geom_hline(aes(yintercept = 0))+
  geom_vline(aes(xintercept = temp_list[[1]]))
ggplotly(tt)





