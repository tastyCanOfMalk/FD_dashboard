
# -------------------------------------------------------------------------

 
getwd()


all_kilns_data   = "data/kiln/Kiln Run Data_2019"

kiln_data_dirs <- list.dirs(all_kilns_data, recursive = FALSE)


# A kiln dir
kiln_data_dirs[1]

# A kiln sub-dirs
list.dirs(kiln_data_dirs[1])[2:length(list.dirs(kiln_data_dirs[1]))]

# A kiln sub dir CSVs
list.files(list.dirs(kiln_data_dirs[1])[2:length(list.dirs(kiln_data_dirs[1]))][1], pattern = "CSV")

# csv to df

dir <- list.dirs(kiln_data_dirs[1])[2:length(list.dirs(kiln_data_dirs[1]))][1]
files <- list.files(list.dirs(kiln_data_dirs[1])[2:length(list.dirs(kiln_data_dirs[1]))][1], pattern = "CSV")

mylist <- list()

for(file in files){
  mylist <- append(mylist, paste0(dir,"/",file))
}

df <- ldply(mylist, get_files) %>% 
  as_tibble()

# try to get lotno
# list.dirs(kiln_data_dirs[1])[2:length(list.dirs(kiln_data_dirs[1]))][1]
a <- list.files(kiln_data_dirs[1])[1]
a <- str_split(a, " ")[[1]][2]

b <- str_split(kiln_data_dirs[1], "/")[[1]][length(str_split(kiln_data_dirs[1], "/")[[1]])]
b <- str_split(b, "-")[[1]][1]

lotno <- paste0(a,b)

df <- df %>% 
  mutate(LOTNO = lotno)
  




# loop --------------------------------------------------------------------
kilns_dir <- "data/kiln/Kiln Run Data_2019"

kilns <- list.dirs(kilns_dir, recursive = FALSE)

# kilns[1:2]

mylist <- list()

for(x in seq(length(kilns[1:2]))){
  
  dir <- kilns[x]
  
  # extract kiln name from directory 
  kiln_name <- str_split(str_split(dir, "/")[[1]][length(str_split(dir, "/")[[1]])], "-")[[1]][1]
  # print(kiln_name)
  
  sub_dir <- list.dirs(dir)[2:length(list.dirs(dir))]
    
  for(y in seq(length(sub_dir))){
    lotno <- str_split(str_split(sub_dir[y], "/")[[1]][length(str_split(sub_dir[y], "/")[[1]])], " ")[[1]][2]
    lotno <- paste0(lotno, kiln_name)
    # print(lotno)
    
    sub_dir_files <- list.files(sub_dir[y], pattern = "CSV")
    for(z in seq(length(sub_dir_files))){
      print(sub_dir[y], sub_dir_files[z])
      # print(sub_dir)
      # files <- paste0(sub_dir, "/", sub_dir_files)
      
      # mylist <- append(mylist, files)
    }
    # print(list.files(sub_dir[y], pattern = "CSV"))
  }
  
  # print(dir)
  # print(sub_dir)
  # print(sub_dir)
  # csv_sub_dir <- list.files(sub_dir, pattern = "CSV")
  # print(paste0(sub_dir, "/", csv_sub_dir))
}


# general loop working ---------------------------------------------------------------------
kilns_dir <- "data/kiln/Kiln Run Data_2019"
kilns <- list.dirs(kilns_dir, recursive = FALSE)

mylist <- list()
test <- tibble()

for(kiln in kilns[1]){
  # loop thru each primrary kiln folder
  kiln_name <- str_split(str_split(kiln, "/")[[1]][length(str_split(kiln, "/")[[1]])], "-")[[1]][1]
  
  for(sub_dir in list.dirs(kiln)[2:length(list.dirs(kiln))]){
    # loop thru each lot in kiln folder
    lotno <- str_split(str_split(sub_dir, "/")[[1]][length(str_split(sub_dir, "/")[[1]])], " ")[[1]][2]
    lotno <- paste0(lotno, kiln_name)
    
    # print(paste0(sub_dir, "/", list.files(sub_dir, pattern = "CSV")))
    # print("--------------------")
    files <- list(paste0(sub_dir, "/", list.files(sub_dir, pattern = "CSV")))
    
    df <- ldply(unlist(files), get_files) %>% 
      mutate(LOTNO = lotno) %>% 
      mutate(new_date_B = as.Date(date, format = "%m/%d/%Y")) %>%
      mutate(new_date_A = as.Date(date, format = "%Y-%m-%d")) %>% 
      mutate(date = (ifelse(is.na(new_date_B), as.character(new_date_A), as.character(new_date_B)))) %>% 
      dplyr::select(-c(new_date_B, new_date_A))
    
    df <- df %>% 
      arrange(date, time) %>%
      mutate(
        diff_setpoint = temp_setpoint - lag(temp_setpoint),
        max_diff_setpoint = max(diff_setpoint, na.rm = TRUE)
      )

    if(lotno == "120519A"){splice = 324}
    else{
      splice <- which(df$diff_setpoint == df$max_diff_setpoint)
      }

    df <- df[splice:nrow(df),]
    
    df <- df %>% 
      mutate(new_time = seq(1:nrow(df))) %>% 
      as_tibble()
    
    
    # df <- df %>% 
    #   arrange(date, time) %>%
    #   mutate(new_time = seq(1:nrow(df_all))) %>% 
    #   as_tibble()
    # 
    test <- bind_rows(test, df)
    
  }
    
}
 
test <- test %>% 
  mutate(LOTNO = as.factor(LOTNO))

# testing
levels(test$LOTNO)

# view many plots
test %>% 
  # dplyr::filter(LOTNO %in% levels(test$LOTNO)[1:25]) %>%
  dplyr::filter(LOTNO %in% levels(test$LOTNO)[26:50]) %>%
  # dplyr::filter(LOTNO %in% levels(test$LOTNO)[51:55]) %>%
  group_by(LOTNO) %>% 
  # ggplot(aes(x=new_time, y = kilntemp, color = LOTNO))+
  ggplot(aes(x=new_time, y = temp_setpoint, color = LOTNO))+
  geom_point()+
  geom_line(aes(y=diff_setpoint))+
  # geom_point()+
  scale_x_continuous(breaks = seq(0,5000,(60*6)))+
  scale_y_continuous(limits = c(0,3000),
                     breaks = seq(0,3000,250))+
  facet_wrap(~LOTNO)
ggplotly(t)

# REMOVE TRASH points before temp increase
# there is always a large change in setpoint diff() before the temp raises (larger than kilntemp)
test %>% 
  dplyr::filter(LOTNO %in% levels(test$LOTNO)[20:25]) %>% 
  group_by(LOTNO) %>% 
  mutate(diff_setpoint   = temp_setpoint - lag(temp_setpoint)) %>% 
  mutate(diff_kilntemp = kilntemp - lag(kilntemp)) %>% 
  ggplot(aes(x=new_time))+
  geom_point(aes(y=temp_setpoint), color="blue", size=.5)+
  geom_point(aes(y=diff_setpoint), color="red", size=.5)+
  geom_point(aes(y=kilntemp), color="green", size=.5)+
  geom_point(aes(y=diff_kilntemp), color="orange", size=.5)+
  geom_vline(aes(xintercept=704))+
  facet_wrap(~LOTNO)
  
# find highest setpoint

t <- test %>% 
  dplyr::select(!c(date, time,new_date_B,new_date_A)) %>% 
  dplyr::filter(LOTNO == levels(test$LOTNO)[20]) %>% 
  mutate(diff_setpoint = temp_setpoint - lag(temp_setpoint),
         max_diff_setpoint = max(diff_setpoint, na.rm=TRUE))

which(t$max_diff_setpoint)
  

which(t$diff_setpoint == t$max_diff_setpoint)  
t[704:nrow(t),]




















max(t$diff_setpoint, na.rm=TRUE)
  ggplot(aes(x=new_time))+
  geom_point(aes(y=temp_setpoint), color="blue", size=.5)+
  geom_point(aes(y=diff_setpoint), color="red", size=.5)+
  facet_wrap(~LOTNO)
  


ggplotly(t)
  dplyr::mutate(diff_setpoint = diff(temp_setpoint),
                diff_kilntemp = diff(kilntemp))

  ggplot(aes(x=new_time, y = kilntemp, color = LOTNO))+
  geom_point()
  








test %>% 
  dplyr::filter(LOTNO %in% levels(test$LOTNO)[7]) %>%
  # mutate(new_date = as.Date(date)) %>% 
  # mutate(new_date = as.Date(date, format = "%m/%d/%Y")) %>% 
  mutate(new_date_head = as.Date(date)) %>%
  mutate(new_date_tail = as.Date(date, format = "%m/%d/%Y")) %>%
  dplyr::select(date, new_date_head, new_date_tail) %>% 
  mutate(new_date = as.Date((ifelse(is.na(new_date_head), as.character(new_date_tail), as.character(new_date_head)))))
  
 
ldply(unlist(files), get_files)

  mutate(lotno = lotno) %>% 
  as_tibble()






# -------------------------------------------------------------------------





mylist




list.dirs(kilns_dir, recursive = FALSE)


# A kiln dir
kiln_data_dirs[1]

# A kiln sub-dirs
list.dirs(kiln_data_dirs[1])[2:length(list.dirs(kiln_data_dirs[1]))]

# A kiln sub dir CSVs
list.files(list.dirs(kiln_data_dirs[1])[2:length(list.dirs(kiln_data_dirs[1]))][1], pattern = "CSV")

# csv to df

dir <- list.dirs(kiln_data_dirs[1])[2:length(list.dirs(kiln_data_dirs[1]))][1]
files <- list.files(list.dirs(kiln_data_dirs[1])[2:length(list.dirs(kiln_data_dirs[1]))][1], pattern = "CSV")

mylist <- list()

for(file in files){
  mylist <- append(mylist, paste0(dir,"/",file))
}

df <- ldply(mylist, get_files) %>% 
  as_tibble()

# try to get lotno
# list.dirs(kiln_data_dirs[1])[2:length(list.dirs(kiln_data_dirs[1]))][1]
a <- list.files(kiln_data_dirs[1])[1]
a <- str_split(a, " ")[[1]][2]

b <- str_split(kiln_data_dirs[1], "/")[[1]][length(str_split(kiln_data_dirs[1], "/")[[1]])]
b <- str_split(b, "-")[[1]][1]

lotno <- paste0(a,b)

df <- df %>% 
  mutate(LOTNO = lotno)
  
