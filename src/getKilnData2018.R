# get functions -----------------------------------------------------------
source("src/userFunctions.R")
source("src/kilnFunctions.R")

# library(plotly)

kilns_dir <- "data/kiln/Kiln Run Data_2018"
kilns <- list.dirs(kilns_dir, recursive = FALSE)

# KILNS A, B ---------------------------------------------------------------------
kilns_AB <- tibble()

for(kiln in kilns[1:2]){
  # loop thru each primary kiln folder
  kiln_name <- str_split(str_split(kiln, "/")[[1]][length(str_split(kiln, "/")[[1]])], "-")[[1]][1]
  
  for(sub_dir in list.dirs(kiln)[2:length(list.dirs(kiln))]){
    # loop thru each lot in kiln folder
    lotno <- str_split(str_split(sub_dir, "/")[[1]][length(str_split(sub_dir, "/")[[1]])], " ")[[1]][2]
    lotno <- paste0(lotno, kiln_name)
    
    # list all CSV files in directory
    files <- list(paste0(sub_dir, "/", list.files(sub_dir, pattern = "CSV")))
    
    # paste lotno, convert dates
    df <- get_data(files = files)
    
    # rename columns
    df <- df %>% 
      dplyr::rename("avg_kiln_temp" = "kilntemp",
                    "setpoint"      = "temp_setpoint")
    
    # create new time column
    df <- update_time(df)
    
    # get beginning and end splices
    if      (lotno == "030118A") { splice_beginning = 2660 }
    else if (lotno == "030918A") { splice_beginning = 477 }
    else if (lotno == "050418A") { splice_beginning = 1311 }
    else if (lotno == "051018A") { splice_beginning = 1415 }
    else if (lotno == "051418A") { splice_beginning = 1296 }
    else if (lotno == "113018A") { splice_beginning = 109 }
    else    { 
      splice_beginning = index_max_setpoint_change(df = df, 
                                                   threshold = 90, 
                                                   lookahead = 1,
                                                   lookahead2 = 100,
                                                   lookahead3 = 200)
    }
    
    if      (lotno == "051018A") { splice_end = 3113 }
    else if (lotno == "051418A") { splice_end = 2969 }
    else{
      splice_end = index_splice_end(df = df, 
                                    temp_threshold = 100,
                                    setpoint_threshold =  1000,
                                    setpoint_change_lookahead = 4,
                                    setpoint_change_threshold = 1)
    }
    
    # splice
    df <- df[ splice_beginning : splice_end, ]
    
    # splice testing
    # df <- add_plot_splices(df)
    
    # create new time column, add kiln name
    df <- update_time(df) %>% 
      dplyr::mutate(kiln = kiln_name)
    
    # bind individual lot df to parent df
    kilns_AB <- bind_rows(kilns_AB, df)
  }
}

# select and mutate
kilns_AB <- kilns_AB[!is.na(kilns_AB$date),]

kilns_AB_2018 <- select_mutate(kilns_AB) %>% 
  dplyr::select(-c(t_c_51a, t_c_51b, t_c_52a, t_c_52b, temp_sp, bzon_out, tzon_out))

# length(levels(kilns_AB_2018$LOTNO))
# plot_range   (kilns_AB_2018,1,25)
# plot_range   (kilns_AB_2018,25,50)
# plot_range   (kilns_AB_2018,51,75)
# plot_range   (kilns_AB_2018,76,100)
# plot_range   (kilns_AB_2018,101,125)
# plot_range(kilns_AB, filter="113018A",plotly_on=TRUE)

# KILNS C ------------------------------------------------------------------
kilns_C <- tibble()

for(kiln in kilns[c(3)]){
  # loop thru each primary kiln folder
  kiln_name <- str_split(str_split(kiln, "/")[[1]][length(str_split(kiln, "/")[[1]])], "-")[[1]][1]
  
  for(sub_dir in list.dirs(kiln)[2:length(list.dirs(kiln))]){
    # loop thru each lot in kiln folder
    lotno <- str_split(sub_dir, "/")[[1]][length(str_split(sub_dir, "/")[[1]])]
    lotno <- paste0(lotno, kiln_name)
    
    # list all CSV files in directory
    files1 <- list(paste0(sub_dir, "/", list.files(sub_dir, pattern = "1.CSV")))
    files2 <- list(paste0(sub_dir, "/", list.files(sub_dir, pattern = "2.CSV")))
    
    # paste lotno, convert dates
    df1 <- get_data(files = files1)
    df2 <- get_data(files = files2)
    
    # join columns
    df <- left_join(df1, df2, by = c("LOTNO", "date", "time"))
    
    # rename columns
    df <- df %>% 
      dplyr::mutate(
        avg_kiln_temp = rowMeans(select(., t_c_51a,t_c_51b,t_c_52a,t_c_52b))
      ) %>% 
      dplyr::rename("setpoint" = "temp_sp")
    
    # create new time column
    df <- update_time(df)
    
    # get beginning and end splices
    splice_beginning = index_max_setpoint_change(df = df, 
                                                 threshold = 5, 
                                                 lookahead = 1)
    splice_end = index_splice_end(df = df, 
                                  temp_threshold = 100,
                                  setpoint_threshold = 1000,
                                  setpoint_change_lookahead = 3,
                                  setpoint_change_threshold = 1)
    
    # splice
    df <- df[ splice_beginning : splice_end, ]
    
    # add splices
    # df <- add_plot_splices(df)
    
    # create new time column, add kiln name
    df <- update_time(df) %>% 
      dplyr::mutate(kiln = kiln_name)
    
    # join lot df to primary df
    kilns_C <- bind_rows(kilns_C, df)
  }
}

# select and mutate
kilns_C <- kilns_C[!is.na(kilns_C$date),]
kilns_C_2018 <- select_mutate(kilns_C)

# length(levels(kilns_C_2018$LOTNO))
# plot_range   (kilns_C_2018,1,25)
# plot_range   (kilns_C_2018,25,50)

# KILNS D -----------------------------------------------------------------

kilns_D <- tibble()

for(kiln in kilns[c(4)]){
  # loop thru each primrary kiln folder
  kiln_name <- str_split(str_split(kiln, "/")[[1]][length(str_split(kiln, "/")[[1]])], "-")[[1]][1]
  
  for(sub_dir in list.dirs(kiln)[2:length(list.dirs(kiln))]){
    # loop thru each lot in kiln folder
    lotno <- str_split(str_split(sub_dir, "/")[[1]][length(str_split(sub_dir, "/")[[1]])], " ")[[1]][2]
    lotno <- paste0(lotno, kiln_name)
    
    files1 <- list(paste0(sub_dir, "/", list.files(sub_dir, pattern = "1.CSV")))
    files2 <- list(paste0(sub_dir, "/", list.files(sub_dir, pattern = "2.CSV")))
    
    # paste lotno, convert dates
    df1 <- get_data(files = files1)
    df2 <- get_data(files = files2)
    
    # join dfs
    df <- left_join(df1,df2,by=c("LOTNO", "date", "time"))
    
    # rename columns
    df <- df %>% 
      dplyr::mutate(
        avg_kiln_temp = rowMeans(select(., bot_t_c_1 ,bot_t_c_2 ,top_t_c_3 ,top_t_c_4))
      )
    
    # create new time column
    df <- update_time(df)
    
    # get beginning and end splices
    if     ( lotno == "011818D" ){ splice_beginning = 1311 }
    else if( lotno == "012518D" ){ splice_beginning = 1465 }
    else if( lotno == "051818D" ){ splice_beginning = 1056 }
    else if( lotno == "070918D" ){ splice_beginning = 1008 }
    else{
      splice_beginning = index_max_setpoint_change(df = df, 
                                                   threshold = 30, 
                                                   lookahead = 1,
                                                   lookahead2 = 50)
    }
    
    splice_end = index_splice_end(df = df, 
                                  temp_threshold = 100,
                                  setpoint_threshold = 1000,
                                  setpoint_change_lookahead = 3,
                                  setpoint_change_threshold = 1)
    
    # splice
    df <- df[ splice_beginning : splice_end, ]
    
    # add splices
    # df <- add_plot_splices(df)
    
    # create new time column, add kiln name
    df <- update_time(df) %>% 
      dplyr::mutate(kiln = kiln_name)
    
    # binds lots to kiln df
    kilns_D <- bind_rows(kilns_D, df)
  }
}

# select and mutate
kilns_D <- kilns_D[!is.na(kilns_D$date),]
kilns_D <- kilns_D[!kilns_D$LOTNO == "010419D",]
kilns_D_2018 <- select_mutate(kilns_D)

# levels(kilns_D_2018$LOTNO)
# length(levels(kilns_D_2018$LOTNO))
# plot_range   (kilns_D_2018,1,25)
# plot_range   (kilns_D_2018,25,50)
# plot_range   (kilns_D_2018,filter="070918D",plotly_on = TRUE)

# KILNS F -----------------------------------------------------------------

kilns_F <- tibble()

for(kiln in kilns[c(6)]){
  # loop thru each primrary kiln folder
  kiln_name <- str_split(str_split(kiln, "/")[[1]][length(str_split(kiln, "/")[[1]])], "-")[[1]][1]
  
  for(sub_dir in list.dirs(kiln)[2:length(list.dirs(kiln))]){
    # loop thru each lot in kiln folder
    lotno <- str_split(sub_dir, "/")[[1]][length(str_split(sub_dir, "/")[[1]])]
    lotno <- paste0(lotno, kiln_name)
    
    # list of files
    files1 <- list(paste0(sub_dir, "/", list.files(sub_dir, pattern = "1.CSV")))
    files2 <- list(paste0(sub_dir, "/", list.files(sub_dir, pattern = "2.CSV")))
    
    # paste lotno, convert dates
    df1 <- get_data(file = files1)
    df2 <- get_data(file = files2)
    
    # join dataframes
    df <- left_join(df1, df2, by = c("LOTNO", "date", "time"))
    
    # paste lotno, convert dates
    df1 <- get_data(files = files1)
    df2 <- get_data(files = files2)
    
    # join dfs
    df <- left_join(df1,df2,by=c("LOTNO", "date", "time"))
    
    # rename columns
    df <- df %>% 
      dplyr::mutate(
        avg_kiln_temp = rowMeans(select(., bot_t_c_1 ,bot_t_c_2 ,top_t_c_3 ,top_t_c_4))
      )
    
    # create new time column
    df <- update_time(df)
    
    # get beginning and end splices
    
    if(lotno == "022818F"){splice_beginning = 3957}
    else{
      splice_beginning = index_max_setpoint_change(df = df, 
                                                   threshold = 5, 
                                                   lookahead = 1)
    }
    splice_end = index_splice_end(df = df, 
                                  temp_threshold = 300,
                                  setpoint_threshold = 1000,
                                  setpoint_change_lookahead = 5,
                                  setpoint_change_threshold = 1)
    
    # splice
    df <- df[ splice_beginning : splice_end, ]
    
    # add splices
    # df <- add_plot_splices(df)
    
    # create new time column, add kiln name
    df <- update_time(df) %>% 
      dplyr::mutate(kiln = kiln_name)
    
    # bind lot data to collective data
    df <- df[1:index_splice_end(df),]
    
    # bind lot data to collective data
    kilns_F <- bind_rows(kilns_F, df)
  }
}

# select and mutate
kilns_F_2018 <- select_mutate(kilns_F)

# length(levels(kilns_F_2018$LOTNO))
# plot_range   (kilns_F_2018,1,25)
# plot_range   (kilns_F_2018,filter="022818F", plotly_on=T)

# KILNS G -----------------------------------------------------------------

kilns_G <- tibble()

for(kiln in kilns[c(7)]){
  # loop thru each primary kiln folder
  kiln_name <- str_split(str_split(kiln, "/")[[1]][length(str_split(kiln, "/")[[1]])], "-")[[1]][1]
  
  for(sub_dir in list.dirs(kiln)[2:length(list.dirs(kiln))]){
    # loop thru each lot in kiln folder
    lotno <- str_split(sub_dir, "/")[[1]][length(str_split(sub_dir, "/")[[1]])]
    lotno <- paste0(lotno, kiln_name)
    
    # list of files
    files1 <- list(paste0(sub_dir, "/", list.files(sub_dir, pattern = "1.CSV")))
    files2 <- list(paste0(sub_dir, "/", list.files(sub_dir, pattern = "2.CSV")))
    files3 <- list(paste0(sub_dir, "/", list.files(sub_dir, pattern = "3.CSV")))
    files4 <- list(paste0(sub_dir, "/", list.files(sub_dir, pattern = "4.CSV")))
    
    # paste lotno, convert dates
    df1 <- get_data(file = files1)
    df2 <- get_data(file = files2)
    df3 <- get_data(file = files3)
    df4 <- get_data(file = files4)
    
    # join dfs
    df <- left_join(df1, df2, by=c("LOTNO", "date", "time"))
    df <- left_join(df,  df3, by=c("LOTNO", "date", "time"))
    df <- left_join(df,  df4, by=c("LOTNO", "date", "time"))
    
    # rename columns
    # bottom TCs gave out
    if( lotno == "050118G" | lotno == "090918G" | lotno == "103018G") { 
      df <- df %>% 
        dplyr::mutate(
          avg_kiln_temp     = rowMeans(select(., tc_t1,tc_t2,tc_t3,tc_t4)),
          setpoint          = tmprsp)  
      }
    else{
      df <- df %>% 
        dplyr::mutate(
          avg_kiln_temp     = rowMeans(select(., tc_t1,tc_t2,tc_t3,tc_t4,tc_b1,tc_b2,tc_b3,tc_b4)),
          setpoint          = rowMeans(select(., tmprsp, bzone_sp))      
        )
    }
    
    # create new time column
    df <- update_time(df)
    
    # get beginning and end splices
    if     ( lotno == "012318G" ){ splice_beginning = 1434 }
    else if( lotno == "010418G" ){ splice_beginning = 702 }
    else{
      splice_beginning = index_max_setpoint_change(df = df, 
                                                   threshold = 170, 
                                                   lookahead = 1,
                                                   lookahead2 = 50)
    }
    splice_end = index_splice_end(df = df, 
                                  temp_threshold = 100,
                                  setpoint_threshold = 1000,
                                  setpoint_change_lookahead = 5,
                                  setpoint_change_threshold = 10)
    
    # splice
    df <- df[ splice_beginning : splice_end, ]
    
    # add splices
    # df <- add_plot_splices(df)
    
    # create new time column, add kiln name
    df <- update_time(df) %>% 
      dplyr::mutate(kiln = kiln_name)
    
    # bind lot data to collective data
    df <- df[1:index_splice_end(df),]
    
    # bind lot data to collective data
    kilns_G <- bind_rows(kilns_G, df)
  }
}

# select and mutate
kilns_G_2018 <- select_mutate(kilns_G)

# length(levels(kilns_G_2018$LOTNO))
# plot_range   (kilns_G_2018,1,25)

# KILNS H -----------------------------------------------------------------

kilns_H <- tibble()

for(kiln in kilns[c(8)]){
  # loop thru each primrary kiln folder
  kiln_name <- str_split(str_split(kiln, "/")[[1]][length(str_split(kiln, "/")[[1]])], "-")[[1]][1]
  
  for(sub_dir in list.dirs(kiln)[2:length(list.dirs(kiln))]){
    # loop thru each lot in kiln folder
    lotno <- str_split(str_split(sub_dir, "/")[[1]][length(str_split(sub_dir, "/")[[1]])], " ")[[1]][2]
    lotno <- paste0(lotno, kiln_name)
    
    # list files
    files1 <- list(paste0(sub_dir, "/", list.files(sub_dir, pattern = "1.CSV")))
    files2 <- list(paste0(sub_dir, "/", list.files(sub_dir, pattern = "2.CSV")))
    files3 <- list(paste0(sub_dir, "/", list.files(sub_dir, pattern = "3.CSV")))
    
    if( lotno == "122018H" ){ 
      files4 <- list(paste0(sub_dir, "/", list.files(sub_dir, pattern = "4.CSV")))
    } else {
      files4 <- list(paste0(sub_dir, "/", list.files(sub_dir, pattern = "6.CSV")))
    }
    
    # paste lotno, convert dates
    df1 <- get_data(file = files1)
    df2 <- get_data(file = files2)
    df3 <- get_data(file = files3)
    df4 <- get_data(file = files4)
    
    # join dfs
    df <- left_join(df1, df2, by=c("LOTNO", "date", "time"))
    df <- left_join(df,  df3, by=c("LOTNO", "date", "time"))
    df <- left_join(df,  df4, by=c("LOTNO", "date", "time"))
    
    # rename columns
    if( lotno == "040718H" ){
      df <- df %>% 
        dplyr::mutate(
          avg_kiln_temp     = rowMeans(select(., t_c_1t.x,t_c_2t.x,t_c_3t.x,t_c_4t.x,t_c_1b,t_c_2b,t_c_3b,t_c_4b)),
          setpoint          = rowMeans(select(., setpoint.x,setpoint.y)))      
          }
    else{
      df <- df %>% 
        dplyr::mutate(
          avg_kiln_temp     = rowMeans(select(., t_c_1t,t_c_2t,t_c_3t,t_c_4t,t_c_1b,t_c_2b,t_c_3b,t_c_4b)),
          setpoint          = rowMeans(select(., setpoint.x,setpoint.y)))
    }
    
    # create new time column
    df <- update_time(df)
    
    # get beginning and end splices
    if     ( lotno == "030118H" ){ splice_beginning = 2554 }
    else if( lotno == "010518H" ){ splice_beginning = 431 }
    else if( lotno == "030718H" ){ splice_beginning = 162 }
    else if( lotno == "072018H" ){ splice_beginning = 500 }
    else if( lotno == "072818H" ){ splice_beginning = 562 }
    else if( lotno == "090618H" ){ splice_beginning = 1200 }
    else if( lotno == "101118H" ){ splice_beginning = 608 }
    else if( lotno == "102518H" ){ splice_beginning = 593 }
    else if( lotno == "111518H" ){ splice_beginning = 1200 }
    else if( lotno == "112418H" ){ splice_beginning = 750 }
    else{
      splice_beginning = index_max_setpoint_change(df = df, 
                                                   threshold = 100, 
                                                   lookahead = 1,
                                                   lookahead2 = 225)
    }
    
    splice_end = index_splice_end(df = df, 
                                  temp_threshold = 300,
                                  setpoint_threshold = 1000,
                                  setpoint_change_lookahead = 5,
                                  setpoint_change_threshold = 1)
    
    # splice
    df <- df[ splice_beginning : splice_end, ]
    
    # add splices
    # df <- add_plot_splices(df)
    
    # create new time column, add kiln name
    df <- update_time(df) %>% 
      dplyr::mutate(kiln = kiln_name)
    
    # bind lot data to collective data
    df <- df[1:index_splice_end(df),]
    
    # bind lot data to collective data
    kilns_H <- bind_rows(kilns_H, df)
  }
}

# select and mutate
kilns_H_2018 <- select_mutate(kilns_H)

# length(levels(kilns_H_2018$LOTNO))
# plot_range   (kilns_H_2018,1,25)
# plot_range   (kilns_H_2018,26,50)
# plot_range   (kilns_H_2018,51,75)
# plot_range   (kilns_H_2018, filter = "101118H", plotly_on = TRUE)
# plot_range   (kilns_H_2018, filter = "101118H", plotly_on = TRUE)
# plot_range   (kilns_H_2018, filter = "102518H", plotly_on = TRUE)