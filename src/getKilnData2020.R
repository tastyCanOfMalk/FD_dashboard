# get functions -----------------------------------------------------------
source("src/userFunctions.R")
source("src/kilnFunctions.R")

# library(plotly)

kilns_dir <- "data/kiln/Kiln Run Data_2020"
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
    if      (lotno == "061820A") { splice_beginning = 1206 }
    else    { 
      splice_beginning = index_max_setpoint_change(df = df, 
                                                   threshold = 90, 
                                                   lookahead = 1,
                                                   lookahead2 = 100,
                                                   lookahead3 = 200)
    }
    
    if (lotno == "061820A") { splice_end = 2866 }
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
kilns_AB_2020 <- select_mutate(kilns_AB)

# length(levels(kilns_AB_2020$LOTNO))
# plot_range(kilns_AB_2020,1,56)

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
kilns_C_2020 <- select_mutate(kilns_C)

# length(levels(kilns_C_2020$LOTNO))
# plot_range(kilns_C_2020,1,56)


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
    if     ( lotno == "011818D" ){ splice_beginning = 635 }
    else if( lotno == "012518D" ){ splice_beginning = 646 }
    else if( lotno == "012518D" ){ splice_beginning = 981 }
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
kilns_D_2020 <- select_mutate(kilns_D)

# length(levels(kilns_D_2020$LOTNO))
# plot_range(kilns_D_2020,1,56)

# KILNS E -----------------------------------------------------------------
kilns_E <- tibble()

for(kiln in kilns[c(5)]){
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
    if     ( lotno == "032219E" ){ splice_beginning = 561 }
    else if( lotno == "072619E" ){ splice_beginning = 999 }
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
    kilns_E <- bind_rows(kilns_E, df)
  }
}

# select and mutate
kilns_E_2020 <- select_mutate(kilns_E)

# length(levels(kilns_E_2020$LOTNO))
# plot_range(kilns_E_2020,1,25)

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
    splice_beginning = index_max_setpoint_change(df = df, 
                                                 threshold = 5, 
                                                 lookahead = 1)
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
    
    # splice end
    df <- df[1:index_splice_end(df),]
    
    # bind lot data to collective data
    kilns_F <- bind_rows(kilns_F, df)
  }
}

# select and mutate
kilns_F_2020 <- select_mutate(kilns_F)

# length(levels(kilns_F_2020$LOTNO))
# plot_range(kilns_F_2020,1,56)


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
    df <- df %>% 
      dplyr::mutate(
        avg_kiln_temp     = rowMeans(select(., tc_t1,tc_t2,tc_t3,tc_t4,tc_b1,tc_b2,tc_b3,tc_b4)),
        setpoint          = rowMeans(select(., tmprsp, bzone_sp))      
      )
    
    # create new time column
    df <- update_time(df)
    
    # get beginning and end splices
    if     ( lotno == "012318G" ){ splice_beginning = 1434 }
    else if( lotno == "032420G" ){ splice_beginning = 1003 }
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
    
    # splice end
    df <- df[1:index_splice_end(df),]
    
    df <- df %>% 
      
    
    # bind lot data to collective data
    kilns_G <- bind_rows(kilns_G, df)
  }
}

# select and mutate
kilns_G_2020 <- select_mutate(kilns_G)

# length(levels(kilns_G_2020$LOTNO))
# plot_range(kilns_G_2020,1,56)


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
    if     ( lotno == "" ){ splice_beginning = 254 }
    else if( lotno == "041520H" ){ splice_beginning = 702 }
    else{
      splice_beginning = index_max_setpoint_change(df = df, 
                                                   threshold = 100, 
                                                   lookahead = 20,
                                                   lookahead2 = 100)
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
kilns_H_2020 <- select_mutate(kilns_H)

length(levels(kilns_H_2020$LOTNO))
plot_range(kilns_H_2020,1,56)
