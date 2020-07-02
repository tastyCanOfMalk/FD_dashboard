source('src/getKilnData2019.R')

rm(list=setdiff(ls(), c("df_merged",
                        "df_defects",
                        "df_yields",
                        
                        "kilns_AB",
                        "kilns_C",
                        "kilns_D",
                        "kilns_E",
                        "kilns_F",
                        "kilns_G",
                        "kilns_H"
                        )))

d1 <- matrix(names(kilns_AB))
d2 <- matrix(names(kilns_C))
d3 <- matrix(names(kilns_D))
d4 <- matrix(names(kilns_E))
d5 <- matrix(names(kilns_F))
d6 <- matrix(names(kilns_G))
d7 <- matrix(names(kilns_H))

length(d1) <- 31
length(d2) <- 31
length(d3) <- 31
length(d4) <- 31
length(d5) <- 31
length(d6) <- 31
length(d7) <- 31

cbind(d1,d2,d3,d4,d5,d6,d7)

kilns_AB %>% 
  dplyr::filter(LOTNO %in% levels(kilns_AB$LOTNO)[1:8]) %>% 
  ggplot(aes(x=new_time))+
  geom_line(aes(y=kilntemp))+
  # geom_line(aes(y=temp_setpoint),color='red')+
  geom_line(aes(y=output),color='blue')+
  # geom_line(aes(y=kilnpress),color='green')+
  # geom_line(aes(y=incin_temp),color='grey50')+
  facet_wrap(~LOTNO)

kilns_AB %>% 
  dplyr::select(LOTNO, kiln, date, time, new_time,
                kilntemp, temp_setpoint)

names(kilns_AB) <- 
  c("LOTNO", "kiln", "date", "time", "new_time", 
    "kiln_temp", "temp_setpoint", 
    "output", "kilnpress", "pres_wsp", "diffair", "incin_temp", "max_temp")

names(kilns_C) <- 
  c( "LOTNO", "kiln", "date", "time", "new_time", 
     "TC_bot_1", "TC_bot_2", "TC_top_1", "TC_top_2", 
     "temp_setpoint", 
     "bzon_out", "tzon_out", "pv", "press_wsp", "incin_temp", "output", "output_1", "avg_kiln_temp", "max_temp")

names(kilns_D) <- 
  c( "LOTNO", "kiln", "date", "time", "new_time", 
     "TC_bot_1", "TC_bot_2", "TC_top_1", "TC_top_2", 
     "setpoint", 
     "bottom_output", "top_output", "kiln_press", "press_wsp", "incin_temp", "incin_wsp", "diffair_output", "avg_kiln_temp", "max_temp")

names(kilns_E) <- 
  c( "LOTNO", "kiln", "date", "time", "new_time", 
     "TC_bot_1", "TC_bot_2", "TC_top_1", "TC_top_2", 
     "setpoint", 
     "bottom_output", "top_output", "kiln_press", "press_wsp", "incin_temp", "incin_wsp", "diffair_output", "avg_kiln_temp", "max_temp")

names(kilns_F) <- 
  c( "LOTNO", "kiln", "date", "time", "new_time", 
     "TC_bot_1", "TC_bot_2", "TC_top_1", "TC_top_2", 
     "setpoint", 
     "bottom_output", "top_output", "kiln_press", "press_wsp", "incin_temp", "incin_wsp", "diffair_output", "avg_kiln_temp", "max_temp")

names(kilns_G) <- 
  c( "LOTNO", "kiln", "date", "time", "new_time", 
     "TC_top_1", "TC_top_2", "TC_top_3", "TC_top_4",
     "setpoint_TC_top", 
     "TC_bot_1", "TC_bot_2", "TC_bot_3", "TC_bot_4",
     "setpoint_TC_bot", 
     "txsa_out", "bxa_out", "txairwsp", "bxairwsp", "kiln_pr", "kpress_wsp", "tc_10", "tc_9", "avg_top_kiln_temp", "avg_bot_kiln_temp", "avg_kiln_temp", "max_temp", "x8", "x9")

names(kilns_H) <- 
  c( "LOTNO", "kiln", "date", "time", "new_time", 
     "t_c_1t",  "t_c_2t",  "t_c_3t",  "t_c_4t",  "setpoint.x",  "t_c_1b",  "t_c_2b",  "t_c_3b",  "t_c_4b",  "setpoint.y",  "pressure",  "press_wsp",  "t_c_9exh",  "exh_wsp",  "t_c10_incin",  "incin_wsp",  "main_air_pv",  "mainair_wsp",  "air_act",  "top_act",  "btm_act",  "avg_top_kiln_temp",  "avg_bot_kiln_temp",  "avg_kiln_temp",  "max_temp",  "x8",  "x9")