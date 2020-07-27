# rm(list = ls())

# load stuff
library(tidyverse)
library(magrittr)
library(scales)

theme_set(theme_minimal())

source("src/kilnFunctions.R")
source("src/userFunctions.R")

# import data -------------------------------------------------------------
df_merged    <- read_csv("data/processed_data/df_merged.csv")
df_yields    <- read_csv("data/processed_data/df_yields.csv")
df_defects   <- read_csv("data/processed_data/df_defects.csv")

allAucValues <- read_csv("data/processed_data/kilnsAucValues.csv")

# kilns_AB     <- read_csv("data/processed_data/kilns_AB.csv") %>% mutate_if(is.character,as.factor)
kilns_C      <- read_csv("data/processed_data/kilns_C.csv")  %>% mutate_if(is.character,as.factor)
kilns_D      <- read_csv("data/processed_data/kilns_D.csv")  %>% mutate_if(is.character,as.factor)
kilns_E      <- read_csv("data/processed_data/kilns_E.csv")  %>% mutate_if(is.character,as.factor)
kilns_F      <- read_csv("data/processed_data/kilns_F.csv")  %>% mutate(kiln = "F") %>% mutate_if(is.character,as.factor) 
kilns_G      <- read_csv("data/processed_data/kilns_G.csv")  %>% mutate_if(is.character,as.factor)
kilns_H      <- read_csv("data/processed_data/kilns_H.csv")  %>% mutate_if(is.character,as.factor)

kilns_All <- bind_rows(kilns_C[c(1:10,22,23)], 
                       kilns_D[c(1:10,22,23)], 
                       kilns_E[c(1:10,22,23)], 
                       kilns_F[c(1:10,22,23)], 
                       kilns_G[c(1:10,31,32)], 
                       kilns_H[c(1:10,40,41)])

# Goals -------------------------------------------------------------------

# Features:
  # Area between setpoint and temp in 400-600F temp range (done)
  # Time spent at positive pressure between 0 and 1000 minutes (replaced)
  # Time when (0.01 < pressure < 0.03) but in what range? before maxtemp? (done)
  # Area under positive pressure curve up to 1200F (done)
  # Area under negative pressure curve up to 1200F (done)
  # SD of pressure up to 1200F
  # mean of pressure up to 1200F
  # slope of pressure up to 1200F

# Single lot calcs -------------------------------------------------------------

t_lot <- sample(c(levels(kilns_H$LOTNO),levels(kilns_G$LOTNO)),1)
# t_lot <- sample(levels(kilns_H$LOTNO),1)
# test <- kilns_All[kilns_All$LOTNO == t_lot,]
test <- kilns_All[kilns_All$LOTNO == "101719H",]

test <- test %>% 
  mutate(
    pressure = ifelse(pressure < -.07, -0.07,pressure),
    pressure = ifelse(pressure > .07, 0.07,pressure),
    
    # find time index where temp reaches 1200F
    close_1200  = if_else( (time < index_max_temp), (abs(1200 - avg_kiln_temp)), NULL ),
    index_1200F = which.min(close_1200),
    
    press_greater_03 = if_else((pressure >  .03) & (time < index_1200F), pressure, NULL),
    press_betw_01_03 = if_else((pressure >= .01) & (pressure <= .03) & (time < index_1200F), pressure, NULL),
    press_betw_00_01 = if_else((pressure <  .01) & (pressure >= 0) & (time < index_1200F), pressure, NULL),
    press_less_00    = if_else((pressure <  0) & (time < index_1200F), pressure, NULL),
    
    # is pressure pos/neg?
    press_pos = if_else((pressure >=  0) & (time < index_1200F), pressure, NULL),
    press_neg = if_else((pressure < 0) & (time < index_1200F), pressure, NULL),
    
    # add counters to sum times
    press_pos_count = if_else(!is.na(press_pos),1,0),
    press_neg_count = if_else(!is.na(press_neg),1,0),
    
    press_greater_03_count = if_else(!is.na(press_greater_03),1,0),
    press_betw_01_03_count = if_else(!is.na(press_betw_01_03),1,0),
    press_betw_00_01_count = if_else(!is.na(press_betw_00_01),1,0),
    press_less_00_count    = if_else(!is.na(press_less_00),1,0)
    
    
  ) %>% 
  dplyr::select(-c(close_1200)) %>% 
  dplyr::select(c(LOTNO,index_max_temp,index_1200F,everything()))

# nrow(test_joined[(test_joined$pressure >= 0) & (test_joined$time < 1018),]) # pos
# nrow(test_joined[(test_joined$pressure < 0) & (test_joined$time < 1018),])  # neg

# extract summary values from new df
test_summ <- test %>% 
  group_by(LOTNO) %>% 
  dplyr::summarise(
    auc_pos_pres      = sum(press_pos,na.rm = TRUE),
    time_at_pos       = sum(press_pos_count,na.rm=TRUE),
    
    auc_neg_pres      = sum(abs(press_neg),na.rm = TRUE),
    time_at_neg       = sum(press_neg_count,na.rm=TRUE),
    
    auc_betw_01_03     = sum(abs(press_betw_01_03-.01), na.rm=TRUE),
    time_betw_01_03    = sum(press_betw_01_03_count,na.rm=TRUE),
    
    auc_greater_03     = sum(abs(press_greater_03-.03), na.rm=TRUE),
    time_greater_03    = sum(press_greater_03_count,na.rm=TRUE),
    
    auc_betw_00_01     = sum(abs(press_betw_00_01), na.rm=TRUE),
    time_betw_00_01    = sum(press_betw_00_01_count,na.rm=TRUE),
    
    auc_less_00        = sum(abs(press_less_00)   , na.rm=TRUE),
    time_less_00       = sum(press_less_00_count,na.rm=TRUE)
    )

# join summary values to original

test_joined <- test %>% 
  left_join(test_summ) %>% 
  mutate(
    pct_time_at_pos =     time_at_pos/(index_1200F-1),
    pct_time_at_neg =     time_at_neg/(index_1200F-1),
    pct_time_greater_03 = time_greater_03/(index_1200F-1),
    pct_time_betw_01_03 = time_betw_01_03/(index_1200F-1),
    pct_time_betw_00_01 = time_betw_00_01/(index_1200F-1),
    pct_time_less_00 =    time_less_00/(index_1200F-1)
  ) %>% 
  dplyr::select(-c(press_pos_count,
                   press_neg_count,
                   press_greater_03_count,
                   press_betw_01_03_count,
                   press_betw_00_01_count,
                   press_less_00_count))

# fix time axis?
# start_date <- test$date[[1]]
# test_joined <- test_joined %>%  
#   mutate(
#     time2 = as.POSIXct(as.character(start_date)) + as.difftime(time, unit='mins'),
#     time3 = floor(time/60)
#     )
# alternative
# test_joined$time3 <- as.POSIXct(as.character(test$date[[1]])) + as.difftime(test$time, unit="mins")

# plot 
sec_y_scale=20000
sec_y_shift=1500

g.title = as.character(unique(test_joined$LOTNO))

test_joined %>% 
  ggplot(aes(x=time,y=avg_kiln_temp))+
  geom_line()+
  geom_line(aes(y=setpoint),color='grey50',linetype='dashed')+
  
  geom_hline(aes(yintercept = .00 * sec_y_scale + sec_y_shift),color='red')+
  geom_hline(aes(yintercept = .01 * sec_y_scale + sec_y_shift),color='red',linetype='dashed')+
  geom_hline(aes(yintercept = .03 * sec_y_scale + sec_y_shift),color='red',linetype='dashed')+
  geom_vline(aes(xintercept = index_1200F),color='red',linetype='dashed')+

  # points
  # geom_point(aes(y=press_betw_01_03 * sec_y_scale + sec_y_shift),alpha=1,size=.1,color='green')+
  # geom_point(aes(y=press_greater_03 * sec_y_scale + sec_y_shift),alpha=1,size=.1,color='red')+
  # geom_point(aes(y=press_betw_00_01 * sec_y_scale + sec_y_shift),alpha=1,size=.1,color='yellow2')+
  # geom_point(aes(y=press_less_00    * sec_y_scale + sec_y_shift),alpha=1,size=.1,color='red')+

  # ribbons
  geom_ribbon(aes(ymin = 0.01 * sec_y_scale + sec_y_shift,
                  ymax = pressure * sec_y_scale + sec_y_shift),fill='red',alpha=1)+
  # geom_ribbon(aes(ymin = 0 * sec_y_scale + sec_y_shift,
  #                 ymax = press_betw_00_01 * sec_y_scale + sec_y_shift),fill='yellow3',alpha=1)+
  # geom_ribbon(aes(ymin = 0 * sec_y_scale + sec_y_shift,
  #                 ymax = press_betw_01_03 * sec_y_scale + sec_y_shift),fill='green',alpha=1)+
  # geom_ribbon(aes(ymin = 0 * sec_y_scale + sec_y_shift,
  #                 ymax = press_greater_03 * sec_y_scale + sec_y_shift),fill='red',alpha=1)+
  # geom_ribbon(aes(ymin = press_less_00 * sec_y_scale + sec_y_shift,
  #                 ymax = 0 * sec_y_scale + sec_y_shift),fill='red',alpha=1)+
  # geom_ribbon(aes(ymin = 0 * sec_y_scale + sec_y_shift,
  #                 ymax = press_betw_00_01 * sec_y_scale + sec_y_shift),fill='yellow3',alpha=1)+
  # geom_ribbon(aes(ymin = 0 * sec_y_scale + sec_y_shift,
  #                 ymax = press_betw_01_03 * sec_y_scale + sec_y_shift),fill='green',alpha=1)+
  # geom_ribbon(aes(ymin = 0 * sec_y_scale + sec_y_shift,
  #                 ymax = press_greater_03 * sec_y_scale + sec_y_shift),fill='red',alpha=1)+
  # scale_x_continuous(limits = c(0,18.4*60))+
  # scale_y_continuous(limits = c(1250,2350))
  geom_ribbon(aes(ymin = auc_min, ymax = auc_max),fill='pink',alpha=1)+
  
  scale_y_continuous(name = "Kiln temp (F)",breaks = sort(c(seq(0,3000,1000),400,600,1200)),
                     sec.axis = sec_axis(~. / sec_y_scale - (sec_y_shift / sec_y_scale),
                                         name = "Pressure",
                                         breaks = seq(-.1,.1,.01)
                                         )
                     )+
  scale_x_continuous(name = "Hours", labels = number_format(scale=1/60),
                     breaks = sort(c(seq(0,100*60,12*60),unique(test$index_1200F)))
                     )+
  labs(title = g.title)+
  
  # labels
  geom_label(data = test_joined %>% distinct(auc_pos_pres, time_at_pos, pct_time_at_pos, index_1200F),
             aes(x = 60 * 72, y = -0.04 * sec_y_scale + sec_y_shift,
                 label = paste0(
                   round(auc_pos_pres, 1), "a, ", 
                   time_at_pos, "m, ",
                   mypercent(pct_time_at_pos))
             ),
             label.padding = unit(0.2, "lines"),
             hjust=1,vjust=.5,
             fill='grey90'
  ) +
  geom_label(data = test_joined %>% distinct(auc_neg_pres, time_at_neg, pct_time_at_neg, index_1200F),
             aes(x = 60 * 72, y = -0.06 * sec_y_scale + sec_y_shift,
                 label = paste0(
                   round(auc_neg_pres, 1), "a, ", 
                   time_at_neg, "m, ",
                   mypercent(pct_time_at_neg))
             ),
             label.padding = unit(0.2, "lines"),
             hjust=1,vjust=.5,
             fill='grey80'
  )+
  geom_label(data = test_joined %>% distinct(auc_less_00, time_less_00, pct_time_less_00, index_1200F),
             aes(x = 60 * 72, y = -.02 * sec_y_scale + sec_y_shift,
                 label = paste0(
                   round(auc_less_00, 1), "a, ", 
                   time_less_00, "m, ",
                   mypercent(pct_time_less_00))
                 ),
             label.padding = unit(0.2, "lines"),
             hjust=1,vjust=.5,
             fill='red'
             )+
  geom_label(data = test_joined %>% distinct(auc_betw_00_01, time_betw_00_01, pct_time_betw_00_01, index_1200F),
             aes(x = 60 * 72, y = 0.0 * sec_y_scale + sec_y_shift,
                 label = paste0(
                   round(auc_betw_00_01, 1), "a, ", 
                   time_betw_00_01, "m, ",
                   mypercent(pct_time_betw_00_01))
                 ),
             label.padding = unit(0.2, "lines"),
             hjust=1,vjust=.5,
             fill='yellow3'
             ) +
  geom_label(data = test_joined %>% distinct(auc_betw_01_03, time_betw_01_03, pct_time_betw_01_03, index_1200F),
             aes(x = 60 * 72, y = .02 * sec_y_scale + sec_y_shift,
                 label = paste0(
                   round(auc_betw_01_03, 1), "a, ", 
                   time_betw_01_03, "m, ",
                   mypercent(pct_time_betw_01_03))
                 ),
             label.padding = unit(0.2, "lines"),
             hjust=1,vjust=.5,
             fill='green'
             ) +
  geom_label(data = test_joined %>% distinct(auc_greater_03, time_greater_03, pct_time_greater_03, index_1200F),
             aes(x = 60 * 72, y = 0.04 * sec_y_scale + sec_y_shift,
                 label = paste0(
                   round(auc_greater_03, 1), "a, ", 
                   time_greater_03, "m, ",
                   mypercent(pct_time_greater_03))
                 ),
             label.padding = unit(0.2, "lines"),
             hjust=1,vjust=.5,
             fill='red'
             )


# Multiple lot --------------------------------------------------

# random lots
# t_lots <- sample(c(levels(kilns_H$LOTNO),levels(kilns_G$LOTNO)),9)
t_lots <- sample(levels(kilns_All$LOTNO),9)
# t_lot <- sample(levels(kilns_H$LOTNO),1)
test <- kilns_All[kilns_All$LOTNO %in% t_lots,]

# test[test$pressure > .07,] %>% view()
# nrow(test[test$pressure > .07,])
test <- test %>% 
  group_by(LOTNO) %>% 
  mutate(

    # adust pressure max and min
    pressure = case_when(pressure > .07 ~ .07,
                         pressure < -.07 ~ -.07,
                         TRUE ~ pressure
                         ),
    
    # find time index where temp reaches 1200F
    close_1200  = if_else( (time < index_max_temp), (abs(1200 - avg_kiln_temp)), NULL ))

# join time to original
test <- left_join(test, test %>% 
                    group_by(LOTNO) %>% 
                    dplyr::summarise(index_1200F = which.min(close_1200))
                  )

# find pressure ranges and count occurences
test <- test %>% 
  group_by(LOTNO) %>% 
  mutate(

    # is pressure pos/neg?
    press_pos = if_else((pressure >= 0) & (time <= index_1200F), pressure, NULL),
    press_neg = if_else((pressure <  0) & (time <= index_1200F), pressure, NULL),
    
    # pressure in ranges
    press_greater_03 = if_else((pressure >  .03) &                     (time <= index_1200F), pressure, NULL),
    press_betw_01_03 = if_else((pressure >= .01) & (pressure <= .03) & (time <= index_1200F), pressure, NULL),
    press_betw_00_01 = if_else((pressure <  .01) & (pressure >=   0) & (time <= index_1200F), pressure, NULL),
    press_less_00    = if_else((pressure <    0) &                     (time <= index_1200F), pressure, NULL),
    
    # add counters to sum times
    press_pos_count = if_else(!is.na(press_pos),1,0),
    press_neg_count = if_else(!is.na(press_neg),1,0),
    
    press_greater_03_count = if_else(!is.na(press_greater_03),1,0),
    press_betw_01_03_count = if_else(!is.na(press_betw_01_03),1,0),
    press_betw_00_01_count = if_else(!is.na(press_betw_00_01),1,0),
    press_less_00_count    = if_else(!is.na(press_less_00),1,0),
    
    # get pressures below 1200F point for mean/sd
    press_1200F = if_else(time <= index_1200F, pressure, NULL)
    
    
  ) %>% 
  dplyr::select(-c(close_1200)) %>% 
  dplyr::select(c(LOTNO,index_max_temp,index_1200F,everything()))

# summarise AUC and times at pressures
test_summ <- test %>% 
      group_by(LOTNO) %>% 
      dplyr::summarise(
        auc_pos_press = sum(press_pos,na.rm = TRUE),
        time_at_pos   = sum(press_pos_count,na.rm=TRUE),
        
        auc_neg_press = sum(abs(press_neg),na.rm = TRUE),
        time_at_neg   = sum(press_neg_count,na.rm=TRUE),
        
        auc_betw_01_03     = sum(abs(press_betw_01_03-.01), na.rm=TRUE),
        time_betw_01_03    = sum(press_betw_01_03_count,na.rm=TRUE),
        
        auc_greater_03     = sum(abs(press_greater_03-.03), na.rm=TRUE),
        time_greater_03    = sum(press_greater_03_count,na.rm=TRUE),
        
        auc_betw_00_01     = sum(abs(press_betw_00_01), na.rm=TRUE),
        time_betw_00_01    = sum(press_betw_00_01_count,na.rm=TRUE),
        
        auc_less_00        = sum(abs(press_less_00)   , na.rm=TRUE),
        time_less_00       = sum(press_less_00_count,na.rm=TRUE),
        
        mean_press = mean(press_1200F, na.rm=TRUE),
        sd_press   = sd(press_1200F, na.rm=TRUE)
      )

# join original to summary data
test_joined <- left_join(test, test_summ) %>% 
  mutate(
    pct_time_at_pos =     time_at_pos/index_1200F,
    pct_time_at_neg =     time_at_neg/index_1200F,
    pct_time_greater_03 = time_greater_03/index_1200F,
    pct_time_betw_01_03 = time_betw_01_03/index_1200F,
    pct_time_betw_00_01 = time_betw_00_01/index_1200F,
    pct_time_less_00 =    time_less_00/index_1200F
    ) %>% 
  dplyr::select(-c(press_pos_count,
                   press_neg_count,
                   press_greater_03_count,
                   press_betw_01_03_count,
                   press_betw_00_01_count,
                   press_less_00_count,
                   press_1200F
                   ))

# plot
test_joined %>% 
  
  # kiln temp, setpoint, pressure
  ggplot(aes(x=time))+
  geom_line(aes(y=avg_kiln_temp))+
  geom_line(aes(y=setpoint),color='grey50',linetype='dashed')+
  geom_point(aes(y=pressure * sec_y_scale + sec_y_shift),alpha=.1,size=.1)+
  
  # color points
  geom_point(aes(y=press_betw_01_03 * sec_y_scale + sec_y_shift),alpha=1,size=.1,color='green')+
  geom_point(aes(y=press_greater_03 * sec_y_scale + sec_y_shift),alpha=1,size=.1,color='red')+
  geom_point(aes(y=press_betw_00_01 * sec_y_scale + sec_y_shift),alpha=1,size=.1,color='yellow2')+
  geom_point(aes(y=press_less_00    * sec_y_scale + sec_y_shift),alpha=1,size=.1,color='red')+
  
  # hlines
  geom_hline(aes(yintercept = .0 * sec_y_scale + sec_y_shift),color='red')+
  # geom_hline(aes(yintercept = .01 * sec_y_scale + sec_y_shift),color='red',linetype='dashed')+
  # geom_hline(aes(yintercept = .03 * sec_y_scale + sec_y_shift),color='red',linetype='dashed')+
  # mean/sd
  geom_hline(aes(yintercept = mean_press * sec_y_scale + sec_y_shift),            color = 'blue',linetype='dotdash',size=1)+
  geom_hline(aes(yintercept = (mean_press+sd_press) * sec_y_scale + sec_y_shift), color = 'blue2',linetype='dotted',size=1)+
  geom_hline(aes(yintercept = (mean_press-sd_press) * sec_y_scale + sec_y_shift), color = 'blue2',linetype='dotted',size=1)+
  
  # ribbons
  # geom_ribbon(aes(ymin = press_less_00 * sec_y_scale + sec_y_shift, ymax = 0 * sec_y_scale + sec_y_shift),fill='red',alpha=1)+
  # geom_ribbon(aes(ymin = 0.00 * sec_y_scale + sec_y_shift, ymax = press_betw_00_01 * sec_y_scale + sec_y_shift),fill='yellow3',alpha=1)+
  # geom_ribbon(aes(ymin = 0.0 * sec_y_scale + sec_y_shift, ymax = press_betw_01_03 * sec_y_scale + sec_y_shift),fill='green',alpha=1)+
  # geom_ribbon(aes(ymin = 0.0 * sec_y_scale + sec_y_shift, ymax = press_greater_03 * sec_y_scale + sec_y_shift),fill='red',alpha=1)+
  # zoom on ribbons
  # scale_x_continuous(limits = c(0,18.4*60))+
  # scale_y_continuous(limits = c(1250,2350))
  
  # auc 400-600
  # geom_ribbon(aes(ymin = auc_min, ymax = auc_max),fill='pink',alpha=1)+
  
  scale_y_continuous(name = "Kiln temp (F)",
                     breaks = sort(c(seq(0,3000,1000),400,600,1200)),
                     sec.axis = sec_axis(~. / sec_y_scale - (sec_y_shift / sec_y_scale),
                                         name = "Pressure",
                                         breaks = seq(-.1,.1,.01)
                     )
  )+
  scale_x_continuous(name = "Hours", 
                     labels = number_format(scale=1/60),
                     breaks = sort(c(seq(0,100*60,12*60)))
  )+
  
  # labels
  geom_label(data = test_joined %>% distinct(mean_press),
             aes(x = 60 * 30, y = -0.04 * sec_y_scale + sec_y_shift,
                 label = paste0( "mean: ", round(mean_press,3) )
                 ),
                 label.padding = unit(0.2, "lines"),
                 hjust=1,vjust=.5,
                 fill='lightskyblue',
                 label.size=.1
  ) +
  geom_label(data = test_joined %>% distinct(sd_press),
             aes(x = 60 * 30, y = -0.06 * sec_y_scale + sec_y_shift,
                 label = paste0( "sd: ", round(sd_press,3) )
                 ),
             label.padding = unit(0.2, "lines"),
             hjust=1,vjust=.5,
             fill='lightskyblue2',
             label.size=.1
             ) +
  geom_label(data = test_joined %>% distinct(auc_pos_press, time_at_pos, pct_time_at_pos, index_1200F),
             aes(x = 60 * 72, y = -0.04 * sec_y_scale + sec_y_shift,
                 label = paste0( round(auc_pos_press, 1), "a, ", 
                                 time_at_pos, "m, ",
                                 mypercent(pct_time_at_pos))
                 ),
             label.padding = unit(0.2, "lines"),
             hjust=1,vjust=.5,
             fill='grey80',
             label.size=.1
             ) +
  geom_label(data = test_joined %>% distinct(auc_neg_press, time_at_neg, pct_time_at_neg, index_1200F),
             aes(x = 60 * 72, y = -0.06 * sec_y_scale + sec_y_shift,
                 label = paste0( round(auc_neg_press, 1), "a, ", 
                                 time_at_neg, "m, ",
                                 mypercent(pct_time_at_neg))
                 ),
             label.padding = unit(0.2, "lines"),
             hjust=1,vjust=.5,
             fill='grey90'
             )+
  geom_label(data = test_joined %>% distinct(auc_less_00, time_less_00, pct_time_less_00, index_1200F),
             aes(x = 60 * 72, y = -.02 * sec_y_scale + sec_y_shift,
                 label = paste0( round(auc_less_00, 1), "a, ", 
                                 time_less_00, "m, ",
                                 mypercent(pct_time_less_00))
                 ),
             label.padding = unit(0.2, "lines"),
             hjust=1,vjust=.5,
             fill='red'
             )+
  geom_label(data = test_joined %>% distinct(auc_betw_00_01, time_betw_00_01, pct_time_betw_00_01, index_1200F),
             aes(x = 60 * 72, y = 0.0 * sec_y_scale + sec_y_shift,
                 label = paste0( round(auc_betw_00_01, 1), "a, ", 
                                 time_betw_00_01, "m, ",
                                 mypercent(pct_time_betw_00_01))
                 ),
             label.padding = unit(0.2, "lines"),
             hjust=1,vjust=.5,
             fill='yellow3'
             ) +
  geom_label(data = test_joined %>% distinct(auc_betw_01_03, time_betw_01_03, pct_time_betw_01_03, index_1200F),
             aes(x = 60 * 72, y = .02 * sec_y_scale + sec_y_shift,
                 label = paste0( round(auc_betw_01_03, 1), "a, ", 
                                 time_betw_01_03, "m, ",
                                 mypercent(pct_time_betw_01_03))
                 ),
             label.padding = unit(0.2, "lines"),
             hjust=1,vjust=.5,
             fill='green'
             ) +
  geom_label(data = test_joined %>% distinct(auc_greater_03, time_greater_03, pct_time_greater_03, index_1200F),
             aes(x = 60 * 72, y = 0.04 * sec_y_scale + sec_y_shift,
                 label = paste0( round(auc_greater_03, 1), "a, ", 
                                 time_greater_03, "m, ",
                                 mypercent(pct_time_greater_03))
                 ),
             label.padding = unit(0.2, "lines"),
             hjust=1,vjust=.5,
             fill='red'
             ) +
  facet_wrap(~LOTNO)



# all lots and join -------------------------------------------------------

press_calcs <- kilns_All %>% 
  group_by(LOTNO) %>% 
  mutate(
    
    # adust pressure max and min
    pressure = case_when(pressure > .07 ~ .07,
                         pressure < -.07 ~ -.07,
                         TRUE ~ pressure
    ),
    
    # find time index where temp reaches 1200F
    close_1200  = if_else( (time < index_max_temp), (abs(1200 - avg_kiln_temp)), NULL ))

# join time to original
press_calcs <- left_join(press_calcs, press_calcs %>% 
                           group_by(LOTNO) %>% 
                           dplyr::summarise(index_1200F = which.min(close_1200))
                         )

# find pressure ranges and count occurences
press_calcs <- press_calcs %>% 
  group_by(LOTNO) %>% 
  mutate(
    
    # is pressure pos/neg?
    press_pos = if_else((pressure >= 0) & (time <= index_1200F), pressure, NULL),
    press_neg = if_else((pressure <  0) & (time <= index_1200F), pressure, NULL),
    
    # pressure in ranges
    press_greater_03 = if_else((pressure >  .03) &                     (time <= index_1200F), pressure, NULL),
    press_betw_01_03 = if_else((pressure >= .01) & (pressure <= .03) & (time <= index_1200F), pressure, NULL),
    press_betw_00_01 = if_else((pressure <  .01) & (pressure >=   0) & (time <= index_1200F), pressure, NULL),
    press_less_00    = if_else((pressure <    0) &                     (time <= index_1200F), pressure, NULL),
    
    # add counters to sum times
    press_pos_count = if_else(!is.na(press_pos),1,0),
    press_neg_count = if_else(!is.na(press_neg),1,0),
    
    press_greater_03_count = if_else(!is.na(press_greater_03),1,0),
    press_betw_01_03_count = if_else(!is.na(press_betw_01_03),1,0),
    press_betw_00_01_count = if_else(!is.na(press_betw_00_01),1,0),
    press_less_00_count    = if_else(!is.na(press_less_00),1,0),
    
    # get pressures below 1200F point for mean/sd
    press_1200F = if_else(time <= index_1200F, pressure, NULL)
  ) %>% 
  dplyr::select(-c(close_1200)) %>% 
  dplyr::select(c(LOTNO,index_max_temp,index_1200F,everything()))

# summarise AUC and times at pressures
press_summ <- press_calcs %>% 
  group_by(LOTNO) %>% 
  dplyr::summarise(
    auc_pos_press = sum(press_pos,na.rm = TRUE),
    time_at_pos   = sum(press_pos_count,na.rm=TRUE),
    
    auc_neg_press = sum(abs(press_neg),na.rm = TRUE),
    time_at_neg   = sum(press_neg_count,na.rm=TRUE),
    
    auc_betw_01_03     = sum(abs(press_betw_01_03-.01), na.rm=TRUE),
    time_betw_01_03    = sum(press_betw_01_03_count,na.rm=TRUE),
    
    auc_greater_03     = sum(abs(press_greater_03-.03), na.rm=TRUE),
    time_greater_03    = sum(press_greater_03_count,na.rm=TRUE),
    
    auc_betw_00_01     = sum(abs(press_betw_00_01), na.rm=TRUE),
    time_betw_00_01    = sum(press_betw_00_01_count,na.rm=TRUE),
    
    auc_less_00        = sum(abs(press_less_00)   , na.rm=TRUE),
    time_less_00       = sum(press_less_00_count,na.rm=TRUE),
    
    mean_press = mean(press_1200F, na.rm=TRUE),
    sd_press   = sd(press_1200F, na.rm=TRUE)
  )

# join original to summary data
press_joined <- left_join(press_calcs, press_summ) %>% 
  mutate(
    pct_time_at_pos =     time_at_pos/index_1200F,
    pct_time_at_neg =     time_at_neg/index_1200F,
    pct_time_greater_03 = time_greater_03/index_1200F,
    pct_time_betw_01_03 = time_betw_01_03/index_1200F,
    pct_time_betw_00_01 = time_betw_00_01/index_1200F,
    pct_time_less_00 =    time_less_00/index_1200F
  ) %>% 
  dplyr::select(-c(press_pos_count,
                   press_neg_count,
                   press_greater_03_count,
                   press_betw_01_03_count,
                   press_betw_00_01_count,
                   press_less_00_count,
                   press_1200F
  ))

# plot
sample <- sample(levels(kilns_All$LOTNO),9)
sec_y_scale=20000
sec_y_shift=1500
press_joined %>% 
  dplyr::filter(LOTNO %in% sample) %>% 
  # dplyr::filter(LOTNO == "") %>% 
  
  # kiln temp, setpoint, pressure
  ggplot(aes(x=time))+
  geom_line(aes(y=avg_kiln_temp))+
  geom_line(aes(y=setpoint),color='grey50',linetype='dashed')+
  geom_point(aes(y=pressure * sec_y_scale + sec_y_shift),alpha=.1,size=.1)+
  
  # color points
  geom_point(aes(y=press_betw_01_03 * sec_y_scale + sec_y_shift),alpha=1,size=.1,color='green')+
  geom_point(aes(y=press_greater_03 * sec_y_scale + sec_y_shift),alpha=1,size=.1,color='red')+
  geom_point(aes(y=press_betw_00_01 * sec_y_scale + sec_y_shift),alpha=1,size=.1,color='yellow2')+
  geom_point(aes(y=press_less_00    * sec_y_scale + sec_y_shift),alpha=1,size=.1,color='red')+
  
  # hlines
  geom_hline(aes(yintercept = .0 * sec_y_scale + sec_y_shift),color='red')+
  geom_hline(aes(yintercept = .01 * sec_y_scale + sec_y_shift),color='red',linetype='dashed')+
  geom_hline(aes(yintercept = .03 * sec_y_scale + sec_y_shift),color='red',linetype='dashed')+
  # mean/sd
  geom_hline(aes(yintercept = mean_press * sec_y_scale + sec_y_shift),            color = 'blue',linetype='dotdash',size=1)+
  geom_hline(aes(yintercept = (mean_press+sd_press) * sec_y_scale + sec_y_shift), color = 'blue2',linetype='dotted',size=1)+
  geom_hline(aes(yintercept = (mean_press-sd_press) * sec_y_scale + sec_y_shift), color = 'blue2',linetype='dotted',size=1)+
  
  # ribbons
  # geom_ribbon(aes(ymin = press_less_00 * sec_y_scale + sec_y_shift, ymax = 0 * sec_y_scale + sec_y_shift),fill='red',alpha=1)+
  # geom_ribbon(aes(ymin = 0.00 * sec_y_scale + sec_y_shift, ymax = press_betw_00_01 * sec_y_scale + sec_y_shift),fill='yellow3',alpha=1)+
  # geom_ribbon(aes(ymin = 0.0 * sec_y_scale + sec_y_shift, ymax = press_betw_01_03 * sec_y_scale + sec_y_shift),fill='green',alpha=1)+
  # geom_ribbon(aes(ymin = 0.0 * sec_y_scale + sec_y_shift, ymax = press_greater_03 * sec_y_scale + sec_y_shift),fill='red',alpha=1)+
  # zoom on ribbons
  # scale_x_continuous(limits = c(0,18.4*60))+
  # scale_y_continuous(limits = c(1250,2350))
  
  # auc 400-600
geom_ribbon(aes(ymin = auc_min, ymax = auc_max),fill='pink',alpha=1)+

scale_y_continuous(name = "Kiln temp (F)",
                   breaks = sort(c(seq(0,3000,1000),400,600,1200)),
                   sec.axis = sec_axis(~. / sec_y_scale - (sec_y_shift / sec_y_scale),
                                       name = "Pressure",
                                       breaks = seq(-.1,.1,.01)
                   )
)+
  scale_x_continuous(name = "Hours", 
                     labels = number_format(scale=1/60),
                     breaks = sort(c(seq(0,100*60,12*60)))
  )+
  
  # labels
  geom_label(data = press_joined %>% dplyr::filter(LOTNO %in% sample) %>% distinct(mean_press),
             aes(x = 60 * 30, y = -0.04 * sec_y_scale + sec_y_shift,
                 label = paste0( "mean: ", round(mean_press,3) )
             ),
             label.padding = unit(0.2, "lines"),
             hjust=1,vjust=.5,
             fill='lightskyblue',
             label.size=.1
  ) +
  geom_label(data = press_joined %>% dplyr::filter(LOTNO %in% sample) %>% distinct(sd_press),
             aes(x = 60 * 30, y = -0.06 * sec_y_scale + sec_y_shift,
                 label = paste0( "sd: ", round(sd_press,3) )
             ),
             label.padding = unit(0.2, "lines"),
             hjust=1,vjust=.5,
             fill='lightskyblue2',
             label.size=.1
  ) +
  geom_label(data = press_joined %>% dplyr::filter(LOTNO %in% sample) %>% distinct(auc_pos_press, time_at_pos, pct_time_at_pos, index_1200F),
             aes(x = 60 * 72, y = -0.04 * sec_y_scale + sec_y_shift,
                 label = paste0( round(auc_pos_press, 1), "a, ", 
                                 time_at_pos, "m, ",
                                 mypercent(pct_time_at_pos))
             ),
             label.padding = unit(0.2, "lines"),
             hjust=1,vjust=.5,
             fill='grey80',
             label.size=.1
  ) +
  geom_label(data = press_joined %>% dplyr::filter(LOTNO %in% sample) %>% distinct(auc_neg_press, time_at_neg, pct_time_at_neg, index_1200F),
             aes(x = 60 * 72, y = -0.06 * sec_y_scale + sec_y_shift,
                 label = paste0( round(auc_neg_press, 1), "a, ", 
                                 time_at_neg, "m, ",
                                 mypercent(pct_time_at_neg))
             ),
             label.padding = unit(0.2, "lines"),
             hjust=1,vjust=.5,
             fill='grey90'
  )+
  geom_label(data = press_joined %>% dplyr::filter(LOTNO %in% sample) %>% distinct(auc_less_00, time_less_00, pct_time_less_00, index_1200F),
             aes(x = 60 * 72, y = -.02 * sec_y_scale + sec_y_shift,
                 label = paste0( round(auc_less_00, 1), "a, ", 
                                 time_less_00, "m, ",
                                 mypercent(pct_time_less_00))
             ),
             label.padding = unit(0.2, "lines"),
             hjust=1,vjust=.5,
             fill='red'
  )+
  geom_label(data = press_joined %>% dplyr::filter(LOTNO %in% sample) %>% distinct(auc_betw_00_01, time_betw_00_01, pct_time_betw_00_01, index_1200F),
             aes(x = 60 * 72, y = 0.0 * sec_y_scale + sec_y_shift,
                 label = paste0( round(auc_betw_00_01, 1), "a, ", 
                                 time_betw_00_01, "m, ",
                                 mypercent(pct_time_betw_00_01))
             ),
             label.padding = unit(0.2, "lines"),
             hjust=1,vjust=.5,
             fill='yellow3'
  ) +
  geom_label(data = press_joined %>% dplyr::filter(LOTNO %in% sample) %>% distinct(auc_betw_01_03, time_betw_01_03, pct_time_betw_01_03, index_1200F),
             aes(x = 60 * 72, y = .02 * sec_y_scale + sec_y_shift,
                 label = paste0( round(auc_betw_01_03, 1), "a, ", 
                                 time_betw_01_03, "m, ",
                                 mypercent(pct_time_betw_01_03))
             ),
             label.padding = unit(0.2, "lines"),
             hjust=1,vjust=.5,
             fill='green'
  ) +
  geom_label(data = press_joined %>% dplyr::filter(LOTNO %in% sample) %>% distinct(auc_greater_03, time_greater_03, pct_time_greater_03, index_1200F),
             aes(x = 60 * 72, y = 0.04 * sec_y_scale + sec_y_shift,
                 label = paste0( round(auc_greater_03, 1), "a, ", 
                                 time_greater_03, "m, ",
                                 mypercent(pct_time_greater_03))
             ),
             label.padding = unit(0.2, "lines"),
             hjust=1,vjust=.5,
             fill='red'
  ) +
  facet_wrap(~LOTNO)

