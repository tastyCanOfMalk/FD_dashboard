
# rm(list = ls())

# load stuff
library(tidyverse)
library(magrittr)
# library(plotly)
# library(ggpointdensity)
# library(viridis)
# library(knitr)
# library(kableExtra)

theme_set(theme_minimal())

source("src/kilnFunctions.R")
source("src/userFunctions.R")

# import data -------------------------------------------------------------
df_merged    <- read_csv("data/processed_data/df_merged.csv")
df_yields    <- read_csv("data/processed_data/df_yields.csv")
df_defects   <- read_csv("data/processed_data/df_defects.csv")

allAucValues <- read_csv("data/processed_data/kilnsAucValues.csv")

kilns_AB     <- read_csv("data/processed_data/kilns_AB.csv") %>% mutate_if(is.character,as.factor)
kilns_C      <- read_csv("data/processed_data/kilns_C.csv")  %>% mutate_if(is.character,as.factor)
kilns_D      <- read_csv("data/processed_data/kilns_D.csv")  %>% mutate_if(is.character,as.factor)
kilns_E      <- read_csv("data/processed_data/kilns_E.csv")  %>% mutate_if(is.character,as.factor)
kilns_F      <- read_csv("data/processed_data/kilns_F.csv")  %>% mutate(kiln = "F") %>% mutate_if(is.character,as.factor) 
kilns_G      <- read_csv("data/processed_data/kilns_G.csv")  %>% mutate_if(is.character,as.factor)
kilns_H      <- read_csv("data/processed_data/kilns_H.csv")  %>% mutate_if(is.character,as.factor)

kilns_All <- bind_rows(kilns_AB[c(1:10,16,17)], 
                        kilns_C[c(1:10,22,23)], 
                        kilns_D[c(1:10,22,23)], 
                        kilns_E[c(1:10,22,23)], 
                        kilns_F[c(1:10,22,23)], 
                        kilns_G[c(1:10,31,32)], 
                        kilns_H[c(1:10,40,41)])


# Goals -------------------------------------------------------------------

# Features:
  # Area between setpoint and temp in 400-600F temp range (done)
  # Time spent at positive pressure between 0 and 1000 minutes
  # Time when (0.01 < pressure < 0.03) but in what range? before maxtemp?
  # Area under positive pressure curve up to 1200F
  # Area under negative pressure curve up to 1200F
  # SD of pressure up to 1200F
  # mean of pressure up to 1200F
  # slope of pressure up to 1200F

# PRESSURE setup  ---------------------------------------------------------

# set.seed(1)
# 053018H (negative vals)
# 022118H (pos and neg vals)
# 080918H
# 110519H (pos/neg/in range.1.3)
# 051918G (little of all)
# 042018G
# 102318G
t_lot <- sample(levels(kilns_H$LOTNO),1)
test <- kilns_All[kilns_All$LOTNO == t_lot,]

test <- test %>% 
  mutate(
    
    # Pressure positive and below 1000-minutes?
    pos_pres_1000 = case_when(
      pressure <= 0 ~ FALSE,
      (pressure > 0) & (time <= 1000) ~ TRUE,
      TRUE ~ FALSE), 
    
    
    # find time index where temp reaches 1200F
    close_1200 = if_else(
      (time < index_max_temp), (abs(1200 - avg_kiln_temp)), NULL
      ),
    index_1200F = which.min(close_1200),
    
    # is pressure within range 0.01 - 0.03 before 1200F?
    press_auc_max_01 = if_else((pressure > .01) & (pressure <= .03) & (time < index_1200F), pressure, NULL),
    press_auc_max_03 = if_else((pressure > .03) & (time < index_1200F), pressure, NULL),
    betw_1_3 = if_else(
      (pressure >= 0.01) & (pressure <= 0.03) & (time < index_1200F), TRUE, FALSE
    ),

        # find pos pressures before 1200F index for AUC plotting
    press_auc_max_1200 = if_else((pressure >  0) & (time <= index_1200F), pressure, NULL),
    press_auc_min_1200 = if_else((pressure <= 0) & (time <= index_1200F), pressure, NULL)
    ) %>% 
  
  dplyr::select(-c(close_1200))

# which.min(test$close_1200)
# test <- test %>% 
#   mutate(index_1200F = which.min(test$close_1200))

# time at positive pressure between 0, 1000 minutes:
test %>% dplyr::summarise(pos_pres_1000 = sum(pos_pres_1000,na.rm = TRUE))

sec_y_scale=20000
sec_y_shift=1500

test %>% 
  ggplot(aes(x=time, y=avg_kiln_temp))+
  geom_line(color='blue')+
  geom_line(aes(y=setpoint),color='grey50',linetype='dashed')+
  geom_point(aes(y=pressure * sec_y_scale + sec_y_shift,
                 color=pos_pres_1000),
             size=.3)+
  geom_hline(aes(yintercept = 0 * sec_y_scale + sec_y_shift),color='grey70')+
  geom_vline(aes(xintercept = 1000),color='grey70')+
  geom_vline(aes(xintercept = 0),color='grey70')+
  scale_y_continuous(
    sec.axis = sec_axis(~ . / sec_y_scale - (sec_y_shift / sec_y_scale), 
                        name = "Pressure", 
                        breaks = seq(-.1,.1,.01))
  )+
  theme(axis.text.y        = element_text(color = 'blue'),
        axis.title.y       = element_text(color = 'blue'),
        axis.text.y.right  = element_text(color = 'red'),
        axis.title.y.right = element_text(color = 'red'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        # panel.border = element_blank(),
        panel.background = element_blank())

# AUC of positive pressure when temp <= 1200F
test %>% dplyr::summarise(auc_pos_pres_1200 = sum(press_auc_max_1200,na.rm = TRUE),
                          auc_neg_pres_1200 = sum(abs(press_auc_min_1200),na.rm = TRUE),
                          time_at_pos = sum(press_auc_max_1200/press_auc_max_1200,na.rm=TRUE),
                          time_at_neg = sum(press_auc_min_1200/press_auc_min_1200,na.rm=TRUE))

# sec_y_scale=20000
# sec_y_shift=1500

test %>% 
  ggplot(aes(x=time, y=avg_kiln_temp))+
  geom_line(color='blue')+
  geom_line(aes(y=setpoint),color='grey50',linetype='dashed')+
  geom_hline(aes(yintercept = .0 * sec_y_scale + sec_y_shift),color='red')+
  geom_hline(aes(yintercept = .01 * sec_y_scale + sec_y_shift),color='red',linetype='dashed')+
  geom_hline(aes(yintercept = .03 * sec_y_scale + sec_y_shift),color='red',linetype='dashed')+
  geom_vline(aes(xintercept = index_1200F),color='red',linetype='dotted')+
  geom_point(aes(y=pressure * sec_y_scale + sec_y_shift),size=.3, alpha=.2)+
  geom_smooth(aes(y=pressure * sec_y_scale + sec_y_shift),size=.5)+
  scale_x_continuous(breaks = sort(c(seq(0,5000,1000),unique(test$index_1200F))))+
  scale_y_continuous(
    breaks = sort(c(seq(0,3000,1000),400,600,1200)),
    sec.axis = sec_axis(~ . / sec_y_scale - (sec_y_shift / sec_y_scale), 
                        name = "Pressure", 
                        breaks = seq(-.1,.1,.01))
    )+
  geom_ribbon(aes(ymin = 0 * sec_y_scale + sec_y_shift, ymax = press_auc_max_1200 * sec_y_scale + sec_y_shift),fill='yellow',alpha=.5)+
  geom_ribbon(aes(ymin = press_auc_min_1200 * sec_y_scale + sec_y_shift, ymax = 0 * sec_y_scale + sec_y_shift),fill='red',alpha=.5)+
  geom_ribbon(aes(ymin = auc_min, ymax = auc_max),fill='purple',alpha=.3)


# auc and time betw .01 .03 -----------------------------------------------
t_lot <- sample(levels(kilns_G$LOTNO),1)
test <- kilns_All[kilns_All$LOTNO == t_lot,]

test <- test %>% 
  mutate(
    
    # Pressure positive and below 1000-minutes?
    pos_pres_1000 = case_when(
      pressure <= 0 ~ FALSE,
      (pressure > 0) & (time <= 1000) ~ TRUE,
      TRUE ~ FALSE), 
    
    
    # find time index where temp reaches 1200F
    close_1200 = if_else(
      (time < index_max_temp), (abs(1200 - avg_kiln_temp)), NULL
    ),
    index_1200F = which.min(close_1200),
    
    # is pressure within range 0.01 - 0.03 before 1200F?
    press_auc_max_01 = if_else((pressure > .01) & (pressure <= .03) & (time < index_1200F), pressure, NULL),
    press_auc_max_03 = if_else((pressure > .03) & (time < index_1200F), pressure, NULL),
    betw_1_3 = if_else(
      (pressure >= 0.01) & (pressure <= 0.03) & (time < index_1200F), TRUE, FALSE
    ),
    
    # find pos pressures before 1200F index for AUC plotting
    press_auc_max_1200 = if_else((pressure >  0) & (time <= index_1200F), pressure, NULL),
    press_auc_min_1200 = if_else((pressure <= 0) & (time <= index_1200F), pressure, NULL)
  ) %>% 
  
  dplyr::select(-c(close_1200))

test %>% 
  ggplot(aes(x=time, y=avg_kiln_temp))+
  geom_line(color='blue')+
  geom_line(aes(y=setpoint),color='grey50',linetype='dashed')+
  geom_hline(aes(yintercept = .0 * sec_y_scale + sec_y_shift),color='red')+
  geom_hline(aes(yintercept = .01 * sec_y_scale + sec_y_shift),color='red',linetype='dashed')+
  geom_hline(aes(yintercept = .03 * sec_y_scale + sec_y_shift),color='red',linetype='dashed')+
  geom_vline(aes(xintercept = index_1200F),color='red',linetype='dotted')+
  geom_point(aes(y=pressure * sec_y_scale + sec_y_shift),size=.3, alpha=.2)+
  geom_smooth(aes(y=pressure * sec_y_scale + sec_y_shift),size=.5)+
  scale_x_continuous(breaks = sort(c(seq(0,5000,1000),unique(test$index_1200F))))+
  scale_y_continuous(
    breaks = sort(c(seq(0,3000,1000),400,600,1200)),
    sec.axis = sec_axis(~ . / sec_y_scale - (sec_y_shift / sec_y_scale), 
                        name = "Pressure", 
                        breaks = seq(-.1,.1,.01))
  )+
  geom_ribbon(aes(ymin = 0 * sec_y_scale + sec_y_shift, ymax = press_auc_max_1200 * sec_y_scale + sec_y_shift),fill='blue',alpha=.1)+
  geom_ribbon(aes(ymin = 0.01 * sec_y_scale + sec_y_shift, ymax = press_auc_max_01 * sec_y_scale + sec_y_shift),fill='green',alpha=1)+
  geom_ribbon(aes(ymin = 0.03 * sec_y_scale + sec_y_shift, ymax = press_auc_max_03 * sec_y_scale + sec_y_shift),fill='yellow',alpha=1)+
  geom_ribbon(aes(ymin = press_auc_min_1200 * sec_y_scale + sec_y_shift, ymax = 0 * sec_y_scale + sec_y_shift),fill='red',alpha=1)+
  geom_ribbon(aes(ymin = auc_min, ymax = auc_max),fill='pink',alpha=1)


# start fresh -------------------------------------------------------------

# 032018G

t_lot <- sample(c(levels(kilns_H$LOTNO),levels(kilns_G$LOTNO)),1)
# t_lot <- sample(levels(kilns_H$LOTNO),1)
test <- kilns_All[kilns_All$LOTNO == t_lot,]

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
    press_less_00    = if_else((pressure <  .00) & (time < index_1200F), pressure, NULL),
    
    # is pressure pos/neg?
    press_auc_max_1200 = if_else((pressure >  0) & (time <= index_1200F), pressure, NULL),
    press_auc_min_1200 = if_else((pressure <= 0) & (time <= index_1200F), pressure, NULL)
    
  ) %>% 
  dplyr::select(-c(close_1200)) %>% 
  dplyr::select(c(LOTNO,index_max_temp,index_1200F,everything()))

# test <- test %>% 
#   mutate(
#     dot_colors = case_when(
#       !is.na(press_greater_03) ~ 1,
#       !is.na(press_betw_01_03) ~ 2,
#       !is.na(press_betw_00_01) ~ 3,
#       !is.na(press_less_00)    ~ 4,
#       TRUE ~ 0),
#   )
# test$dot_colors <- as.factor(test$dot_colors)
# test[test$dot_colors == 0,]$dot_colors <- NA


# extract summary values from new df
test_summ <- test %>% 
  group_by(LOTNO) %>% 
  dplyr::summarise(
    auc_pos_pres_1200 = sum(press_auc_max_1200,na.rm = TRUE),
    time_at_pos = sum(press_auc_max_1200/press_auc_max_1200,na.rm=TRUE),
    
    auc_neg_pres_1200 = sum(abs(press_auc_min_1200),na.rm = TRUE),
    time_at_neg = sum(press_auc_min_1200/press_auc_min_1200,na.rm=TRUE),
    
    auc_betw_01_03     = sum(abs(press_betw_01_03-.01), na.rm=TRUE),
    time_betw_01_03    = sum(press_betw_01_03/press_betw_01_03,na.rm=TRUE),
    
    auc_greater_03     = sum(abs(press_greater_03-.03), na.rm=TRUE),
    time_greater_03    = sum(press_greater_03/press_greater_03,na.rm=TRUE),
    
    auc_betw_00_01     = sum(abs(press_betw_00_01), na.rm=TRUE),
    time_betw_00_01    = sum(press_betw_00_01/press_betw_00_01,na.rm=TRUE),
    
    auc_less_00        = sum(abs(press_less_00)   , na.rm=TRUE),
    time_less_00       = sum(press_less_00/press_less_00,na.rm=TRUE)
    )

# join summary values to original

test_joined <- test %>% 
  left_join(test_summ)

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
  
  geom_point(aes(y=pressure * sec_y_scale + sec_y_shift),alpha=.1,size=.1)+
  # geom_point(aes(y=pressure * sec_y_scale + sec_y_shift,color=dot_colors),alpha=.1,size=.1)+
  geom_hline(aes(yintercept = .0 * sec_y_scale + sec_y_shift),color='red')+
  geom_hline(aes(yintercept = .01 * sec_y_scale + sec_y_shift),color='red',linetype='dashed')+
  geom_hline(aes(yintercept = .03 * sec_y_scale + sec_y_shift),color='red',linetype='dashed')+
  
  # points
  # geom_point(aes(y=press_betw_01_03 * sec_y_scale + sec_y_shift),alpha=1,size=.1,color='green')+
  # geom_point(aes(y=press_greater_03 * sec_y_scale + sec_y_shift),alpha=1,size=.1,color='red')+
  # geom_point(aes(y=press_betw_00_01 * sec_y_scale + sec_y_shift),alpha=1,size=.1,color='yellow2')+
  # geom_point(aes(y=press_less_00    * sec_y_scale + sec_y_shift),alpha=1,size=.1,color='red')+

  # ribbons
  geom_ribbon(aes(ymin = press_less_00 * sec_y_scale + sec_y_shift, ymax = 0 * sec_y_scale + sec_y_shift),fill='red',alpha=1)+
  geom_ribbon(aes(ymin = 0.00 * sec_y_scale + sec_y_shift, ymax = press_betw_00_01 * sec_y_scale + sec_y_shift),fill='yellow3',alpha=1)+
  geom_ribbon(aes(ymin = 0.01 * sec_y_scale + sec_y_shift, ymax = press_betw_01_03 * sec_y_scale + sec_y_shift),fill='green',alpha=1)+
  geom_ribbon(aes(ymin = 0.03 * sec_y_scale + sec_y_shift, ymax = press_greater_03 * sec_y_scale + sec_y_shift),fill='red',alpha=1)+
  # scale_x_continuous(limits = c(0,18.4*60))+
  # scale_y_continuous(limits = c(1250,2350))
  
  
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
                     breaks = sort(c(seq(0,100*60,12*60),unique(test$index_1200F)))
                     )+
  labs(title = g.title)+
  
  geom_label(data = test_joined %>% distinct(auc_less_00,time_less_00,index_1200F),
            aes(x=60*50,y=-.005*sec_y_scale+sec_y_shift,label=paste0(round(auc_less_00,2), "auc, ", time_less_00, "m"))
            )+
  geom_label(data = test_joined %>% distinct(auc_betw_00_01, time_betw_00_01,index_1200F),
            aes(x=60*50,y=0.005*sec_y_scale+sec_y_shift,label=paste0(round(auc_betw_00_01,2),"auc, ", time_betw_00_01, "m"))
            )+
  geom_label(data = test_joined %>% distinct(auc_betw_01_03, time_betw_01_03,index_1200F),
            aes(x=60*50,y=.02*sec_y_scale+sec_y_shift,label=paste0(round(auc_betw_01_03,2), "auc, ", time_betw_01_03, "m"))
            )+
  geom_label(data = test_joined %>% distinct(auc_greater_03, time_greater_03,index_1200F),
            aes(x=60*50,y=0.04*sec_y_scale+sec_y_shift,label=paste0(round(auc_greater_03,2), "auc, ", time_greater_03, "m"))
            )+
  geom_label(data = test_joined %>% distinct(auc_pos_pres_1200, time_at_pos,index_1200F),
            aes(x=60*30,y=0.005*sec_y_scale+sec_y_shift,label=paste0(round(auc_pos_pres_1200,2), "auc, ", time_at_pos, "m"))
            )+
  geom_label(data = test_joined %>% distinct(auc_neg_pres_1200, time_at_neg,index_1200F),
            aes(x=60*30,y=-0.005*sec_y_scale+sec_y_shift,label=paste0(round(auc_neg_pres_1200,2), "auc, ", time_at_neg, "m"))
            )


# apply mutate on groups --------------------------------------------------

t_lots <- sample(c(levels(kilns_H$LOTNO),levels(kilns_G$LOTNO)),4)
# t_lot <- sample(levels(kilns_H$LOTNO),1)
test <- kilns_All[kilns_All$LOTNO %in% t_lots,]

test <- test %>% 
  group_by(LOTNO) %>% 
  mutate(
    pressure = ifelse(pressure < -.07, -0.07,pressure),
    pressure = ifelse(pressure > .07, 0.07,pressure),
    
    # find time index where temp reaches 1200F
    close_1200  = if_else( (time < index_max_temp), (abs(1200 - avg_kiln_temp)), NULL ))

test <- left_join(test, test %>% 
                    group_by(LOTNO) %>% 
                    dplyr::summarise(index_1200F = which.min(close_1200))
                  )

test <- test %>% 
  group_by(LOTNO) %>% 
  mutate(

    press_greater_03 = if_else((pressure >  .03) & (time < index_1200F), pressure, NULL),
    press_betw_01_03 = if_else((pressure >= .01) & (pressure <= .03) & (time < index_1200F), pressure, NULL),
    press_betw_00_01 = if_else((pressure <  .01) & (pressure >= 0) & (time < index_1200F), pressure, NULL),
    press_less_00    = if_else((pressure <  .00) & (time < index_1200F), pressure, NULL),
    
    # is pressure pos/neg?
    press_auc_max_1200 = if_else((pressure >  0) & (time <= index_1200F), pressure, NULL),
    press_auc_min_1200 = if_else((pressure <= 0) & (time <= index_1200F), pressure, NULL)
    
  ) %>% 
  dplyr::select(-c(close_1200)) %>% 
  dplyr::select(c(LOTNO,index_max_temp,index_1200F,everything()))

test_summ <- test %>% 
      group_by(LOTNO) %>% 
      dplyr::summarise(
        auc_pos_pres_1200 = sum(press_auc_max_1200,na.rm = TRUE),
        time_at_pos = sum(press_auc_max_1200/press_auc_max_1200,na.rm=TRUE),
        
        auc_neg_pres_1200 = sum(abs(press_auc_min_1200),na.rm = TRUE),
        time_at_neg = sum(press_auc_min_1200/press_auc_min_1200,na.rm=TRUE),
        
        auc_betw_01_03     = sum(abs(press_betw_01_03-.01), na.rm=TRUE),
        time_betw_01_03    = sum(press_betw_01_03/press_betw_01_03,na.rm=TRUE),
        
        auc_greater_03     = sum(abs(press_greater_03-.03), na.rm=TRUE),
        time_greater_03    = sum(press_greater_03/press_greater_03,na.rm=TRUE),
        
        auc_betw_00_01     = sum(abs(press_betw_00_01), na.rm=TRUE),
        time_betw_00_01    = sum(press_betw_00_01/press_betw_00_01,na.rm=TRUE),
        
        auc_less_00        = sum(abs(press_less_00)   , na.rm=TRUE),
        time_less_00       = sum(press_less_00/press_less_00,na.rm=TRUE)
      )

test_joined <- left_join(test, test_summ)


test_joined %>% 
  ggplot(aes(x=time,y=avg_kiln_temp))+
  geom_line()+
  geom_line(aes(y=setpoint),color='grey50',linetype='dashed')+
  
  geom_point(aes(y=pressure * sec_y_scale + sec_y_shift),alpha=.1,size=.1)+
  # geom_point(aes(y=pressure * sec_y_scale + sec_y_shift,color=dot_colors),alpha=.1,size=.1)+
  geom_hline(aes(yintercept = .0 * sec_y_scale + sec_y_shift),color='red')+
  geom_hline(aes(yintercept = .01 * sec_y_scale + sec_y_shift),color='red',linetype='dashed')+
  geom_hline(aes(yintercept = .03 * sec_y_scale + sec_y_shift),color='red',linetype='dashed')+
  
  # points
  # geom_point(aes(y=press_betw_01_03 * sec_y_scale + sec_y_shift),alpha=1,size=.1,color='green')+
  # geom_point(aes(y=press_greater_03 * sec_y_scale + sec_y_shift),alpha=1,size=.1,color='red')+
  # geom_point(aes(y=press_betw_00_01 * sec_y_scale + sec_y_shift),alpha=1,size=.1,color='yellow2')+
  # geom_point(aes(y=press_less_00    * sec_y_scale + sec_y_shift),alpha=1,size=.1,color='red')+
  
  # ribbons
  geom_ribbon(aes(ymin = press_less_00 * sec_y_scale + sec_y_shift, ymax = 0 * sec_y_scale + sec_y_shift),fill='red',alpha=1)+
  geom_ribbon(aes(ymin = 0.00 * sec_y_scale + sec_y_shift, ymax = press_betw_00_01 * sec_y_scale + sec_y_shift),fill='yellow3',alpha=1)+
  geom_ribbon(aes(ymin = 0.01 * sec_y_scale + sec_y_shift, ymax = press_betw_01_03 * sec_y_scale + sec_y_shift),fill='green',alpha=1)+
  geom_ribbon(aes(ymin = 0.03 * sec_y_scale + sec_y_shift, ymax = press_greater_03 * sec_y_scale + sec_y_shift),fill='red',alpha=1)+
  # scale_x_continuous(limits = c(0,18.4*60))+
  # scale_y_continuous(limits = c(1250,2350))
  
  
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
  
  labs(title = g.title)+
  
  geom_label(data = test_joined %>% distinct(auc_less_00,time_less_00,index_1200F),
             aes(x=60*50,y=-.005*sec_y_scale+sec_y_shift,label=paste0(round(auc_less_00,2), "auc, ", time_less_00, "m"))
  )+
  geom_label(data = test_joined %>% distinct(auc_betw_00_01, time_betw_00_01,index_1200F),
             aes(x=60*50,y=0.005*sec_y_scale+sec_y_shift,label=paste0(round(auc_betw_00_01,2),"auc, ", time_betw_00_01, "m"))
  )+
  geom_label(data = test_joined %>% distinct(auc_betw_01_03, time_betw_01_03,index_1200F),
             aes(x=60*50,y=.02*sec_y_scale+sec_y_shift,label=paste0(round(auc_betw_01_03,2), "auc, ", time_betw_01_03, "m"))
  )+
  geom_label(data = test_joined %>% distinct(auc_greater_03, time_greater_03,index_1200F),
             aes(x=60*50,y=0.04*sec_y_scale+sec_y_shift,label=paste0(round(auc_greater_03,2), "auc, ", time_greater_03, "m"))
  )+
  geom_label(data = test_joined %>% distinct(auc_pos_pres_1200, time_at_pos,index_1200F),
             aes(x=60*30,y=0.005*sec_y_scale+sec_y_shift,label=paste0(round(auc_pos_pres_1200,2), "auc, ", time_at_pos, "m"))
  )+
  geom_label(data = test_joined %>% distinct(auc_neg_pres_1200, time_at_neg,index_1200F),
             aes(x=60*30,y=-0.005*sec_y_scale+sec_y_shift,label=paste0(round(auc_neg_pres_1200,2), "auc, ", time_at_neg, "m"))
  )+
  facet_wrap(~LOTNO)

