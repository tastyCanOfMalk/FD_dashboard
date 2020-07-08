library(tidyverse)

source("src/kilnFunctions.R")
source("src/userFunctions.R")

# import processed kiln data -----------------------------------------------------
kilns_AB <- read_csv("data/kiln/export/kilns_AB.csv") %>% 
  mutate(year = as.factor(year)) %>% 
  mutate_if(is.character, as.factor)%>% 
  mutateAucValues()
kilns_C  <- read_csv("data/kiln/export/kilns_C.csv") %>% 
  mutate(year = as.factor(year)) %>% 
  mutate_if(is.character, as.factor)%>% 
  mutateAucValues()
kilns_D  <- read_csv("data/kiln/export/kilns_D.csv") %>% 
  mutate(year = as.factor(year)) %>% 
  mutate_if(is.character, as.factor)%>% 
  mutateAucValues()
kilns_E  <- read_csv("data/kiln/export/kilns_E.csv") %>% 
  mutate(year = as.factor(year)) %>% 
  mutate_if(is.character, as.factor)%>% 
  mutateAucValues()
kilns_F  <- read_csv("data/kiln/export/kilns_F.csv") %>% 
  mutate(year = as.factor(year)) %>% 
  mutate_if(is.character, as.factor)%>% 
  mutateAucValues()
kilns_G  <- read_csv("data/kiln/export/kilns_G.csv") %>% 
  mutate(year = as.factor(year)) %>% 
  mutate_if(is.character, as.factor)%>% 
  mutateAucValues()
kilns_H  <- read_csv("data/kiln/export/kilns_H.csv") %>% 
  mutate(year = as.factor(year)) %>% 
  mutate_if(is.character, as.factor) %>% 
  mutateAucValues()


test <- kilns_H %>% 
  summariseAucValues() %>% 
  right_join(kilns_H, by="LOTNO")

test %>% 
  # dplyr::filter(LOTNO %in% levels(kilns_H$LOTNO)[c(1:25)]) %>% 
  dplyr::filter(LOTNO %in% levels(kilns_H$LOTNO)[c(26:60)]) %>% 
  plotAucValues()

test2 <- test %>% 
  dplyr::filter(LOTNO %in% levels(test$LOTNO)[c(1:6)])
test2 %>% 
  plotAucValues()

test2  %>%  
  dplyr::filter((time < index_max_temp) & (avg_kiln_temp >= bot_temp_range) & (avg_kiln_temp <= top_temp_range)) %>%
  ggplot(aes(x=time, y=avg_kiln_temp),color='black',size=.8)+
  geom_line()+
  geom_line(aes(y=setpoint),color='red')+
  geom_ribbon(aes(ymin = auc_min, ymax = auc_max), fill='green', alpha=.3)+
  geom_text(aes(x = 400, y = 400, label = paste0("AUC = ", mynumber(auc))), color = "red", data = test2 %>% distinct(LOTNO, auc)) +
  # geom_text(aes(x = 2000, y = 1000, label = paste0("AUC = ", mynumber(auc))), color = "red", data = test2 %>% distinct(LOTNO, auc)) +
  # geom_text(aes(x = 400, y = 400, label = auc)) +
  # facet_wrap(~LOTNO)+
  # scale_y_continuous(breaks = seq(400, 600, 200))+
  theme_minimal()+
  facet_wrap(~LOTNO)
theme(axis.text.x=element_blank(),
      axis.ticks.x=element_blank())+
  

  
  
  
  




kilns_H %>% 
  dplyr::filter(LOTNO %in% levels(kilns_H$LOTNO)[c(1:25)]) %>% 
  plotAucValues()

levels(kilns_H$LOTNO)

kilns_H %>% 
  plotAucValues()

# omit weird lots?
# discard <- c("022818C","120519A","120319H","022818C","022818G","050118G")

# rm(list=setdiff(ls(), c("kilns_AB",
#                         "kilns_C",
#                         "kilns_D",
#                         "kilns_E",
#                         "kilns_F",
#                         "kilns_G",
#                         "kilns_H",
#                         "df_merged",
#                         "df_yields",
#                         "df_defects"
#                         
# )))

# temperature vs setpoint deviation from 400 - 600C
# pressure must stay positive in the before 1000 minutes range
# i.e. how much time is spent in positive pressure in this range? (between 0 and 1000 minutes)


# try to highlight time ranges from 400 - 600
kilns_H %>%
  dplyr::filter(LOTNO %in% levels(kilns_H$LOTNO)[c(1:5)]) %>%
  group_by(LOTNO) %>%
  ggplot(aes(x=time, y=avg_kiln_temp))+
  geom_point(size=.8)+
  geom_line(aes(y= setpoint),color='red')+
  facet_wrap(~LOTNO)

bot_temp_range <- 400
top_temp_range <- 600

kilns_H %>%
  dplyr::filter(LOTNO %in% levels(kilns_H$LOTNO)[c(1:5)]) %>%
# kilns_G %>% 
  # dplyr::filter(LOTNO %in% levels(kilns_G$LOTNO)[c(1:2)]) %>%
  # dplyr::filter(LOTNO %in% levels(kilns_H$LOTNO)[c(1:56)]) %>%
  # dplyr::filter(LOTNO %in% levels(kilns_H$LOTNO)[c(57:113)]) %>%
  group_by(LOTNO) %>% 
  mutate(index_max_temp = median( which( max(avg_kiln_temp,na.rm=TRUE) == avg_kiln_temp ) )) %>%
  dplyr::filter((time < index_max_temp) & (avg_kiln_temp >= bot_temp_range) & (avg_kiln_temp <= top_temp_range)) %>%
  plyr::mutate(min = pmin(avg_kiln_temp, setpoint),
                max = pmax(avg_kiln_temp, setpoint)) %>%
  # plyr::mutate( 
  #   test1 = ifelse(
  #     ( (time < index_max_temp) & (avg_kiln_temp >= bot_temp_range) & (avg_kiln_temp <= top_temp_range) ),
  #     avg_kiln_temp, NA),
  #   test2 = ifelse(
  #     ( (time < index_max_temp) & (avg_kiln_temp >= bot_temp_range) & (avg_kiln_temp <= top_temp_range) ),
  #     setpoint, NA)) %>% 
  # dplyr::mutate(min = pmin(test1, test2),
  #               max = pmax(test1, test2)) %>% 
  ggplot(aes(x=time, y=avg_kiln_temp),color='black',size=.8)+
  geom_line()+
  geom_line(aes(y=setpoint),color='red')+
  geom_ribbon(aes(ymin = min, ymax = max), fill='green', alpha=.3)+
  facet_wrap(~LOTNO)
  
# kilns_H %>% 
#   dplyr::filter(LOTNO %in% levels(kilns_H$LOTNO)[c(1:5)]) %>%
kilns_G %>% 
  dplyr::filter(LOTNO %in% levels(kilns_G$LOTNO)[c(1:5)]) %>%
  # dplyr::filter(LOTNO %in% levels(kilns_H$LOTNO)[c(1:56)]) %>%
  # dplyr::filter(LOTNO %in% levels(kilns_H$LOTNO)[c(57:113)]) %>%
  group_by(LOTNO) %>% 
  mutate(index_max_temp = median( which( max(avg_kiln_temp,na.rm=TRUE) == avg_kiln_temp ) )) %>% 
  mutate(
    test1 = ifelse(
      ( (time < index_max_temp) & (avg_kiln_temp >= bot_temp_range) & (avg_kiln_temp <= top_temp_range) ),
      avg_kiln_temp, NA),
    test2 = ifelse(
      ( (time < index_max_temp) & (avg_kiln_temp >= bot_temp_range) & (avg_kiln_temp <= top_temp_range) ),
      setpoint, NA),
    min = pmin(test1, test2),
    max = pmax(test1, test2)
    ) %>% 
  dplyr::summarise(aucMin = sum(min, na.rm=TRUE),
                   aucMax = sum(max, na.rm=TRUE),
                   diff = abs(aucMin - aucMax)
                   ) 
  ggplot(aes(x=time, y=avg_kiln_temp),color='black',size=.8)+
  geom_line()+
  geom_line(aes(y=setpoint),color='red')+
  geom_ribbon(aes(ymin = min, ymax = max), fill='green', alpha=.3)+
  facet_wrap(~LOTNO)
  
  



022020H
050118H
051918H
091419H
120319H