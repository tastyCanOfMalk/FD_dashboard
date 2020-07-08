library(tidyverse)

# import processed kiln data -----------------------------------------------------
kilns_AB <- read_csv("data/kiln/export/kilns_AB.csv") %>% 
  mutate(year = as.factor(year)) %>% 
  mutate_if(is.character, as.factor)
kilns_C  <- read_csv("data/kiln/export/kilns_C.csv") %>% 
  mutate(year = as.factor(year)) %>% 
  mutate_if(is.character, as.factor)
kilns_D  <- read_csv("data/kiln/export/kilns_D.csv") %>% 
  mutate(year = as.factor(year)) %>% 
  mutate_if(is.character, as.factor)
kilns_E  <- read_csv("data/kiln/export/kilns_E.csv") %>% 
  mutate(year = as.factor(year)) %>% 
  mutate_if(is.character, as.factor)
kilns_F  <- read_csv("data/kiln/export/kilns_F.csv") %>% 
  mutate(year = as.factor(year)) %>% 
  mutate_if(is.character, as.factor)
kilns_G  <- read_csv("data/kiln/export/kilns_G.csv") %>% 
  mutate(year = as.factor(year)) %>% 
  mutate_if(is.character, as.factor)
kilns_H  <- read_csv("data/kiln/export/kilns_H.csv") %>% 
  mutate(year = as.factor(year)) %>% 
  mutate_if(is.character, as.factor)

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
# kilns_H %>% 
#   dplyr::filter(LOTNO %in% levels(kilns_H$LOTNO)[c(1:5)]) %>% 
#   group_by(LOTNO) %>% 
#   ggplot(aes(x=time, y=avg_kiln_temp))+
#   geom_point(size=.8)+
#   geom_line(aes(y= setpoint),color='red')+
#   facet_wrap(~LOTNO)

bot_temp_range <- 400
top_temp_range <- 600

a <- kilns_H %>% 
    dplyr::filter(LOTNO %in% levels(kilns_H$LOTNO)[c(1)])
a %>% 
  group_by(LOTNO) %>% 
  mutate(index_max_temp = median( which( max(avg_kiln_temp,na.rm=TRUE) == avg_kiln_temp ) )) %>% 
  # dplyr::filter((time < index_max_temp) & (avg_kiln_temp >= bot_temp_range) & (avg_kiln_temp <= top_temp_range)) %>%
  dplyr::mutate( test = ifelse((time < index_max_temp) & (avg_kiln_temp >= bot_temp_range) & (avg_kiln_temp <= top_temp_range)),
                 ) %>%
  dplyr::mutate(min = pmin(avg_kiln_temp, setpoint),
                max = pmax(avg_kiln_temp, setpoint)) %>% 
  ggplot(aes(x=time, y=avg_kiln_temp),color='black',size=.8)+
  geom_line()+
  geom_line(aes(y=setpoint),color='red')+
  geom_ribbon(aes(ymin = min, ymax = max), fill='green', alpha=.3)+
  facet_wrap(~LOTNO)
  
  


  
  index_max <- function(x){
  return(tibble(max = which(max(x$avg_kiln_temp, na.rm=TRUE) == x$avg_kiln_temp)))
  }



median( which( max(a$avg_kiln_temp,na.rm=TRUE) == a$avg_kiln_temp ) )

  plyr::mutate(index_max_temp = mapply(function() index_max_temp()))

  index_max_temp()

kilns_H %>% 
  dplyr::filter(LOTNO %in% levels(kilns_H$LOTNO)[c(1)]) %>% 
  group_by(LOTNO) %>% 
  ggplot(aes(x=time, y=avg_kiln_temp))+
  geom_point(size=.8)+
  geom_line(aes(y= setpoint),color='red')+
  geom_hline(aes(yintercept = 400))+
  geom_hline(aes(yintercept = 600))+
  facet_wrap(~LOTNO)







kilns_H %>% 
  dplyr::filter(LOTNO %in% levels(kilns_H$LOTNO)[c(1:25)]) %>% 
  group_by(LOTNO) %>% 
  ggplot(aes(x=time, y=avg_kiln_temp)) +
  # geom_point(aes(), color='red', size=.8)+
  geom_line(aes(y=setpoint), color='black')+
  geom_line(aes(), color='red')+
  # scale_y_continuous(limits = c(400,600))+
  # scale_x_continuous(limits = c(100,1200))+
  facet_wrap(~LOTNO)

kilns_H %>% 
  dplyr::filter(LOTNO %in% levels(kilns_H$LOTNO)[c(1:5)]) %>% 
  group_by(LOTNO) %>% 
  ggplot(aes(x=time, y=pressure)) +
  geom_line(aes(), color='red',alpha=.3)+
  geom_line(aes(y=press_wsp), color='black')+
  # scale_y_continuous(limits = c(400,600))+
  # scale_x_continuous(limits = c(0, 1000))+
  facet_wrap(~LOTNO)


kilns_H %>% 
  dplyr::filter(LOTNO %in% levels(kilns_H$LOTNO)[c(150:175)]) %>% 
  group_by(LOTNO) %>% 
  ggplot(aes(x=time)) +
  geom_line(aes(y=avg_kiln_temp), color='red')+
  geom_line(aes(y=setpoint), color='black')+
  geom_line(aes(y=pressure *10000), color='red',alpha=.3)+
  geom_line(aes(y=press_wsp*10000), color='black')+
  # scale_y_continuous(limits = c(400,600))+
  # scale_x_continuous(limits = c(0, 1000))+
  facet_wrap(~LOTNO)

022020H
050118H
051918H
091419H
120319H