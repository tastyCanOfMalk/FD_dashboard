# concerning lotnos:
# 022818C
# 120519A
# 120319H
# 022818C
# 022818G
# 050118G

source("src/getKilnData2018.R")
source("src/getKilnData2019.R")
source("src/getKilnData2020.R")

kilns_AB <- bind_rows(kilns_AB_2018, bind_rows(kilns_AB_2019, kilns_AB_2020)) %>% 
  plyr::mutate(year = as.factor(year(date))) %>%
  mutate_if(is.character, as.factor) %>% 
  dplyr::select(date, year, kiln, LOTNO, time, avg_kiln_temp, setpoint, everything()) 

kilns_C <- bind_rows(kilns_C_2018, 
           bind_rows(kilns_C_2019, 
                     kilns_C_2020)) %>%
  plyr::mutate(year = as.factor(year(date))) %>%
  mutate_if(is.character, as.factor) %>% 
  dplyr::select(date, year, kiln, LOTNO, time, avg_kiln_temp, setpoint, everything())
  
kilns_D <- bind_rows(kilns_D_2018, 
           bind_rows(kilns_D_2019, 
                     kilns_D_2020)) %>%
  plyr::mutate(year = as.factor(year(date))) %>%
  mutate_if(is.character, as.factor) %>% 
  dplyr::select(date, year, kiln, LOTNO, time, avg_kiln_temp, setpoint, everything())

kilns_E <- bind_rows(kilns_E_2019, 
                     kilns_E_2020) %>%
  plyr::mutate(year = as.factor(year(date))) %>%
  mutate_if(is.character, as.factor) %>% 
  dplyr::select(date, year, kiln, LOTNO, time, avg_kiln_temp, setpoint, everything())

kilns_F <- bind_rows(kilns_F_2018, 
           bind_rows(kilns_F_2019, 
                     kilns_F_2020)) %>%
  plyr::mutate(year = as.factor(year(date))) %>%
  mutate_if(is.character, as.factor) %>% 
  dplyr::select(date, year, kiln, LOTNO, time, avg_kiln_temp, setpoint, everything())

kilns_G <- bind_rows(kilns_G_2018, 
           bind_rows(kilns_G_2019, 
                     kilns_G_2020)) %>%
  plyr::mutate(year = as.factor(year(date))) %>%
  mutate_if(is.character, as.factor) %>% 
  dplyr::select(date, year, kiln, LOTNO, time, avg_kiln_temp, setpoint, everything())
  
kilns_H <- bind_rows(kilns_H_2018, 
           bind_rows(kilns_H_2019, 
                     kilns_H_2020)) %>%
  plyr::mutate(year = as.factor(year(date))) %>%
  mutate_if(is.character, as.factor) %>% 
  dplyr::select(date, year, kiln, LOTNO, time, avg_kiln_temp, setpoint, everything())

# rm(list=setdiff(ls(), c("kilns_AB",
#                         "kilns_C",
#                         "kilns_D",
#                         "kilns_E",
#                         "kilns_F",
#                         "kilns_G",
#                         "kilns_H"
# )))

write_csv(kilns_AB,"data/kiln/export/kilns_AB.csv")
write_csv(kilns_C, "data/kiln/export/kilns_C.csv")
write_csv(kilns_D, "data/kiln/export/kilns_D.csv")
write_csv(kilns_D, "data/kiln/export/kilns_E.csv")
write_csv(kilns_F, "data/kiln/export/kilns_F.csv")
write_csv(kilns_G, "data/kiln/export/kilns_G.csv")
write_csv(kilns_H, "data/kiln/export/kilns_H.csv")
