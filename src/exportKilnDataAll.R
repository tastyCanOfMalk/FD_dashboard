# concerning lotnos:
# 022818C
# 120519A
# 120319H
# 022818C
# 022818G
# 050118G

# load annual kiln data ---------------------------------------------------
source("src/getKilnData2018.R")
source("src/getKilnData2019.R")
source("src/getKilnData2020.R")

# bind annual kiln data ---------------------------------------------------
kilns_AB <- bind_rows(kilns_AB_2018, 
            bind_rows(kilns_AB_2019, 
                      kilns_AB_2020)) %>% 
  plyr::mutate(year = as.factor(year(date))) %>%
  mutate_if(is.character, as.factor) %>% 
  dplyr::select(date, year, kiln, LOTNO, time, avg_kiln_temp, setpoint, everything()) %>% 
  mutateAucValues()

kilns_C <- bind_rows(kilns_C_2018, 
           bind_rows(kilns_C_2019, 
                     kilns_C_2020)) %>%
  plyr::mutate(year = as.factor(year(date))) %>%
  mutate_if(is.character, as.factor) %>% 
  dplyr::select(date, year, kiln, LOTNO, time, avg_kiln_temp, setpoint, everything()) %>% 
  mutateAucValues()
  
kilns_D <- bind_rows(kilns_D_2018, 
           bind_rows(kilns_D_2019, 
                     kilns_D_2020)) %>%
  plyr::mutate(year = as.factor(year(date))) %>%
  mutate_if(is.character, as.factor) %>% 
  dplyr::select(date, year, kiln, LOTNO, time, avg_kiln_temp, setpoint, everything()) %>% 
  mutateAucValues()

kilns_E <- bind_rows(kilns_E_2019, 
                     kilns_E_2020) %>%
  plyr::mutate(year = as.factor(year(date))) %>%
  mutate_if(is.character, as.factor) %>% 
  dplyr::select(date, year, kiln, LOTNO, time, avg_kiln_temp, setpoint, everything()) %>% 
  mutateAucValues()

kilns_F <- bind_rows(kilns_F_2018, 
           bind_rows(kilns_F_2019, 
                     kilns_F_2020)) %>%
  plyr::mutate(year = as.factor(year(date))) %>%
  mutate_if(is.character, as.factor) %>% 
  dplyr::select(date, year, kiln, LOTNO, time, avg_kiln_temp, setpoint, everything()) %>% 
  mutateAucValues()

kilns_G <- bind_rows(kilns_G_2018, 
           bind_rows(kilns_G_2019, 
                     kilns_G_2020)) %>%
  plyr::mutate(year = as.factor(year(date))) %>%
  mutate_if(is.character, as.factor) %>% 
  dplyr::select(date, year, kiln, LOTNO, time, avg_kiln_temp, setpoint, everything()) %>% 
  mutateAucValues()
  
kilns_H <- bind_rows(kilns_H_2018, 
           bind_rows(kilns_H_2019, 
                     kilns_H_2020)) %>%
  plyr::mutate(year = as.factor(year(date))) %>%
  mutate_if(is.character, as.factor) %>% 
  dplyr::select(date, year, kiln, LOTNO, time, avg_kiln_temp, setpoint, everything()) %>% 
  mutateAucValues()

# export all separated kiln data ------------------------------------------
write_csv(kilns_AB,"data/processed_data/kilns_AB.csv")
write_csv(kilns_C, "data/processed_data/kilns_C.csv")
write_csv(kilns_D, "data/processed_data/kilns_D.csv")
write_csv(kilns_E, "data/processed_data/kilns_E.csv")
write_csv(kilns_F, "data/processed_data/kilns_F.csv")
write_csv(kilns_G, "data/processed_data/kilns_G.csv")
write_csv(kilns_H, "data/processed_data/kilns_H.csv")

# view plots --------------------------------------------------------------
# kilns_AB <- read_csv("data/processed_data/kilns_AB.csv") %>% dplyr::mutate_if(is.character,as.factor)
# kilns_C  <- read_csv("data/processed_data/kilns_C.csv") %>% dplyr::mutate_if(is.character,as.factor)
# kilns_D  <- read_csv("data/processed_data/kilns_D.csv") %>% dplyr::mutate_if(is.character,as.factor)
# kilns_E  <- read_csv("data/processed_data/kilns_E.csv") %>% dplyr::mutate_if(is.character,as.factor)
# kilns_F  <- read_csv("data/processed_data/kilns_F.csv") %>% dplyr::mutate_if(is.character,as.factor)
# kilns_G  <- read_csv("data/processed_data/kilns_G.csv") %>% dplyr::mutate_if(is.character,as.factor)
# kilns_H  <- read_csv("data/processed_data/kilns_H.csv") %>% dplyr::mutate_if(is.character,as.factor)

# length(levels(kilns_AB$LOTNO)) # 277
# plotAucValues(df = kilns_AB, 1, 56,    crop=T, free.x=F)
# plotAucValues(df = kilns_AB, 57, 112,  crop=T, free.x=F)
# plotAucValues(df = kilns_AB, 113, 168, crop=T, free.x=F)
# plotAucValues(df = kilns_AB, 169, 224, crop=T, free.x=F)
# plotAucValues(df = kilns_AB, 225, 280, crop=T, free.x=F)
# 
# # length(levels(kilns_C$LOTNO))  # 56
# plotAucValues(df = kilns_C, 1, 56,    crop=T, free.x=F)
# 
# # length(levels(kilns_D$LOTNO))  # 92
# plotAucValues(df = kilns_D, 1, 56,    crop=T, free.x=F)
# plotAucValues(df = kilns_D, 57, 112,  crop=T, free.x=F)
# 
# # length(levels(kilns_E$LOTNO))  # 17
# plotAucValues(df = kilns_E, 1, 56,    crop=T, free.x=F)
# 
# # length(levels(kilns_F$LOTNO))  # 122
# plotAucValues(df = kilns_F, 1, 56,    crop=T, free.x=F)
# plotAucValues(df = kilns_F, 57, 112,  crop=T, free.x=F)
# plotAucValues(df = kilns_F, 113, 168, crop=T, free.x=F)
# 
# # length(levels(kilns_G$LOTNO))  # 240
# plotAucValues(df = kilns_G, 1, 56,    crop=T, free.x=F)
# plotAucValues(df = kilns_G, 57, 112,  crop=T, free.x=F)
# plotAucValues(df = kilns_G, 113, 168, crop=T, free.x=F)
# plotAucValues(df = kilns_G, 169, 224, crop=T, free.x=F)
# plotAucValues(df = kilns_G, 225, 280, crop=T, free.x=F)
# 
# # length(levels(kilns_H$LOTNO))  # 192
# plotAucValues(df = kilns_H, 1, 56,    crop=T, free.x=F)
# plotAucValues(df = kilns_H, 57, 112,  crop=T, free.x=F)
# plotAucValues(df = kilns_H, 113, 168, crop=T, free.x=F)
# plotAucValues(df = kilns_H, 169, 224, crop=T, free.x=F)


# bind summary stats to df ------------------------------------------------
allAucValues <- bind_rows(
  kilns_AB %>% summariseAucValues(),
  kilns_C %>%  summariseAucValues(),
  kilns_D %>%  summariseAucValues(),
  kilns_E %>%  summariseAucValues(),
  kilns_F %>%  summariseAucValues(),
  kilns_G %>%  summariseAucValues(),
  kilns_H %>%  summariseAucValues()
) %>% 
  mutate(LOTNO = as.factor(LOTNO))

write_csv(allAucValues, "data/processed_data/kilnsAucValues.csv")

# # import processed kiln data and view AUC -----------------------------------------------------
# kilns_AB <- read_csv("data/processed_data/kilns_AB.csv") %>%
#   mutate(year = as.factor(year)) %>%
#   mutate_if(is.character, as.factor)

# kilns_C  <- read_csv("data/processed_data/kilns_C.csv") %>% 
#   mutate(year = as.factor(year)) %>% 
#   mutate_if(is.character, as.factor)

# kilns_D  <- read_csv("data/processed_data/kilns_D.csv") %>% 
#   mutate(year = as.factor(year)) %>% 
#   mutate_if(is.character, as.factor)

# kilns_E  <- read_csv("data/processed_data/kilns_E.csv") %>% 
#   mutate(year = as.factor(year)) %>% 
#   mutate_if(is.character, as.factor)

# kilns_F  <- read_csv("data/processed_data/kilns_F.csv") %>% 
#   mutate(year = as.factor(year)) %>% 
#   mutate_if(is.character, as.factor)

# kilns_G  <- read_csv("data/processed_data/kilns_G.csv") %>% 
#   mutate(year = as.factor(year)) %>% 
#   mutate_if(is.character, as.factor)

# kilns_H  <- read_csv("data/processed_data/kilns_H.csv") %>% 
#   mutate(year = as.factor(year)) %>% 
#   mutate_if(is.character, as.factor)

# view plots --------------------------------------------------------------
# library(plotly)
# length(levels(kilns_AB$LOTNO)) # 277
# plotAucValues(df = kilns_AB, 1, 56,    crop=T, free.x=F)
# plotAucValues(df = kilns_AB, 57, 112,  crop=T, free.x=F)
# plotAucValues(df = kilns_AB, 113, 168, crop=T, free.x=F)
# plotAucValues(df = kilns_AB, 169, 224, crop=T, free.x=F)
# plotAucValues(df = kilns_AB, 225, 280, crop=T, free.x=F)
#
# # length(levels(kilns_C$LOTNO))  # 56
# plotAucValues(df = kilns_C, 1, 56,    crop=T, free.x=F)
#
# # length(levels(kilns_D$LOTNO))  # 92
# plotAucValues(df = kilns_D, 1, 56,    crop=T, free.x=F)
# plotAucValues(df = kilns_D, 57, 112,  crop=T, free.x=F)
#
# # length(levels(kilns_E$LOTNO))  # 17
# plotAucValues(df = kilns_E, 1, 56,    crop=T, free.x=F)
#
# # length(levels(kilns_F$LOTNO))  # 122
# plotAucValues(df = kilns_F, 1, 56,    crop=T, free.x=F)
# plotAucValues(df = kilns_F, 57, 112,  crop=T, free.x=F)
# plotAucValues(df = kilns_F, 113, 168, crop=T, free.x=F)
#
# # length(levels(kilns_G$LOTNO))  # 240
# plotAucValues(df = kilns_G, 1, 56,    crop=T, free.x=F)
# plotAucValues(df = kilns_G, 57, 112,  crop=T, free.x=F)
# plotAucValues(df = kilns_G, 113, 168, crop=T, free.x=F)
# plotAucValues(df = kilns_G, 169, 224, crop=T, free.x=F)
# plotAucValues(df = kilns_G, 225, 280, crop=T, free.x=F)
#
# # length(levels(kilns_H$LOTNO))  # 192
# plotAucValues(df = kilns_H, 1, 56,    crop=T, free.x=F)
# plotAucValues(df = kilns_H, 57, 112,  crop=T, free.x=F)
# plotAucValues(df = kilns_H, 113, 168, crop=T, free.x=F)
# plotAucValues(df = kilns_H, 169, 224, crop=T, free.x=F)



