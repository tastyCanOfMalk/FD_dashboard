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
  mutate_if(is.character, as.factor) %>% 
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

# view plots --------------------------------------------------------------
# # length(levels(kilns_AB$LOTNO)) # 277
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
  kilns_C %>% summariseAucValues(),
  kilns_D %>% summariseAucValues(),
  kilns_E %>% summariseAucValues(),
  kilns_F %>% summariseAucValues(),
  kilns_G %>% summariseAucValues(),
  kilns_H %>% summariseAucValues()
  )

df_merged <- df_merged %>% 
  left_join(allAucValues, by='LOTNO') %>% 
  mutate(LOTNO = as.factor(LOTNO))
