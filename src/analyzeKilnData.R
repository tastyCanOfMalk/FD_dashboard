# recollect kiln data -----------------------------------------------------

kilns_AB <- read_csv("data/kiln/export/kilns_AB.csv") %>% 
  mutate(year = as.factor(year)) %>% 
  mutate_if(is.character, as.factor)
kilns_C  <- read_csv("data/kiln/export/kilns_C.csv") %>% 
  mutate(year = as.factor(year)) %>% 
  mutate_if(is.character, as.factor)
kilns_D  <- read_csv("data/kiln/export/kilns_D.csv") %>% 
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




