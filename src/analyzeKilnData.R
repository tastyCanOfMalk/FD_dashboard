library(tidyverse)

source("src/kilnFunctions.R")
source("src/userFunctions.R")

rm(list = ls())

# load stuff
# extrafont::loadfonts(device="win")
# library(hrbrthemes)
library(ggplot2)
# library(extrafont)
# loadfonts(device = "postscript")
library(tidyverse)
library(plotly)
# library(silgelib)

# theme_set(theme_minimal(base_family = "IBM Plex Sans"))
theme_set(theme_minimal())
# theme_set(theme_plex())

source("src/kilnFunctions.R")
source("src/userFunctions.R")

# import data -------------------------------------------------------------
df_merged    <- read_csv("data/processed_data/df_merged.csv")
df_yields    <- read_csv("data/processed_data/df_yields.csv")

df_defects   <- read_csv("data/processed_data/df_defects.csv")
kilns_AB     <- read_csv("data/processed_data/kilns_AB.csv") %>% mutate_if(is.character,as.factor)
kilns_C      <- read_csv("data/processed_data/kilns_C.csv")  %>% mutate_if(is.character,as.factor)
kilns_D      <- read_csv("data/processed_data/kilns_D.csv")  %>% mutate_if(is.character,as.factor)
kilns_E      <- read_csv("data/processed_data/kilns_E.csv")  %>% mutate_if(is.character,as.factor)
kilns_F      <- read_csv("data/processed_data/kilns_F.csv")  %>% mutate_if(is.character,as.factor)
kilns_G      <- read_csv("data/processed_data/kilns_G.csv")  %>% mutate_if(is.character,as.factor)
kilns_H      <- read_csv("data/processed_data/kilns_H.csv")  %>% mutate_if(is.character,as.factor)

allAucValues <- read_csv("data/processed_data/kilnsAucValues.csv")

# join kiln AUC calcs to original data ------------------------------------------
df_merged_auc <- df_merged %>% 
  left_join(allAucValues, by='LOTNO') %>% 
  dplyr::mutate_if(is.character, as.factor) %>% 
  mutate(LOTNO = as.factor(LOTNO),
         ITEM = as.factor(ITEM)) %>%
  na.omit(aucDiff)

df_yields_auc <- df_yields %>% 
  left_join(allAucValues, by='LOTNO') %>% 
  mutate_if(is.character, as.factor) %>% 
  mutate(LOTNO = as.factor(LOTNO),
         ITEM = as.factor(ITEM)) %>% 
  mutate(LOTNO = as.factor(LOTNO))%>% 
  na.omit(aucDiff)

df_defects_auc <- df_defects %>% 
  left_join(allAucValues, by='LOTNO') %>% 
  mutate_if(is.character, as.factor) %>% 
  mutate(LOTNO = as.factor(LOTNO),
         ITEM = as.factor(ITEM)) %>% 
  mutate(LOTNO = as.factor(LOTNO))%>% 
  na.omit(aucDiff)

# join all kiln AUC -------------------------------------------------------
all_kilns <- bind_rows(
  kilns_AB %>% dplyr::select(time, setpoint, avg_kiln_temp, LOTNO, auc_min, auc_max),
  kilns_C %>%  dplyr::select(time, setpoint, avg_kiln_temp, LOTNO, auc_min, auc_max),
  kilns_D %>%  dplyr::select(time, setpoint, avg_kiln_temp, LOTNO, auc_min, auc_max),
  kilns_E %>%  dplyr::select(time, setpoint, avg_kiln_temp, LOTNO, auc_min, auc_max),
  kilns_F %>%  dplyr::select(time, setpoint, avg_kiln_temp, LOTNO, auc_min, auc_max),
  kilns_G %>%  dplyr::select(time, setpoint, avg_kiln_temp, LOTNO, auc_min, auc_max),
  kilns_H %>%  dplyr::select(time, setpoint, avg_kiln_temp, LOTNO, auc_min, auc_max)
)

# random sample of LOTNOs
set.seed(5)
n_kilns <- sample_n(all_kilns, 12) %>% dplyr::select(LOTNO) %>% unlist()

sample_kilns <- all_kilns %>% 
  dplyr::filter(LOTNO %in% n_kilns) %>% 
  mutate(LOTNO = as.character(LOTNO)) %>% 
  mutate(LOTNO = factor(LOTNO)) %>% 
  mutateAucValues()

# plot random LOTNOs ------------------------------------------------------
plot_range   (sample_kilns)
plotAucValues(sample_kilns)
plotAucValues(sample_kilns, crop=T)
plotAucValues(sample_kilns, crop=T, free.x=T)

# check distribution of AUC -----------------------------------------------
df_merged_auc %>% 
  group_by(LOTNO) %>% slice(1) %>% 
  mutate(KILN2 = str_replace(KILN, "R", "")) %>% 
  # ggplot(aes(x=aucDiff, y=fct_reorder(KILN,aucDiff), fill =KILN2))+
  ggplot(aes(x=aucDiff, y=fct_reorder(KILN2,aucDiff)))+
  geom_boxplot(outlier.alpha = 0,
               outlier.shape = 21)+
  geom_jitter(height = .2, alpha=.1)+
  labs(
    title = "Distribution of AUC values"
  )+
  xlab("Area between curves")+
  ylab("Kiln")+
  theme(legend.position = 'none')+
  scale_x_continuous(labels = scales::label_number())


# does AUC value impact overall lot yields? -------------------------------
df_yields_auc %>% 
  mutate(KILN2 = str_replace(KILN, "R", "")) %>% 
  ggplot(aes(x=aucDiff))+
  geom_density()+
  facet_wrap(~KILN2, scales='free')

library(ggpubr)  
ggqqplot(df_yields_auc[df_yields_auc$KILN == "H",]$aucDiff)
ggqqplot(df_yields_auc[df_yields_auc$KILN == "G",]$aucDiff)
ggqqplot(df_yields_auc[df_yields_auc$KILN == "C",]$aucDiff)
ggqqplot(df_yields_auc[df_yields_auc$KILN == "D",]$aucDiff)
ggqqplot(df_yields_auc[df_yields_auc$KILN == "E",]$aucDiff)


  
df_yields_auc %>% 
  group_by(LOTNO, KILN, aucDiff, temp_avg, precip, snow_fall) %>% 
  dplyr::summarise(
    total_fired = sum(TOTAL_ITEM_FIRED),
    total_rejected = sum(TOTAL_ITEM_REJECTED),
    pct_lot_yield = (total_fired - total_rejected) / total_fired
    ) %>% 
  mutate(KILN2 = str_replace(KILN, "R", "")) %>% 
  ggplot(aes(x=pct_lot_yield, y=aucDiff))+
  # ggplot(aes(x=aucDiff, y=pct_lot_yield))+
  geom_point(alpha=.2)+
  facet_wrap(~KILN2, scales='free_x')












# import data -------------------------------------------------------------
df_merged <- read_csv("data/processed_data/df_merged.csv")

df_yields <- read_csv("data/processed_data/df_yields.csv")

df_defects <- read_csv("data/processed_data/df_defects.csv")

allAucValues <- read_csv("data/processed_data/kilnsAucValues.csv")

# join kiln AUC to original data ------------------------------------------
# ONLY > 2018 (kiln data 2018-2020)

df_merged_auc <- df_merged %>% 
  dplyr::filter(year(FIRE_DATE) >= 2018) %>% 
  left_join(allAucValues, by='LOTNO') %>% 
  dplyr::mutate_if(is.character, as.factor) %>% 
  mutate(LOTNO = as.factor(LOTNO),
         ITEM = as.factor(ITEM)) %>% 
  na.omit(aucDiff)
# gg_miss_var(df_merged)
# length(levels(df_merged$LOTNO))

df_yields_auc <- df_yields %>% 
  dplyr::filter(year(FIRE_DATE) >= 2018) %>% 
  left_join(allAucValues, by='LOTNO') %>% 
  dplyr::mutate_if(is.character, as.factor) %>% 
  mutate(LOTNO = as.factor(LOTNO),
         ITEM = as.factor(ITEM)) %>% 
  mutate(LOTNO = as.factor(LOTNO))%>% 
  na.omit(aucDiff)
# gg_miss_var(df_yields)
# length(levels(df_yields$LOTNO))

df_defects_auc <- df_defects %>% 
  dplyr::filter(year(FIRE_DATE) >= 2018) %>% 
  left_join(allAucValues, by='LOTNO') %>% 
  dplyr::mutate_if(is.character, as.factor) %>% 
  mutate(LOTNO = as.factor(LOTNO),
         ITEM = as.factor(ITEM)) %>% 
  mutate(LOTNO = as.factor(LOTNO))%>% 
  na.omit(aucDiff)
# gg_miss_var(df_defects)
# length(levels(df_defects$LOTNO))

plot_range   (kilns_G, 190, 201)
plotAucValues(kilns_G, 190, 201)
plotAucValues(kilns_G, 190, 201, crop = T)
plotAucValues(kilns_G, 190, 201, crop = T, free.x = T)

df_yields_auc %>% 
  dplyr::select(LOTNO, KILN, aucDiff)







# allAucValues <- allAucValues %>% dplyr::mutate_if(is.character, as.factor)
# length(levels(allAucValues$LOTNO))

# check influence of AUC on overall yields --------------------------------

# do some items have higher yields in some kilns based on AUC or weather?

# get top 50 most popular items in kiln G to check

df_sub <- df_yields %>% 
  dplyr::filter(KILN == "G") %>% 
  count(DESCRIPTION) %>% 
  left_join(df_yields %>% dplyr::filter(KILN == "G"))
  dplyr::filter(n >= 25)

df_sub %>% 
  ggplot(aes(x=DESCRIPTION))+
  geom_boxplot(aes(y=total_item_pct_yield))+
  # geom_boxplot(aes(y=aucDiff))+
  coord_flip()



glimpse(df_sub)
lm1 <- glm(total_item_pct_yield ~ aucDiff + temp_avg, 
    data=df_sub)
summary(lm1)

df_yields %>% 
  dplyr::filter(KILN == "G") %>% 
  count(DESCRIPTION, ITEM) %>% 
  left_join(df_yields) %>% 
  dplyr::filter(n >= 30) %>%
  ggplot(aes(x=DESCRIPTION))+
  geom_boxplot(aes(y=total_item_pct_yield))
  # facet_wrap(~DESCRIPTION)

sub <- df_yields %>% 
  dplyr::filter(KILN == "G") %>% 
  count(DESCRIPTION, ITEM) %>% 
  left_join(df_yields) %>% 
  dplyr::filter(n >= 30)
sub %>% 
  count(DESCRIPTION)

levels(sub$DESCRIPTION)



library(tidymodels)
library(skimr)

df %>% 
  count(DESCRIPTION,ITEM) %>% 
  # count(DESCRIPTION) %>% 
  arrange(-n) %>% slice(1:50)
  


skim(df)

set.seed(1)

df_split  <-  initial_split(df, prop = .75)
df_split

df_split %>% 
  training() %>% glimpse()