library(tidyverse)

source("src/kilnFunctions.R")
source("src/userFunctions.R")

rm(list = ls())

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







`# allAucValues <- allAucValues %>% dplyr::mutate_if(is.character, as.factor)
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