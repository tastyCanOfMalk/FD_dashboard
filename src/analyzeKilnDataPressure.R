
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

# check pressures ---------------------------------------------------------

test <- kilns_All[kilns_All$LOTNO == "061320H",]

sec_y_scale=20000
sec_y_shift=1500

test %>% 
  mutate(
    pos_pres = case_when(
      pressure <= 0 ~ "neg",
      pressure > 0 ~ "pos",
      TRUE ~ "NA"),
    pres_limits = case_when(
      pressure > .01 & pressure < .03 ~ TRUE,
      TRUE ~ FALSE)
    ) %>% 
  ggplot(aes(x=time, y=avg_kiln_temp))+
  geom_line(aes(y=setpoint),color='grey50',linetype='dashed')+
  geom_line(color='blue')+
  # geom_point(aes(y=pressure * sec_y_scale + sec_y_shift,color=pres_limits),size=.3)+
  # geom_line(aes(y=pressure * sec_y_scale + sec_y_shift),alpha=.1,size=.1,color='grey50')+
  geom_point(aes(y=pressure * sec_y_scale + sec_y_shift,color=pos_pres),size=.3)+
  geom_line(aes(y=pressure * sec_y_scale + sec_y_shift),alpha=.1,size=.1,color='grey50')+
  geom_hline(aes(yintercept = .0 * sec_y_scale + sec_y_shift),color='grey70')+
  geom_hline(aes(yintercept = .01 * sec_y_scale + sec_y_shift),linetype='dotted')+
  geom_hline(aes(yintercept = .03 * sec_y_scale + sec_y_shift),linetype='dotted')+
  geom_ribbon(aes(ymin = auc_min, ymax = auc_max), fill='green', alpha=.3)+
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




# select temp, pressure columns ----------------------------------------------------------



kilns_All
  



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

set.seed(5)
n_kilns <- sample_n(all_kilns, 16) %>% dplyr::select(LOTNO) %>% unlist()

# random sample of LOTNOs -------------------------------------------------
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

# definitely not normal distribution --------------------------------------
df_yields_auc %>% 
  mutate(KILN2 = str_replace(KILN, "R", "")) %>% 
  ggplot(aes(x=aucDiff, y = ..count../sum(..count..)))+
  geom_density()+
  scale_y_continuous(labels = scales::percent_format())+
  scale_x_continuous(labels = scales::number_format(scale=1e-3, suffix='K'))+
  facet_wrap(~KILN2, scales='free')

library(ggpubr)  
ggqqplot(df_yields_auc[df_yields_auc$KILN == "H",]$aucDiff)
ggqqplot(df_yields_auc[df_yields_auc$KILN == "G",]$aucDiff)
ggqqplot(df_yields_auc[df_yields_auc$KILN == "C",]$aucDiff)
ggqqplot(df_yields_auc[df_yields_auc$KILN == "D",]$aucDiff)
ggqqplot(df_yields_auc[df_yields_auc$KILN == "E",]$aucDiff)

# does AUC value impact overall lot yields? -------------------------------
library(ggpointdensity)
library(viridis)

# join correlation of AUC, lot yield to original DF and plot ---------
df <- df_yields_auc %>% 
  group_by(LOTNO, KILN, aucDiff, temp_avg, precip, snow_fall, snow_depth) %>% 
  dplyr::summarise(
    total_fired = sum(TOTAL_ITEM_FIRED),
    total_rejected = sum(TOTAL_ITEM_REJECTED),
    pct_lot_yield = (total_fired - total_rejected) / total_fired
  ) %>% 
  mutate(KILN2 = str_replace(KILN, "R", ""))
df <- df %>% 
  group_by(KILN2) %>% 
  dplyr::summarise(cor = cor(pct_lot_yield, aucDiff)) %>% 
  left_join(df) %>% 
  mutate(kiln_cor = factor(paste0(KILN2, " (", round(cor,3), ")")))

df %>% 
  ggplot(aes(x=pct_lot_yield, y=aucDiff))+
  geom_pointdensity(alpha=.8, size=1)+
  # geom_smooth(alpha=.1, color = 'red')+
  # stat_smooth(alpha=.1)+
  scale_x_continuous(limits = c(0,1),labels = scales::percent_format())+
  scale_y_continuous(labels = scales::number_format(scale=1e-3, suffix='K'))+
  scale_color_viridis_c()+
  facet_wrap(~kiln_cor, scales='free_y')+
  xlab('Lot yield')+
  ylab('Area between curves')+
  labs(title = 'AUC versus entire lot yields',
       subtitle = 'Correlation value (in parentheses)')+
  theme(legend.position = 'none')

# does AUC impact item yields within a kiln? -------------
# yields df of kiln
df <- df_yields_auc %>% 
  mutate(KILN2 = str_replace(KILN, "R", "")) %>% 
  dplyr::filter(KILN2 == "A")

# get top items fired in kiln
df_items <- df %>% 
  count(DESCRIPTION) %>% 
  arrange(-n) %>% 
  slice(1:9)

# filter original df for top items
df <- df %>% 
  dplyr::filter(DESCRIPTION %in% df_items$DESCRIPTION)

# get cor values and join to original
df_cor <- df %>% 
  group_by(DESCRIPTION) %>% 
  dplyr::summarise(cor = round(cor(aucDiff, total_item_pct_yield),2)) %>%
  left_join(df) %>% 
  dplyr::select(DESCRIPTION, cor) %>% 
  group_by(DESCRIPTION) %>% slice(1)

df <- df %>% 
  left_join(df_cor) %>% 
  mutate(descr_cor = paste0(DESCRIPTION, " (", cor, ")")) %>% 
  right_join(df)

# plot
df %>% 
  ggplot(aes(x=total_item_pct_yield, y=aucDiff))+
  # geom_point()+
  # geom_bin2d()+
  # stat_density_2d(aes(fill=..level..))+
  # stat_bin_hex()+
  geom_pointdensity()+
  scale_x_continuous(limits = c(0,1),labels = scales::percent_format())+
  scale_y_continuous(labels = scales::number_format(scale=1e-3, suffix='K'))+
  scale_color_viridis_c()+
  xlab('Lot yield')+
  ylab('Area between curves')+
  labs(title = 'AUC versus item yields')+
  # facet_wrap(~descr_cor)+
  facet_wrap(~descr_cor, scales='free_y')+
  theme(legend.position = 'none')

# table
df %>% 
  count(cor, DESCRIPTION) %>% 
  arrange(-abs(cor)) %>% 
  mutate(
    cor = cell_spec(round(cor,2), 'html', color= ifelse(cor < 0, 'red', 'black'))
  ) %>% 
  set_colnames(c("Correlation", "Description", "Observations")) %>% 
  kable(format = 'html', escape = 'F') %>% 
  kable_styling('striped',full_width = F)


# AUC impact CW% ? --------------------------------------------------------

# how many %CW per lot?

# get lot fired total
df <- df_merged_auc %>%
  # lump kilns
  mutate(KILN2 = str_replace(KILN, "R", "")) %>% 
  mutate(reject_count_single_row = reject_vol_single_row_D * vol_piece) %>% 
  group_by(LOTNO) %>% 
  dplyr::summarise(total_lot_count_fired = sum(total_item_count_fired_D)) %>% 
  right_join(df_merged_auc)

# get defects total
df <- df %>% 
  group_by(LOTNO, CAUSE) %>%
  dplyr::summarise(total_defect_count_per_lot = sum(total_item_count_rejected_D)) %>% 
  right_join(df) %>% 
  group_by(LOTNO, CAUSE) %>%
  slice(1) %>% ungroup() %>%
  dplyr::select(LOTNO, CAUSE, total_lot_count_fired, total_defect_count_per_lot, aucDiff) %>% 
  mutate(pct_defect = 1 - (total_lot_count_fired - total_defect_count_per_lot)  / total_lot_count_fired )
  
# fill missing values
pct_defect_by_lot <- df %>% 
  pivot_wider(id_cols     = LOTNO, 
              names_from  = CAUSE, 
              values_from = pct_defect,
              values_fill = 0)

# join pct defect to aucDiff
pct_defect_by_lot <- pct_defect_by_lot %>% 
  pivot_longer(cols = BE:BIT) %>% 
  # join to aucDiff 
  left_join(
    df_merged_auc %>% 
      mutate(KILN = str_replace(KILN, "R", "")) %>% 
      group_by(LOTNO) %>% slice(1) %>% 
      dplyr::select(LOTNO, KILN, aucDiff)
  ) %>% 
  set_colnames(c("LOTNO", "CAUSE", "defect_pct", "KILN", "aucDiff")) %>% 
  mutate_if(is.character, factor)

# # get cor, join
# pct_defect_by_lot <- pct_defect_by_lot %>%
#   group_by(name, KILN) %>%
#   dplyr::summarise(cor = cor(value, aucDiff)) %>%
#   arrange(-cor) %>%
#   right_join(pct_defect_by_lot) %>%
#   dplyr::mutate(name_cor = paste0(name, " (", cor, ")"))

# for CW, what is the relationship between KILN and aucDiff?
def = "NRS"

pct_defect_by_lot %>%
  dplyr::filter(CAUSE == def) %>% 
  group_by(KILN) %>%
  dplyr::summarise(cor = cor(defect_pct, aucDiff)) %>%
  # arrange(-cor) %>%
  right_join(pct_defect_by_lot %>%
               dplyr::filter(CAUSE == def)) %>% 
  dplyr::mutate(kiln_cor = paste0(KILN, " (", round(cor,2), ")")) %>%
  ggplot(aes(y=defect_pct, x=aucDiff))+
  geom_smooth(alpha=.2)+
  geom_pointdensity()+
  scale_color_viridis_c()+
  facet_wrap(~kiln_cor, scales = 'free')+
  scale_x_continuous(labels = scales::number_format(scale=1e-3, suffix='K'))+
  scale_y_continuous(labels = scales::percent_format(), limits = c(0,.1))+
  labs(
    title = paste0(def, " defect rate per lot vs AUC"),
    subtitle = "Correlation value (in parentheses)"
  )+
  xlab("Area between setpoint, kiln temp")+
  ylab(paste0(def, " defect rate"))

# pct_defect_by_lot %>%
#   group_by(CAUSE) %>%
#   dplyr::summarise(median = median(defect_pct),
#                    mean = mean(defect_pct),
#                    sum = sum(defect_pct)) %>%
#   arrange(-mean)

# repeat above, switch cause and kilns ------------------------------------
# for each kiln, compare the defect rate vs AUC for each cause
kil = "H"

pct_defect_by_lot %>%
  dplyr::filter(KILN == kil) %>% 
  group_by(CAUSE) %>%
  dplyr::summarise(cor = cor(defect_pct, aucDiff),
                   cor = ifelse(is.na(cor), 0, cor)) %>%
  # arrange(-cor) %>%
  right_join(pct_defect_by_lot %>%
               dplyr::filter(KILN == kil)) %>% 
  dplyr::mutate(cause_cor = paste0(CAUSE, " (", round(cor,2), ")")) %>%
  ggplot(aes(y=defect_pct, x=aucDiff))+
  geom_smooth(alpha=.2)+
  geom_pointdensity(adjust = .2)+
  scale_color_viridis_c()+
  facet_wrap(~cause_cor, scales='free')+
  scale_x_continuous(labels = scales::number_format(scale=1e-3, suffix='K'))+
  scale_y_continuous(labels = scales::percent_format())+
  # scale_y_continuous(labels = scales::percent_format(), limits = c(0,.06))+
  labs(
    title = paste0("Kiln ", kil, " defect rate per defect vs AUC"),
    subtitle = "Correlation value (in parentheses)"
  )+
  xlab("Area between setpoint, kiln temp")+
  ylab(paste0("Defect rate"))




# -------------------------------------------------------------------------


pct_defect_by_lot %>%
  dplyr::filter(KILN == kil) %>% 
  group_by(CAUSE) %>%
  dplyr::summarise(cor = cor(defect_pct, aucDiff),
                   cor = ifelse(is.na(cor), 0, cor)) %>%
  # arrange(-cor) %>%
  right_join(pct_defect_by_lot %>%
               dplyr::filter(KILN == kil)) %>% 
  dplyr::mutate(cause_cor = paste0(CAUSE, " (", round(cor,2), ")")) %>%
  ggplot(aes(y=defect_pct, x=aucDiff))+
  geom_smooth(alpha=.2)+
  # geom_pointdensity(adjust = .2)+
  geom_point(aes(color=CAUSE))
  # scale_color_viridis_c()
  
  # facet_wrap(~cause_cor, scales='free')+
  scale_x_continuous(labels = scales::number_format(scale=1e-3, suffix='K'))+
  scale_y_continuous(labels = scales::percent_format())+
  # scale_y_continuous(labels = scales::percent_format(), limits = c(0,.06))+
  labs(
    title = paste0("Kiln ", kil, " defect rate per defect vs AUC"),
    subtitle = "Correlation value (in parentheses)"
  )+
  xlab("Area between setpoint, kiln temp")+
  ylab(paste0("Defect rate"))








  group_by(LOTNO,CAUSE) %>% 
  dplyr::summarise(defect_totals = sum(reject_count_single_row),
                   total_items_fired = )
  
  
  dplyr::filter(KILN2 == "A") %>% 
  dplyr::select(LOTNO, KILN2, DESCRIPTION, CAUSE, TOTAL_ITEM_FIRED_Y:total_item_pct_yield_Y,aucDiff) %>%
  group_by()
  

# does AUC impact occurences of certain defects within a kiln?


# bin to quantile values --------
df_G <- df_yields_auc %>% 
  group_by(LOTNO, KILN, aucDiff, temp_avg, precip, snow_fall, snow_depth) %>% 
  dplyr::summarise(
    total_fired = sum(TOTAL_ITEM_FIRED),
    total_rejected = sum(TOTAL_ITEM_REJECTED),
    pct_lot_yield = (total_fired - total_rejected) / total_fired
    ) %>% 
  mutate(KILN2 = str_replace(KILN, "R", "")) %>% 
  dplyr::filter(KILN2 == "G")
  
cut_G      <- bins.quantiles(df_G$pct_lot_yield, 6, 10)
cut_G_vals <- bins.getvals(cut_G)

df_G %>% 
  mutate(pct_lot_yield_binned = case_when(
    pct_lot_yield < cut_G_vals[[2]] ~ "F",
    (pct_lot_yield >= cut_G_vals[[2]] & pct_lot_yield < cut_G_vals[[3]]) ~ "E",
    (pct_lot_yield >= cut_G_vals[[3]] & pct_lot_yield < cut_G_vals[[4]]) ~ "D",
    (pct_lot_yield >= cut_G_vals[[4]] & pct_lot_yield < cut_G_vals[[5]]) ~ "C",
    (pct_lot_yield >= cut_G_vals[[5]] & pct_lot_yield < cut_G_vals[[6]]) ~ "B",
    pct_lot_yield >= cut_G_vals[[6]] ~ "A",
    TRUE ~ "error"
  )) %>% 
  ggplot(aes(x=aucDiff, y=pct_lot_yield_binned))+
  geom_boxplot()
  # coord_flip()






a <- a %>% 
  left_join(cut(a$pct_lot_yield, 4), by = LOTNO)

  ggplot(aes(x=pct_lot_yield, y=aucDiff))+
  # ggplot(aes(x=aucDiff, y=pct_lot_yield))+
  geom_pointdensity(alpha=.8)+
  scale_x_continuous(limits = c(0,1),labels = scales::percent_format())+
  scale_y_continuous(labels = scales::number_format(scale=1e-3, suffix='K'))+
  scale_color_viridis_c()+
  facet_wrap(~KILN2, scales='free_y')












  
  
# overall look shows 
  
  
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