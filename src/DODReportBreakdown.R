source("src/cleanCombine.R")
source("src/userFunctions.R")

library(scales)
library(plotly)
library(DT)

# Reproduce DOD report from Steve

# initial variables -------------------------------------------------------



# subset selection --------------------------------------------------------

year_to_compare  <- 2019
year_current     <- 2020
defect_selection <- "BE"
select_EC <- "None"
select_PPI <- c("10","15","20","30", "40","45","5","50","65","80")
select_COMPOSITION <- c("PSZT", "PSZT-FBG")
select_KILN <- c("A","AR","B","BR","C","CR","D","DR","E","ER","F","FR","G","GR","H","HR","U")

# Recreate table using df_yields/defects separately -----------------------

options(scipen=999)

# merge both ----
df_costs_sep <- df_defects %>% 
  dplyr::filter(
    EC %in% select_EC &
    PPI %in% select_PPI &
    COMPOSITION %in% select_COMPOSITION & 
    KILN %in% select_KILN & 
    (year == year_to_compare | year == year_current) & 
    CAUSE == defect_selection
  ) %>% 
  group_by(year, month, year_month, CAUSE) %>% 
  dplyr::summarise(total_reject_cost = sum(reject_cost_single_row)) %>% 
  pivot_wider(names_from = CAUSE, values_from = total_reject_cost) %>% 
  left_join(
    df_yields %>% 
      dplyr::filter(
        EC %in% select_EC &
        PPI %in% select_PPI &
        COMPOSITION %in% select_COMPOSITION & 
        KILN %in% select_KILN & 
        (year == year_to_compare | year == year_current)
      ) %>% 
      group_by(KILN, ITEM, LOTNO) %>% slice(1) %>% ungroup() %>% 
      mutate(total_item_fired_cost = TOTAL_ITEM_FIRED * cost_piece) %>% 
      group_by(year, month) %>% 
      dplyr::summarise(total_fired_cost = sum(total_item_fired_cost)),
    by = c("year", "month")
  ) %>% 
  dplyr::select(year,month,year_month,total_fired_cost,everything())

df_costs_sep

# Recreate table using df_merged -----------------------------------------

df_costs_merged <- df_merged %>% 
  dplyr::filter(
    EC %in% select_EC &
    PPI %in% select_PPI &
    COMPOSITION %in% select_COMPOSITION & 
    KILN %in% select_KILN & 
    (year == year_to_compare | year == year_current) & 
    CAUSE == defect_selection
  ) %>% 
  group_by(year, month, year_month, CAUSE) %>% 
  dplyr::summarise(total_reject_cost = sum(reject_cost_single_row_D)) %>% 
  pivot_wider(names_from = CAUSE, values_from = total_reject_cost) %>% 
  left_join(
    df_merged %>% 
      dplyr::filter(
        EC %in% select_EC &
        PPI %in% select_PPI &
        COMPOSITION %in% select_COMPOSITION & 
        KILN %in% select_KILN & 
        (year == year_to_compare | year == year_current)
      ) %>% 
      group_by(ITEM, LOTNO, KILN) %>% slice(1) %>% ungroup() %>% 
      mutate(total_item_fired_cost = TOTAL_ITEM_FIRED_Y * cost_piece) %>% 
      group_by(year, month) %>% 
      dplyr::summarise(total_fired_cost = sum(total_item_fired_cost)),
    by = c("year", "month")
  ) %>% 
  dplyr::select(year, month, year_month, total_fired_cost, everything())
  
df_costs_merged
  
# calculate reject rate ---------------------------------------------------

# get overall rates from sep_
df_costs_sep_rates <- df_costs_sep %>% 
  dplyr::select(year, month, total_fired_cost, defect_selection) %>% 
  group_by(year) %>% 
  dplyr::summarise(total_fired_cost = sum(total_fired_cost),
                   total_defect_cost = sum(get(defect_selection))) %>% 
  mutate(defect_rate = total_defect_cost / total_fired_cost)
df_costs_sep_rates %>% 
  mutate(last_years_rate = ifelse(year == year_current, df_costs_sep_rates$defect_rate[1], NA),
         savings_loss = (total_fired_cost * last_years_rate) - total_defect_cost) %>% 
  dplyr::select( -last_years_rate)
  
# get overall rates from merged_
df_costs_merged_rates <- df_costs_merged %>% 
  dplyr::select(year, month, total_fired_cost, defect_selection) %>% 
  group_by(year) %>% 
  dplyr::summarise(total_fired_cost = sum(total_fired_cost),
                   total_defect_cost = sum(get(defect_selection))) %>% 
  mutate(defect_rate = total_defect_cost / total_fired_cost)
df_costs_merged_rates %>% 
  mutate(last_years_rate = ifelse(year == year_current, df_costs_merged_rates$defect_rate[1], NA),
         savings_loss = (total_fired_cost * last_years_rate) - total_defect_cost) %>% 
  dplyr::select( -last_years_rate)
  
