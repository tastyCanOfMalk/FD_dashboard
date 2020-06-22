source("src/cleanCombine.R")
source("src/userFunctions.R")

library(scales)
library(plotly)
library(DT)

# Reproduce DOD report from Steve

# initial variables -------------------------------------------------------

year_to_compare  <- 2019
year_current     <- 2020
defect_selection <- "BE"

filtered_subset <- df_merged %>% 
  dplyr::filter(
    PPI != "3d" &
    EC == "None" & 
    (COMPOSITION == "PSZT" | COMPOSITION == "PSZT-FBG")
  )


# Recreate table using df_yields/defects separately -----------------------

options(scipen=999)

# fired totals ----
# df_yields %>% 
#   dplyr::filter(
#     PPI != "3d" &
#       EC == "None" & 
#       (COMPOSITION == "PSZT" | COMPOSITION == "PSZT-FBG")
#   ) %>% 
#   dplyr::filter((year == year_to_compare | year == year_current)) %>% 
#   mutate(total_item_fired_cost = TOTAL_ITEM_FIRED * cost_piece) %>% 
#   group_by(year, month) %>% 
#   dplyr::summarise(total_fired_cost = sum(total_item_fired_cost))

# defect totals ----
# df_defects %>% 
#   dplyr::filter(
#     PPI != "3d" &
#       EC == "None" & 
#       (COMPOSITION == "PSZT" | COMPOSITION == "PSZT-FBG")
#   ) %>% 
#   # dplyr::filter((year == year_to_compare | year == year_current) & CAUSE == defect_selection) %>% 
#   dplyr::filter((year == year_to_compare | year == year_current)) %>%
#   group_by(year, month, CAUSE) %>% 
#   dplyr::summarise(total_reject_cost = sum(reject_cost_single_row)) %>% 
#   pivot_wider(names_from = CAUSE, values_from = total_reject_cost)

# merge both ----
df_costs_sep <- df_defects %>% 
  dplyr::filter(
    PPI != "3d" &
      EC == "None" & 
      (COMPOSITION == "PSZT" | COMPOSITION == "PSZT-FBG")
  ) %>% 
  # dplyr::filter((year == year_to_compare | year == year_current) & CAUSE == defect_selection) %>% 
  dplyr::filter((year == year_to_compare | year == year_current)) %>%
  group_by(year, month, CAUSE) %>% 
  dplyr::summarise(total_reject_cost = sum(reject_cost_single_row)) %>% 
  pivot_wider(names_from = CAUSE, values_from = total_reject_cost) %>% 
  left_join(
    df_yields %>% 
      dplyr::filter(
        PPI != "3d" &
          EC == "None" & 
          (COMPOSITION == "PSZT" | COMPOSITION == "PSZT-FBG")
      ) %>% 
      dplyr::filter((year == year_to_compare | year == year_current)) %>% 
      mutate(total_item_fired_cost = TOTAL_ITEM_FIRED * cost_piece) %>% 
      group_by(year, month) %>% 
      dplyr::summarise(total_fired_cost = sum(total_item_fired_cost)),
    by = c("year", "month")
  ) %>% 
  dplyr::select(year, month, total_fired_cost, BE, CW, SBS, PP, BF,DC,OTH,NF,NRS,WHL)

df_costs_sep

# Recreate table using df_merged -----------------------------------------

df_costs_merged <- df_merged %>% 
  dplyr::filter(
    PPI != "3d" &
      EC == "None" & 
      (COMPOSITION == "PSZT" | COMPOSITION == "PSZT-FBG")
  ) %>% 
  dplyr::filter((year == year_to_compare | year == year_current)) %>%
  group_by(year, month, CAUSE) %>% 
  dplyr::summarise(total_reject_cost = sum(reject_cost_single_row_D)) %>% 
  pivot_wider(names_from = CAUSE, values_from = total_reject_cost) %>% 
  left_join(
    df_merged %>% 
      dplyr::filter(
        PPI != "3d" &
          EC == "None" & 
          (COMPOSITION == "PSZT" | COMPOSITION == "PSZT-FBG")
      ) %>% 
      dplyr::filter((year == year_to_compare | year == year_current)) %>% 
      group_by(ITEM, LOTNO, KILN) %>% slice(1) %>% ungroup() %>% 
      mutate(total_item_fired_cost = TOTAL_ITEM_FIRED_Y * cost_piece) %>% 
      group_by(year, month) %>% 
      dplyr::summarise(total_fired_cost = sum(total_item_fired_cost)),
    by = c("year", "month")
  ) %>% 
  dplyr::select(year, month, total_fired_cost, BE, CW, SBS, PP, BF,DC,OTH,NF,NRS,WHL)
  
df_costs_merged
  
# calculate reject rate ---------------------------------------------------

year_to_compare  <- 2019
year_current     <- 2020
defect_selection <- "BE"

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
  
  
  


