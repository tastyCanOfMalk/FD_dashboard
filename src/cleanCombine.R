# Packages, fxns ----
if (!require("plyr")) install.packages("plyr")
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("naniar")) install.packages("naniar")

library(plyr)
library(dplyr)
library(tidyverse)
library(naniar)
library(lubridate)
library(magrittr)
library(janitor)

rm(list = ls())

# directory locations -----
yield_data_dir   = "data/csv/Yield_data/"
defect_data_dir  = "data/csv/Defect_data/"
pr1_pr6_lookup   = "data/resources/PR1_PR6_Lookup.csv"
vol_piece_lookup = "data/resources/vol_piece.csv"

source("src/userFunctions.R")

# ' ----
# IMPORT/CLEAN YIELD DATA----

fnames   = list.files(yield_data_dir, full.names=TRUE)

df_all <- ldply(fnames, get_files) %>% 
  remove_empty(which = "rows") %>% 
  # dplyr::filter(rowSums(is.na(.)) != ncol(.)) %>% 
  clean_names() %>% 
  as_tibble()

# reformat columns names / text ----
df_all <- clean_column_names(df_all)
names  <- colnames(df_all)
names[c(2,8,13,16)] <- c("LOTNO",
                         "DESCRIPTION",
                         "COMPOSITION",
                         "TOTAL_REJECTED")
colnames(df_all) <- names

# remove junk columns ----
df_all <- df_all %>% 
  dplyr::select(c(
    FIRE_DATE,
    LOTNO,
    ITEM,
    DESCRIPTION,
    KILN,
    TYPE,
    EC,
    COMPOSITION,
    TOTAL_FIRE,
    TOTAL_REJECTED,
    VOL_PIECE,
    COST_PIECE,
    TOTAL_PACK   # needed to fix pct yield
  ))

# fix duplicate items per single lotno/kiln ----
df_all <- 
  # slice single item per lotno/kiln
  df_all %>%
  group_by(LOTNO,ITEM,KILN) %>% slice(1) %>% ungroup() %>%
  dplyr::select(-c(
    TOTAL_FIRE,
    TOTAL_REJECTED,
    TOTAL_PACK)) %>% 
  left_join(
    # sum of values when multiple items per lotno/kiln, join
    df_all %>%
      group_by(LOTNO,ITEM,KILN) %>%
      dplyr::summarise_at(
        c("TOTAL_FIRE",
          "TOTAL_REJECTED",
          "TOTAL_PACK"),
        sum)
  )

# mutate columns / classes ---- 
df_all_clean <- df_all %>%
  mutate(
    FIRE_DATE   = as.Date( FIRE_DATE, "%m/%d/%Y" ),
    ITEM        = gsub("[^0-9]", "", ITEM),
    DESCRIPTION = toupper(DESCRIPTION),
    EC          = ifelse( is.na(EC), "None", EC),
    APPI        = gsub("APPI", "", str_extract(DESCRIPTION, "([0-9]*)APPI")),
    PPI         = ifelse( is.na(APPI),
                        ifelse( str_detect(DESCRIPTION, "MMC"),
                                "3d",
                                gsub("PPI", "", str_extract(DESCRIPTION, "([0-9]*)PPI"))),
                        APPI),
    # give is3d own column?
    # is3d        = ifelse(PPI == "3d", TRUE, FALSE),
    # PPI         = ifelse(PPI == "3d", NA, PPI),
    # fix incorrect TOTAL_REJECTED (rejected based on (fired-packed), need for pct_yield calc)
    pack_equals_fired_minus_rejects = ifelse( TOTAL_PACK == (TOTAL_FIRE - TOTAL_REJECTED), 
                                              TRUE, 
                                              FALSE),
    TOTAL_REJECTED = ifelse( pack_equals_fired_minus_rejects == FALSE,
                             TOTAL_FIRE - TOTAL_PACK,
                             TOTAL_REJECTED),
    
    pct_yield   = ( abs(TOTAL_FIRE - TOTAL_REJECTED) / TOTAL_FIRE )
    ) %>% 
  # remove junk cols
  dplyr::select(-c(
    APPI,
    # PCT_YIELD,
    TOTAL_PACK,
    pack_equals_fired_minus_rejects
    ))

# joining / cleaning functions ----
df_all_clean <- df_all_clean %>% 
  replace_pr1_with_pr6() %>% 
  replace_cost_vol() %>% 
  clean_composition() %>%
  create_year_month_cols() %>%
  mutate_if(is.character, as.factor)

# relevel PPI ----
df_all_clean$PPI <- factor(df_all_clean$PPI, 
                           levels=c('5', '10', '15', '20', '30', '40',
                                    '45', '50', '65', '80', '3d'))

# add new columns ----
df_all_clean <- df_all_clean %>%
  mutate(
    # total_item_fired_cost    = TOTAL_FIRE * cost_piece,
    # total_item_rejected_cost = TOTAL_REJECTED * cost_piece,

    total_item_vol_fired     = TOTAL_FIRE * vol_piece,
    total_item_vol_rejected  = TOTAL_REJECTED * vol_piece
  )

# rename columns ----
# data.frame(colnames(df_all_clean))
names <- colnames(df_all_clean)
names[c(9,10,12)] <- c("TOTAL_ITEM_FIRED",
                       "TOTAL_ITEM_REJECTED",
                       "total_item_pct_yield")
colnames(df_all_clean) <- names

# reorder tibble ----
# data.frame(colnames(df_all_clean))
df_yields <- df_all_clean %>% 
  dplyr::select(
    FIRE_DATE,
    month,
    year,
    year_month,
    
    LOTNO,
    KILN,
    ITEM,
    TYPE,
    EC,
    PPI,
    COMPOSITION,
    DESCRIPTION,
    
    TOTAL_ITEM_FIRED,
    TOTAL_ITEM_REJECTED,
    total_item_pct_yield,    # = ( abs(TOTAL_ITEM_FIRED - TOTAL_ITEM_REJECTED) / TOTAL_ITEM_FIRED )

    cost_piece,
    # total_item_fired_cost,
    # total_item_rejected_cost,
    
    vol_piece,
    total_item_vol_fired,
    total_item_vol_rejected
  )

# ' ----
# IMPORT/CLEAN DEFECT DATA ----
fnames   = list.files(defect_data_dir, full.names=TRUE)

df_all <- ldply(fnames, get_files) %>% 
  remove_empty(which = "rows") %>% 
  # dplyr::filter(rowSums(is.na(.)) != ncol(.)) %>% 
  clean_names() %>% 
  as_tibble()

# reformat columns names / text ----
df_all <- clean_column_names(df_all)
names <- colnames(df_all)
names[c(2,7,12,16)] <- c("LOTNO",
                      "DESCRIPTION",
                      "COMPOSITION",
                      "TOTVOLFIRE")
colnames(df_all) <- names

# remove junk columns ----
df_all <- df_all %>% 
  dplyr::select(c(
    FIRE_DATE,
    LOTNO,
    
    ITEM,
    DESCRIPTION,
    TYPE,
    EC,
    COMPOSITION,
    KILN,
    
    CAUSE,
    REJECT_VOL,
    TOTVOLFIRE,

    COST_IN3
  ))

# total_item_vol_fired/rejected ----
df_all <- df_all %>% 
  # sum values for each item per lot/kiln
  left_join(
    df_all %>% 
      group_by(LOTNO, ITEM, KILN) %>% 
      dplyr::summarise(total_item_fired_vol       = sum(TOTVOLFIRE),
                       total_item_rejected_vol    = sum(REJECT_VOL)
      ),
    by = c("LOTNO", "ITEM", "KILN")
  ) %>% 
  # then remove the NA
  dplyr::select(-c(TOTVOLFIRE))

# remove duplicate CAUSE per each lotno/item ----
df_all <- df_all %>% 
  left_join(
    # sum value for each cause per lot/kiln/item
    df_all %>% 
      group_by(LOTNO, ITEM, KILN, CAUSE) %>% 
      dplyr::summarise(reject_vol_single_row = sum(REJECT_VOL))
  ) %>% 
  # slice to remove duplicate cause rows
  group_by(LOTNO, ITEM, KILN, CAUSE) %>% slice(1) %>% ungroup() %>% 
  dplyr::select(-c(REJECT_VOL))

# mutate columns / classes ----
df_all_clean <- df_all %>%
  mutate(
    FIRE_DATE   = as.Date( FIRE_DATE, "%m/%d/%Y" ),
    ITEM        = gsub("[^0-9]", "", ITEM),
    DESCRIPTION = toupper(DESCRIPTION),
    EC          = ifelse( is.na(EC), "None", EC),
    APPI        = gsub("APPI", "", str_extract(DESCRIPTION, "([0-9]*)APPI")),
    PPI         = ifelse( is.na(APPI),
                          ifelse( str_detect(DESCRIPTION, "MMC"),
                                  "3d",
                                  gsub("PPI", "", str_extract(DESCRIPTION, "([0-9]*)PPI"))),
                          APPI),
    reject_cost_single_row = round(reject_vol_single_row * COST_IN3, 2)
    ) %>% 
  dplyr::select(-APPI)

# joining / cleaning functions ----
df_all_clean <- df_all_clean %>% 
  replace_pr1_with_pr6() %>% 
  clean_composition() %>%
  create_year_month_cols() %>% 
  mutate_if(is.character, as.factor)

# get vol/cost_piece from df_yields ----
df_all_clean <- df_all_clean %>%
  left_join(
    # create cost/vol index
    df_yields %>%
      group_by(ITEM) %>% slice(1) %>% ungroup() %>%
      dplyr::select(ITEM, cost_piece, vol_piece),
    by=c("ITEM")
  ) %>%
  mutate_if(is.character, as.factor)

# relevel PPI ----
df_all_clean$PPI <- factor(df_all_clean$PPI, 
                           levels=c('5', '10', '15', '20', '30', '40',
                                    '45', '50', '65', '80', '3d'))

# add new columns ----
## total_item_cost_rejected
df_all_clean <- df_all_clean %>%
  left_join(
    df_all_clean %>%
      group_by(LOTNO, ITEM, KILN) %>%
      dplyr::summarise(
        total_item_cost_of_rejects = sum(reject_cost_single_row)
      ),
    by = c("LOTNO", "ITEM", "KILN")
  )

## total_lot_vol_fired/rejected
df_all_clean <- df_all_clean %>% 
  left_join(
    df_all_clean %>% 
      group_by(LOTNO,ITEM,KILN) %>% slice(1) %>%
      group_by(LOTNO) %>% 
      dplyr::summarise(total_lot_vol_fired    = sum(total_item_fired_vol),
                       total_lot_vol_rejected = sum(total_item_rejected_vol))
  )

# calculate items fired/rejected from vol_piece ----
df_all_clean <- df_all_clean %>% 
  mutate(
    total_item_count_fired    = round(total_item_fired_vol / vol_piece),
    total_item_count_rejected = round(total_item_rejected_vol / vol_piece)
  )

# rename columns ----
# data.frame(colnames(df_all_clean))
# names <- colnames(df_all_clean)
# names[c(10,20)] <- c("REJECT_VOL_SINGLE_ROW",
#                      "reject_cost_single_row")
# colnames(df_all_clean) <- names

# reorder tibble ----
# data.frame(colnames(df_all_clean))
df_defects <- df_all_clean %>% 
  dplyr::select(c(
    FIRE_DATE,
    month,
    year,
    year_month,
    
    LOTNO,
    KILN,
    ITEM,
    TYPE,
    EC,
    PPI,
    COMPOSITION,
    DESCRIPTION,
    
    CAUSE,
    COST_IN3,                   
    
    reject_vol_single_row,            
    reject_cost_single_row,     # = ( reject_vol_single_row * COST_IN3 )
    
    total_item_fired_vol,       # group_by( ITEM,LOT,KILN ) %>% sum( TOTVOLFIRE )
    total_item_rejected_vol,    # group_by( ITEM,LOT,KILN ) %>% sum( reject_vol_single_row )
    total_item_cost_of_rejects, # group_by( ITEM,LOT,KILN ) %>% sum( reject_cost_single_row )
      
    vol_piece,                  # joined from df_yields
    total_item_count_fired,     # = ( total_item_vol_fired / vol_piece )
    total_item_count_rejected   # = ( total_item_vol_rejected / vol_piece )
    
    # total_lot_vol_fired,
    # total_lot_vol_rejected,
    
    # cost_piece
  ))

# ' ----
# JOIN YIELD + DEFECT ----

# equalize numrows in each dataset ----
max_yield_date  <- max(df_yields$FIRE_DATE)
max_defect_date <- max(df_defects$FIRE_DATE)

if(max_yield_date > max_defect_date){
  original_rows <- nrow(df_yields)
  
  # store removed data
  df_yields_removed_date_out_of_range <- df_yields %>% 
    dplyr::filter(FIRE_DATE > max_defect_date)
  
  df_defects_removed_date_out_of_range <- 
    tibble("No Data")
  
  df_yields <- df_yields %>% 
    dplyr::filter(FIRE_DATE <= max_defect_date)
  
  rows_removed <- original_rows - nrow(df_yields)
  
  msg_yield_defect_date_equalizing <- paste("Rows omitted from yield data to ensure matching time periods in joined data:",
                                            rows_removed)
  
} else if(max_defect_date > max_yield_date){
  original_rows <- nrow(df_defects)
  
  # store removed data
  df_defects_removed_date_out_of_range <- df_defects %>% 
    dplyr::filter(FIRE_DATE > max_yield_date)
  
  df_yields_removed_date_out_of_range <- 
    tibble("No Data")
  
  df_defects <- df_defects %>% 
    dplyr::filter(FIRE_DATE <= max_yield_date)

  rows_removed <- original_rows - nrow(df_defects)
  
  msg_yield_defect_date_equalizing <- paste("Rows omitted from defect data to ensure matching time periods in joined data:",
                                            rows_removed)
}

# join dfs ----
# yields should keep majority of columns, as all entries should have yield data, while not all may have defects data; left joining omits entries that have defect data with no matching yield data
df_merged <- left_join(
  df_yields %>% 
    dplyr::select(
      FIRE_DATE,
      # month,
      # year,
      # year_month,
      LOTNO,
      KILN,
      ITEM,
      TYPE,
      EC,
      PPI,
      COMPOSITION,
      DESCRIPTION,
      TOTAL_ITEM_FIRED,
      TOTAL_ITEM_REJECTED,
      total_item_pct_yield,
      cost_piece,
      vol_piece,
      total_item_vol_fired,
      total_item_vol_rejected
    ),
  df_defects %>% 
    dplyr::select(
      FIRE_DATE,
      # month,
      # year,
      # year_month,
      LOTNO,
      KILN,
      ITEM,
      # TYPE,
      # EC,
      # PPI,
      # COMPOSITION,
      # DESCRIPTION,
      CAUSE,
      COST_IN3,
      reject_vol_single_row,
      reject_cost_single_row,
      total_item_fired_vol,
      total_item_rejected_vol,
      total_item_cost_of_rejects,
      # vol_piece,
      total_item_count_fired,
      total_item_count_rejected,
    )
)

# get nrows on full join
df_merged_full_join <- 
  full_join(
    df_yields %>% 
      dplyr::select(FIRE_DATE,month,year,year_month,LOTNO,KILN,ITEM,TYPE,EC,PPI,
                    COMPOSITION,DESCRIPTION,TOTAL_ITEM_FIRED,TOTAL_ITEM_REJECTED,
                    total_item_pct_yield,cost_piece,vol_piece,total_item_vol_fired,total_item_vol_rejected),
    df_defects %>% 
      dplyr::select(FIRE_DATE,LOTNO,KILN,ITEM,CAUSE,COST_IN3,reject_vol_single_row,
                    reject_cost_single_row,total_item_fired_vol,total_item_rejected_vol,
                    total_item_cost_of_rejects,total_item_count_fired,total_item_count_rejected)
    )
# message
msg_have_defect_but_no_yield <- paste(
  "Rows omitted from defect data that have no matching yield data:",
  nrow(df_merged_full_join) - nrow(df_merged))

# store removed data
df_defect_no_matching_yield <- 
  df_merged_full_join[is.na(df_merged_full_join$year_month), ] %>% 
  dplyr::select(!c(month:year_month,TYPE:total_item_vol_rejected)) %>% 
  left_join(
    df_defects %>% dplyr::select(ITEM, DESCRIPTION) %>% group_by(ITEM,DESCRIPTION) %>% slice(1)
  ) %>% 
  dplyr::select(c(FIRE_DATE:ITEM, DESCRIPTION, everything()))

# remove no cause, zero defect rows ----
# if (CAUSE == NA & items rejected != 0), then there is no corresponding defect data for these items; remove?
df_yield_no_matching_defect <- df_merged %>% 
  dplyr::filter(is.na(CAUSE) & (TOTAL_ITEM_REJECTED != 0)) %>% 
  dplyr::select(c(FIRE_DATE:total_item_vol_rejected))
# remove 
df_merged <- anti_join(df_merged, df_yield_no_matching_defect)
# message
msg_non_zero_defect_but_no_defect_data <- paste(
  "Rows omitted from yield data that have defects != 0 but no corresponding defect data:",
  nrow(df_yield_no_matching_defect))

# fix various NA values ----
df_merged <- df_merged %>% 
  mutate_if(is.factor, as.character) %>% 
  mutate(
    CAUSE                   = ifelse(TOTAL_ITEM_REJECTED == 0, "No defect", CAUSE),
    COST_IN3                = ifelse( is.na(COST_IN3), 
                                      cost_piece / vol_piece, 
                                      COST_IN3 ),
    reject_vol_single_row   = ifelse( is.na(reject_vol_single_row),
                                      total_item_vol_rejected,
                                      reject_vol_single_row),
    reject_cost_single_row  = ifelse( (is.na(reject_cost_single_row) & TOTAL_ITEM_REJECTED == 0),
                                      0, 
                                      reject_cost_single_row),
    total_item_fired_vol    = ifelse( is.na(total_item_fired_vol),
                                      total_item_vol_fired,
                                      total_item_fired_vol),
    total_item_rejected_vol = ifelse( is.na(total_item_rejected_vol),
                                      total_item_vol_rejected,
                                      total_item_rejected_vol),
    total_item_cost_of_rejects = ifelse( (is.na(total_item_cost_of_rejects) & TOTAL_ITEM_REJECTED == 0),
                                         0,
                                         total_item_cost_of_rejects),
    total_item_count_fired     = ifelse( is.na(total_item_count_fired),
                                         TOTAL_ITEM_FIRED,
                                         total_item_count_fired),
    total_item_count_rejected  = ifelse( is.na(total_item_count_rejected),
                                         TOTAL_ITEM_REJECTED,
                                         total_item_count_rejected)
    ) %>% 
  create_year_month_cols() %>% 
  mutate_if(is.character, as.factor)

# reorder cols ----
df_merged <- df_merged %>% 
  dplyr::select(c(
    FIRE_DATE,
    month,
    year,
    year_month,
    
    LOTNO,
    KILN,
    ITEM,
    TYPE,
    EC,
    PPI,
    COMPOSITION,
    DESCRIPTION,
    
    CAUSE,
    reject_vol_single_row,
    reject_cost_single_row,
    
    TOTAL_ITEM_FIRED,
    total_item_count_fired,
    
    TOTAL_ITEM_REJECTED,
    total_item_count_rejected,
    
    total_item_pct_yield,
    
    total_item_cost_of_rejects,
    
    total_item_fired_vol,
    total_item_vol_fired,
    
    total_item_rejected_vol,
    total_item_vol_rejected,
    
    cost_piece,
    vol_piece,
    COST_IN3
  ))

# rename cols ----
names <- colnames(df_merged)
data.frame(names)
names <- c(
  "FIRE_DATE",
  "month",
  "year",
  "year_month",
  
  "LOTNO",
  "KILN",
  "ITEM",
  "TYPE",
  "EC",
  "PPI",
  "COMPOSITION",
  "DESCRIPTION",
  
  "CAUSE",
  "reject_vol_single_row_D",
  "reject_cost_single_row_D",
  
  "TOTAL_ITEM_FIRED_Y",
  "total_item_count_fired_D",
  
  "TOTAL_ITEM_REJECTED_Y",
  "total_item_count_rejected_D",
  
  "total_item_pct_yield_Y",
  
  "total_item_cost_of_rejects_D",
  
  "total_item_fired_vol_D",
  "total_item_vol_fired_Y",
  
  "total_item_rejected_vol_D",
  "total_item_vol_rejected_Y",
  
  "cost_piece",
  "vol_piece",
  "COST_IN3"
)
colnames(df_merged) <- names



# ' -----
# Messages ----
join_messages <- c(
  msg_yield_defect_date_equalizing,
  msg_have_defect_but_no_yield,
  msg_non_zero_defect_but_no_defect_data
)

# gg_miss_var(df_merged)
# gg_miss_which(df_merged)
# join_messages

# df_yields_removed_date_out_of_range
# df_defects_removed_date_out_of_range
# df_defect_no_matching_yield
# df_yield_no_matching_defect


defect_xlsx_dir <- "data/xlsx/Defect_Data/"
yield_xlsx_dir  <- "data/xlsx/Yield_Data/"
defect_xlsx_files <- list.files(defect_xlsx_dir)
yield_xlsx_files  <- list.files(yield_xlsx_dir)

defect_csv_dir <- "data/csv/Defect_data/"
yield_csv_dir  <- "data/csv/Yield_data/"

# remove all other data ----
rm(list=setdiff(ls(), c("df_merged",
                        "df_defects",
                        "df_yields",
                        
                        "defect_xlsx_files",
                        "yield_xlsx_files",
                        "defect_xlsx_dir",
                        "yield_xlsx_dir",
                        "defect_csv_dir",
                        "yield_csv_dir",
                        
                        "join_messages",
                        
                        "df_yields_removed_date_out_of_range",
                        "df_defects_removed_date_out_of_range",
                        "df_defect_no_matching_yield",
                        "df_yield_no_matching_defect"
                        )))

# write_csv(df_defects, "output\\processed_data\\df_defects.csv")
# write_csv(df_yields, "output\\processed_data\\df_yields.csv")
# write_csv(df_merged, "output\\processed_data\\df_merged.csv")
