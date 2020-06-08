source("src/cleanCombine.R")
source("src/userFunctions.R")

if (!require("scales")) install.packages("scales")
if (!require("plotly")) install.packages("plotly")
if (!require("DT")) install.packages("DT")

library(scales)
library(plotly)
library(DT)
# join_messages

# item-description key ----
# item_description_key <- df_merged %>% 
#   group_by(ITEM) %>% slice(1) %>% ungroup() %>% 
#   dplyr::select(ITEM, DESCRIPTION) %>% 
#   mutate_if(is.factor, as.character)

df_sub <- df_merged %>% 
  dplyr::filter(
    PPI != "3d" & 
    EC == "None" & 
    (COMPOSITION == "PSZT" | COMPOSITION == "PSZT-FBG")
  )
  
# costs per volume ----
g.title =    "Cost of BE defects in subset per total volume of subset fired"
g.subtitle = "Costs of only BE used; volume of entire subset regardless of defect used "
g.caption =  "{PSZT/FBG, not3d, noEC}, merged dataset"

df_cpv <- df_sub %>% 
  dplyr::filter(
    CAUSE == "BE",
    year(FIRE_DATE) >= 2019
    ) %>% 
  group_by(year_month, year, month) %>% 
  dplyr::summarise(total_reject_cost_D = sum(reject_cost_single_row_D)) %>% 
  left_join(
    df_sub %>% 
      group_by(ITEM, LOTNO, KILN) %>% slice(1) %>% ungroup() %>% 
      dplyr::filter(
        year(FIRE_DATE) >= 2019
        ) %>% 
      group_by(year_month, year, month) %>% 
      dplyr::summarise(total_vol_fired_Y = sum(total_item_vol_fired_Y))
  ) %>% 
  mutate(cost_per_volume = total_reject_cost_D / total_vol_fired_Y)

gg_cpv <- df_cpv %>% 
  ggplot(aes(x=month, 
             y=cost_per_volume, 
             fill=year,
             text = 
               paste(
                 "Year:", year,
                 "<br>Total BE reject cost:", mynumber(total_reject_cost_D),
                 "<br>Total subset vol fired:", mynumber(total_vol_fired_Y),
                 "<br>Cost per in3:", mycurrency(cost_per_volume)
               )))+
  geom_bar(stat='identity',
           position = 'dodge')+
  scale_fill_brewer(name = "Year", palette = "Paired")+
  scale_y_continuous(name = "Cost per volume (USD per in3)",
                     labels = dollar_format(accuracy = .001),
                     breaks = seq(0,.3,.005))+
  theme_minimal()+
  labs(title    = g.title,
       subtitle = g.subtitle,
       caption  = g.caption)+
  xlab("Month")
  
gp_cpv <- ggplotly(gg_cpv, tooltip = c("text"))%>%
  layout(title = list(text = paste0(g.title,
                                    '<br>',
                                    '<sup>',
                                    g.subtitle,
                                    '</sup>')),
         margin=list(t = 75),
         annotations = 
           list(x = 1, y = -0.15, 
                text = g.caption, 
                showarrow = F, xref='paper', yref='paper', 
                xanchor='right', yanchor='auto', xshift=0, yshift=0,
                font=list(size=12, color="black"))
  )

# *savings-loss ---- 
g.title =    "Cost per volume by month, with estimted savings"
g.subtitle = "S/L = (Current fired volume * Last years CPV) - Current reject cost"
g.caption =  "From merged dataset"

# filter for months with 2 rows of data
months <- data.frame(summary(df_cpv$month)>1) %>% 
  mutate(month = rownames(data.frame(summary(df_cpv$month)>1))) %>% 
  set_colnames(c(
    "tf", "month"
  )) %>% 
  dplyr::filter(tf) %>% 
  dplyr::select(month)

keep_months <- apply(months, 1, paste, collapse="")
df_cpv_sub <- df_cpv[df_cpv$month %in% keep_months,]

df_cpv_sub_2019 <- df_cpv_sub %>% 
  ungroup() %>% 
  dplyr::filter(year == 2019) %>%
  dplyr::select(-c(
    year_month,
    year
  )) %>% 
  set_colnames(c(
    # "year_month",
    # "year",
    "month",
    "total_reject_cost_D_2019",
    "total_vol_fired_Y_2019",
    "cost_per_volume_2019"))

df_cpv_sub_summary <- df_cpv_sub %>% 
  left_join(df_cpv_sub_2019) %>% 
  mutate(savings_loss = 
           ifelse(year == 2020,
                  (total_vol_fired_Y * cost_per_volume_2019) - total_reject_cost_D,
                  NA
                  ))

# get max CPV value for geom_text
max_geom_text <- df_cpv_sub_summary %>% 
  ungroup() %>% 
  dplyr::filter(!is.na(savings_loss)) %>% 
  dplyr::select(cost_per_volume) %>% 
  unlist() %>% 
  max() %>% 
  signif(1)
max_geom_text <- max_geom_text + .001

gg_cpv_sub_summary <- df_cpv_sub_summary %>% 
  ggplot(aes(x=month,
             fill=year,
             text=paste("Year:",year,
                        "<br>Total BE reject cost:",mycurrency(total_reject_cost_D),
                        "<br>Total subset vol fired:",mynumber(total_vol_fired_Y),
                        "<br>Cost per in3:",mycurrency(cost_per_volume),
                        "<br>Savings/Loss:",mycurrency(savings_loss)
             )
             
  ))+
  geom_bar(stat="identity",aes(y=cost_per_volume),
           position='dodge')+
  geom_text(data=df_cpv_sub_summary[df_cpv_sub_summary$year!=2019,],
            aes(label=mycurrency(savings_loss), 
                # fill=year,
                y=max_geom_text))+
  # y=cost_per_volume+.001))+
  theme_minimal()+
  labs(title = g.title,
       subtitle = g.subtitle,
       caption = g.caption)+
  scale_fill_brewer(name = "Year",
                    palette = "Paired")+
  scale_y_continuous(name="Cost per volume (USD per in3)",
                     labels = dollar_format(accuracy=.005),
                     breaks=seq(0,.03,.005))+
  xlab("Month")

gp_cpv_sub_summary <- ggplotly(gg_cpv_sub_summary, tooltip = c("text"))%>%
  layout(title = list(text = paste0(g.title,
                                    '<br>',
                                    '<sup>',
                                    g.subtitle,
                                    '</sup>')),
         margin=list(t = 75),
         annotations = 
           list(x = 1, y = -0.15, 
                text = g.caption, 
                showarrow = F, xref='paper', yref='paper', 
                xanchor='right', yanchor='auto', xshift=0, yshift=0,
                font=list(size=12, color="black"))
  )
# gp <- layout(gp, margin=list(t = 75))
# gp[['x']][['layout']][['annotations']][[2]][['x']] <- -0.06
# gp[['x']][['layout']][['annotations']][[1]][['y']] <- -0.1
# gp_cpv_sub_summary


# *table of savings/loss ----
dt_savings_loss <- df_cpv_sub_summary %>% 
  ungroup() %>% 
  na.omit() %>% 
  dplyr::select(month,savings_loss) %>% 
  set_colnames(c("Month","Profit/Loss"))
ytd_PL <- dt_savings_loss %>% 
  summarise("Profit/Loss" = sum(`Profit/Loss`))
ytd_PL <- tibble("Month"="YTD",
                 "Profit/Loss" = ytd_PL[[1]])
dt_savings_loss <- dt_savings_loss %>% 
  bind_rows(ytd_PL)

dt_dt_savings_loss <-
  datatable(dt_savings_loss,
          options = list(
            autoWidth = FALSE,
            dom = 't'
            # dom = 'tip'
          ),
          class = 'cell-border stripe',
          caption = "YTD profit/loss",
          rownames = FALSE) %>%
  formatCurrency(c(names(dt_savings_loss[2])), "$") %>%
  formatStyle(
    'Month',
    target = 'row',
    fontWeight = styleEqual("YTD",
                            "bold"),
    backgroundColor = styleEqual("YTD",
                                 "yellow")
  )
