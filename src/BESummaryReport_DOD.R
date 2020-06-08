source("src/cleanCombine.R")
source("src/userFunctions.R")
# 
# if (!require("scales")) install.packages("scales")
# if (!require("plotly")) install.packages("plotly")
# if (!require("DT")) install.packages("DT")
# 
library(scales)
library(plotly)
library(DT)

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

# annual profit loss ------------------------------------------------------

# get defect cost per fired cost
defect_rate <- filtered_subset %>% 
  # total fired cost
  dplyr::filter(year == year_to_compare | 
                  year == year_current) %>% 
  group_by(ITEM, LOTNO, KILN) %>% slice(1) %>% ungroup() %>% 
  mutate(total_item_fired_cost = TOTAL_ITEM_FIRED_Y * cost_piece) %>% 
  group_by(year) %>% 
  dplyr::summarise(total_fired_cost = sum(total_item_fired_cost)) %>% 
  left_join(
    # total defect_selection cost
    filtered_subset %>% 
      dplyr::filter((year == year_to_compare | 
                      year == year_current) & 
                      CAUSE == defect_selection) %>% 
      group_by(year) %>% 
      dplyr::summarise(total_defect_cost = sum(reject_cost_single_row_D))
  ) %>% 
  mutate(defect_cost_per_total_fired_cost = total_defect_cost / total_fired_cost)
  
defect_rate_year_to_compare <- defect_rate[defect_rate$year == year_to_compare, "defect_cost_per_total_fired_cost"][[1]]

profit_loss_year_comparison <- defect_rate %>% 
  mutate(rate_compare = ifelse(year == year_current, defect_rate_year_to_compare, NA)) %>% 
  mutate(profit_loss = total_fired_cost * rate_compare - total_defect_cost)

dt_profit_loss <-
  DT::datatable(
    profit_loss_year_comparison %>%
      dplyr::select(-c(rate_compare)) %>%
      set_colnames(c(
        "Year",
        "Total fired cost",
        "Total defect cost",
        "Defect rate",
        "Profit / Loss"
      )),
    options = list(
      dom = 't'
    ),
    class = 'cell-border stripe',
    caption = paste0("Profit/Loss table for ", defect_selection, " defects"),
    rownames = FALSE) %>%
    formatCurrency(c("Total fired cost",
                     "Total defect cost",
                     "Profit / Loss"), "$") %>%
    formatPercentage("Defect rate",
                     digits = 2) %>%
    formatStyle(
      'Year',
      target = 'row',
      fontWeight = styleEqual(year_current,
                              "bold")
    )
  

# monthly profit loss -----------------------------------------------------

profit_loss_monthly <- filtered_subset %>% 
  # total fired cost
  dplyr::filter(year == year_current) %>% 
  group_by(ITEM, LOTNO, KILN) %>% slice(1) %>% ungroup() %>% 
  mutate(total_item_fired_cost = TOTAL_ITEM_FIRED_Y * cost_piece) %>% 
  group_by(year, month, year_month) %>% 
  dplyr::summarise(total_fired_cost = sum(total_item_fired_cost)) %>% 
  left_join(
    # total defect_selection cost
    filtered_subset %>% 
      dplyr::filter((year == year_to_compare | 
                       year == year_current) & 
                      CAUSE == defect_selection) %>% 
      group_by(year, month, year_month) %>% 
      dplyr::summarise(total_defect_cost = sum(reject_cost_single_row_D))
  ) %>% 
  mutate(defect_cost_per_total_fired_cost = total_defect_cost / total_fired_cost) %>% 
  mutate(rate_compare = defect_rate_year_to_compare) %>% 
  mutate(profit_loss = total_fired_cost * rate_compare - total_defect_cost) %>% 
  ungroup() %>% 
  dplyr::select(year_month, defect_cost_per_total_fired_cost, profit_loss)
  
dt_profit_loss_summary <- 
  DT::datatable(
    profit_loss_monthly %>% 
      bind_rows(
        profit_loss_year_comparison %>% 
          dplyr::select(year, defect_cost_per_total_fired_cost, profit_loss) %>% 
          dplyr::filter(year == year_current) %>% 
          set_colnames(c("year_month", "defect_cost_per_total_fired_cost", "profit_loss"))
      ) %>% 
      set_colnames(c(
        "Date",
        "Defect rate",
        "Profit / Loss"
      )),
    options = list(
      dom = 't',
      pageLength = 13
    ),
    class = 'cell-border stripe',
    caption = paste0("Monthly Profit/Loss table for ", defect_selection, " defects"),
    rownames = FALSE) %>% 
    formatCurrency(c("Profit / Loss"), "$") %>% 
    formatPercentage("Defect rate",
                     digits = 2) %>% 
      formatStyle(
        'Date',
        target = 'row',
        fontWeight = styleEqual(year_current,
                                "bold"),
        backgroundColor = styleEqual(year_current,
                                     "yellow")
      )

# info Boxes -------------------------------------------------------------------
# total profit loss
progress_profit_loss <- paste0("$",formatC(profit_loss_year_comparison[profit_loss_year_comparison$year==year_current,'profit_loss'][[1]], format="fg",big.mark=","))

# rate change vs years
year_current_rate <- defect_rate[defect_rate$year == year_current, "defect_cost_per_total_fired_cost"][[1]]

rate_change <- paste0(formatC(signif( 
  (year_current_rate - defect_rate_year_to_compare) / defect_rate_year_to_compare,
  3)*100, format="fg",big.mark=","), "%" )

# plots -------------------------------------------------------------------

# * monthly defect rate comparison ----
g.title =    paste0(defect_selection, " Defect Rate vs Month, ", year_to_compare, "-", year_current)
g.subtitle = paste0("Defect Rate = (Total ", defect_selection, " Defect Cost / Total Fired Cost) of subset")
g.caption =  ""

df_defect_rate_compare <- filtered_subset %>% 
  # total fired cost
  dplyr::filter(year == year_current | 
                  year == year_to_compare) %>% 
  group_by(ITEM, LOTNO, KILN) %>% slice(1) %>% ungroup() %>% 
  mutate(total_item_fired_cost = TOTAL_ITEM_FIRED_Y * cost_piece) %>% 
  group_by(year, month, year_month) %>% 
  dplyr::summarise(total_fired_cost = sum(total_item_fired_cost)) %>% 
  left_join(
    # total defect_selection cost
    filtered_subset %>% 
      dplyr::filter((year == year_to_compare | 
                       year == year_current) & 
                      CAUSE == defect_selection) %>% 
      group_by(year, month, year_month) %>% 
      dplyr::summarise(total_defect_cost = sum(reject_cost_single_row_D))
  ) %>% 
  mutate(defect_cost_per_total_fired_cost = total_defect_cost / total_fired_cost) %>% 
  left_join(
    profit_loss_year_comparison %>% 
      dplyr::select(year, defect_cost_per_total_fired_cost) %>% 
      set_colnames(c("year", "annual_rate"))
  )

gg_defect_rate_compare <- df_defect_rate_compare %>% 
  ggplot(aes(x = month, 
             fill = year,
             text = 
               paste(
                 "Date:", year_month,
                 "<br>Defect rate:", mypercent(defect_cost_per_total_fired_cost),
                 "<br>Total defect cost", mycurrency(total_defect_cost),
                 "<br>Total fired cost:", mycurrency(total_fired_cost),
                 "<br>Annual rate:", mypercent(annual_rate)
               )))+
  geom_bar(aes(y = defect_cost_per_total_fired_cost), 
           stat='identity',
           position = 'dodge')+
  scale_fill_brewer(name = "Monthly defect rate", palette = "Paired")+
  # geom_hline(aes(yintercept = annual_rate, linetype = year, alpha = .5))+
  # geom_hline(aes(yintercept = annual_rate), linetype = 'dotted', alpha=.16)+
  # geom_point(aes(y = annual_rate),
  #            shape = 20,
  #            size = 2,
  #            stroke = .1)+
  # geom_hline(aes(yintercept = annual_rate,
  #                color = year), 
  #            stroke = 'white',
  #            linetype='dashed')+
  scale_color_brewer(name = "Overall defect rate", palette = "Paired")+
  # geom_point(aes(y = defect_cost_per_total_fired_cost, color = year))+
  # geom_line(aes(y = defect_cost_per_total_fired_cost, group = year, color = year))+
  # geom_line(aes(y = annual_rate, color = year), linetype = 'dashed', size=2)+
  theme_minimal()+
  labs(title    = g.title,
       subtitle = g.subtitle,
       caption  = g.caption)+
  xlab("Month")+
  ylab("Defect Rate")+
  scale_y_continuous(breaks = seq(0, 
                                  round(max(df_defect_rate_compare$defect_cost_per_total_fired_cost),2),
                                  .01))

ggp_defect_rate_compare <- ggplotly(gg_defect_rate_compare, tooltip = c("text"))%>%
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


# * line  version ----------------------------------------------------------

g.title =    paste0(defect_selection, " Defect Rate vs Month, ", year_to_compare, "-", year_current)
g.subtitle = "Defect Rate = (Total Defect Cost / Total Fired Cost) of subset"
g.caption =  ""

# df_defect_rate_compare <- filtered_subset %>% 
#   # total fired cost
#   dplyr::filter(year == year_current | 
#                   year == year_to_compare) %>% 
#   group_by(ITEM, LOTNO, KILN) %>% slice(1) %>% ungroup() %>% 
#   mutate(total_item_fired_cost = TOTAL_ITEM_FIRED_Y * cost_piece) %>% 
#   group_by(year, month, year_month) %>% 
#   dplyr::summarise(total_fired_cost = sum(total_item_fired_cost)) %>% 
#   left_join(
#     # total defect_selection cost
#     filtered_subset %>% 
#       dplyr::filter((year == year_to_compare | 
#                        year == year_current) & 
#                       CAUSE == defect_selection) %>% 
#       group_by(year, month, year_month) %>% 
#       dplyr::summarise(total_defect_cost = sum(reject_cost_single_row_D))
#   ) %>% 
#   mutate(defect_cost_per_total_fired_cost = total_defect_cost / total_fired_cost) %>% 
#   left_join(
#     profit_loss %>% 
#       dplyr::select(year, defect_cost_per_total_fired_cost) %>% 
#       set_colnames(c("year", "annual_rate"))
#   )

gg_defect_rate_compare_line <- df_defect_rate_compare %>% 
  ungroup() %>% 
  ggplot(aes(x = month, 
             fill = year,
             text =
               paste(
                 "Date:", year_month,
                 "<br>Defect rate:", mypercent(defect_cost_per_total_fired_cost),
                 "<br>Total defect cost", mycurrency(total_defect_cost),
                 "<br>Total fired cost:", mycurrency(total_fired_cost),
                 "<br>Annual rate:", mypercent(annual_rate)
             )
         ))+
  geom_point(aes(y = defect_cost_per_total_fired_cost, 
                 color = year),
             shape = 1,
             size = 2)+
  geom_line(aes(y = defect_cost_per_total_fired_cost, 
                group = year,
                color = year))+
  scale_fill_brewer(name = "Monthly defect rate", palette = "Paired")+
  # geom_hline(aes(yintercept = annual_rate, linetype = year, alpha = .5))+
  geom_hline(aes(yintercept = annual_rate,
                 color = year), 
             linetype = 'dotted', 
             alpha=.6)+
  scale_color_brewer(name = "Overall defect rate", palette = "Paired")+
  theme_minimal()+
  labs(title    = g.title,
       subtitle = g.subtitle,
       caption  = g.caption)+
  xlab("Month")+
  ylab("Defect Rate")+
  scale_y_continuous(breaks = seq(0, 
                                  round(max(df_defect_rate_compare$defect_cost_per_total_fired_cost),2),
                                  .01))

ggp_defect_rate_compare_line <- ggplotly(gg_defect_rate_compare_line, tooltip = c("text"))%>%
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

# boxplot
g.title =    paste0(defect_selection, " Defect Rate,", year_to_compare, "vs", year_current)
g.subtitle = ""
g.caption =  ""

gg_t <- df_defect_rate_compare %>% 
  # dplyr::select(year, month, year_month, defect_cost_per_total_fired_cost) %>% 
  # set_colnames(c("Year", "Month", "Year.Month", "Defect.rate")) %>% 
  ggplot(aes(x = year, color = year,
             text = paste(
               "Date:", year_month,
               "<br>Defect rate:", mypercent(defect_cost_per_total_fired_cost)
             )))+
  geom_boxplot(aes(y=defect_cost_per_total_fired_cost),outlier.alpha = 0, size=.8)+
  geom_jitter(aes(y=defect_cost_per_total_fired_cost, fill=year),
              width=.2, 
              shape=21, 
              color='black',
              stroke=.2)+
  scale_color_brewer(name = "Year", palette="Paired")+
  scale_fill_brewer(palette="Paired")+
  theme_minimal()+
  labs(title    = g.title,
       subtitle = g.subtitle,
       caption  = g.caption)+
  xlab("Month")+
  scale_y_continuous(name="Defect rate",
                     labels = percent_format(accuracy=.1))

ggp <- ggplotly(gg_t, tooltip = c("text"))


ggp$x$data[[2]]$showlegend = FALSE
ggp
  

df_defect_rate_compare %>% 
  ggplot(aes(x=year))+
  
  ggdensity(aes(y=defect_cost_per_total_fired_cost))
  dplyr::select(year, month, defect_cost_per_total_fired_cost)
  ggplot(aes(x = year, fill = year))+
  geom_split_violin(aes(y=defect_cost_per_total_fired_cost))
  geom_boxplot(aes(y=defect_cost_per_total_fired_cost),outlier.alpha = 0)+
  geom_jitter(aes(y=defect_cost_per_total_fired_cost,fill=year),shape=21,color='red',stroke=1.5)+
  scale_fill_brewer(name = "Year", palette = "Paired")+
  scale_color_brewer(name = "Year", palette = "Paired")+
  theme_minimal()

  
  head(my_data)
  my_data <- data.frame(
    y = c(rnorm(1000), rnorm(1000, 0.5), rnorm(1000, 1), rnorm(1000, 1.5)),
    x = c(rep("a", 2000), rep("b", 2000)),
    m = c(rep("i", 1000), rep("j", 2000), rep("i", 1000))
  )
  
# filtered_subset %>% 
#   # total fired cost
#   dplyr::filter(year == year_current | 
#                   year == year_to_compare) %>% 
#   group_by(ITEM, LOTNO, KILN) %>% slice(1) %>% ungroup() %>% 
#   mutate(total_item_fired_cost = TOTAL_ITEM_FIRED_Y * cost_piece) %>% 
#   group_by(year, month, year_month) %>% 
#   dplyr::summarise(total_fired_cost = sum(total_item_fired_cost)) %>% 
#   left_join(
#     # total defect_selection cost
#     filtered_subset %>% 
#       dplyr::filter((year == year_to_compare | 
#                        year == year_current) & 
#                       CAUSE == defect_selection) %>% 
#       group_by(year, month, year_month) %>% 
#       dplyr::summarise(total_defect_cost = sum(reject_cost_single_row_D))
#   ) %>% 
#   mutate(defect_cost_per_total_fired_cost = total_defect_cost / total_fired_cost) %>% 
#   left_join(
#     profit_loss %>% 
#       dplyr::select(year, defect_cost_per_total_fired_cost) %>% 
#       set_colnames(c("year", "annual_rate"))
#   ) %>% 
#   ggplot(aes(x=month, fill = year))+
#   geom_bar(aes(y = defect_cost_per_total_fired_cost), 
#            stat='identity',
#            position = 'dodge')+
#   geom_point(aes(y = defect_cost_per_total_fired_cost, color = year))+
#   geom_line(aes(y = defect_cost_per_total_fired_cost, group = year, color = year))+
#   scale_fill_brewer(name = "Monthly defect rate", palette = "Paired")+
#   geom_line(aes(y = annual_rate,group=year, color = year), linetype = 'dashed')+
#   scale_color_brewer(name = "Overal defect rate", palette = "Set2")+
#   theme_minimal()
#   # geom_hline(yintercept = defect_rate_year_to_compare)
# 
# 


profit_loss$defect_cost_per_total_fired_cost

# dt_dt_savings_loss <-
#   datatable(dt_savings_loss,
#           options = list(
#             autoWidth = FALSE,
#             dom = 't'
#             # dom = 'tip'
#           ),
#           class = 'cell-border stripe',
#           caption = "YTD profit/loss",
#           rownames = FALSE) %>%
#   formatCurrency(c(names(dt_savings_loss[2])), "$") %>%
#   formatStyle(
#     'Month',
#     target = 'row',
#     fontWeight = styleEqual("YTD",
#                             "bold"),
#     backgroundColor = styleEqual("YTD",
#                                  "yellow")
#   )



# 
# 
# 
# df_sub
# 
# 
# df_cpv <- df_sub %>% 
#   dplyr::filter(
#     CAUSE == "BE",
#     year(FIRE_DATE) >= 2019
#     ) %>% 
#   group_by(year_month, year, month) %>% 
#   dplyr::summarise(total_reject_cost_D = sum(reject_cost_single_row_D)) %>% 
#   left_join(
#     df_sub %>% 
#       group_by(ITEM, LOTNO, KILN) %>% slice(1) %>% ungroup() %>% 
#       dplyr::filter(
#         year(FIRE_DATE) >= 2019
#         ) %>% 
#       group_by(year_month, year, month) %>% 
#       dplyr::summarise(total_vol_fired_Y = sum(total_item_vol_fired_Y))
#   ) %>% 
#   mutate(cost_per_volume = total_reject_cost_D / total_vol_fired_Y)
# 
# gg_cpv <- df_cpv %>% 
#   ggplot(aes(x=month, 
#              y=cost_per_volume, 
#              fill=year,
#              text = 
#                paste(
#                  "Year:", year,
#                  "<br>Total BE reject cost:", mynumber(total_reject_cost_D),
#                  "<br>Total subset vol fired:", mynumber(total_vol_fired_Y),
#                  "<br>Cost per in3:", mycurrency(cost_per_volume)
#                )))+
#   geom_bar(stat='identity',
#            position = 'dodge')+
#   scale_fill_brewer(name = "Year", palette = "Paired")+
#   scale_y_continuous(name = "Cost per volume (USD per in3)",
#                      labels = dollar_format(accuracy = .001),
#                      breaks = seq(0,.3,.005))+
#   theme_minimal()+
#   labs(title    = g.title,
#        subtitle = g.subtitle,
#        caption  = g.caption)+
#   xlab("Month")
#   
# gp_cpv <- ggplotly(gg_cpv, tooltip = c("text"))%>%
#   layout(title = list(text = paste0(g.title,
#                                     '<br>',
#                                     '<sup>',
#                                     g.subtitle,
#                                     '</sup>')),
#          margin=list(t = 75),
#          annotations = 
#            list(x = 1, y = -0.15, 
#                 text = g.caption, 
#                 showarrow = F, xref='paper', yref='paper', 
#                 xanchor='right', yanchor='auto', xshift=0, yshift=0,
#                 font=list(size=12, color="black"))
#   )
# 
# # *savings-loss ---- 
# g.title =    "Cost per volume by month, with estimted savings"
# g.subtitle = "S/L = (Current fired volume * Last years CPV) - Current reject cost"
# g.caption =  "From merged dataset"
# 
# # filter for months with 2 rows of data
# months <- data.frame(summary(df_cpv$month)>1) %>% 
#   mutate(month = rownames(data.frame(summary(df_cpv$month)>1))) %>% 
#   set_colnames(c(
#     "tf", "month"
#   )) %>% 
#   dplyr::filter(tf) %>% 
#   dplyr::select(month)
# 
# keep_months <- apply(months, 1, paste, collapse="")
# df_cpv_sub <- df_cpv[df_cpv$month %in% keep_months,]
# 
# df_cpv_sub_2019 <- df_cpv_sub %>% 
#   ungroup() %>% 
#   dplyr::filter(year == 2019) %>%
#   dplyr::select(-c(
#     year_month,
#     year
#   )) %>% 
#   set_colnames(c(
#     # "year_month",
#     # "year",
#     "month",
#     "total_reject_cost_D_2019",
#     "total_vol_fired_Y_2019",
#     "cost_per_volume_2019"))
# 
# df_cpv_sub_summary <- df_cpv_sub %>% 
#   left_join(df_cpv_sub_2019) %>% 
#   mutate(savings_loss = 
#            ifelse(year == 2020,
#                   (total_vol_fired_Y * cost_per_volume_2019) - total_reject_cost_D,
#                   NA
#                   ))
# 
# # get max CPV value for geom_text
# max_geom_text <- df_cpv_sub_summary %>% 
#   ungroup() %>% 
#   dplyr::filter(!is.na(savings_loss)) %>% 
#   dplyr::select(cost_per_volume) %>% 
#   unlist() %>% 
#   max() %>% 
#   signif(1)
# max_geom_text <- max_geom_text + .001
# 
# gg_cpv_sub_summary <- df_cpv_sub_summary %>% 
#   ggplot(aes(x=month,
#              fill=year,
#              text=paste("Year:",year,
#                         "<br>Total BE reject cost:",mycurrency(total_reject_cost_D),
#                         "<br>Total subset vol fired:",mynumber(total_vol_fired_Y),
#                         "<br>Cost per in3:",mycurrency(cost_per_volume),
#                         "<br>Savings/Loss:",mycurrency(savings_loss)
#              )
#              
#   ))+
#   geom_bar(stat="identity",aes(y=cost_per_volume),
#            position='dodge')+
#   geom_text(data=df_cpv_sub_summary[df_cpv_sub_summary$year!=2019,],
#             aes(label=mycurrency(savings_loss), 
#                 # fill=year,
#                 y=max_geom_text))+
#   # y=cost_per_volume+.001))+
#   theme_minimal()+
#   labs(title = g.title,
#        subtitle = g.subtitle,
#        caption = g.caption)+
#   scale_fill_brewer(name = "Year",
#                     palette = "Paired")+
#   scale_y_continuous(name="Cost per volume (USD per in3)",
#                      labels = dollar_format(accuracy=.005),
#                      breaks=seq(0,.03,.005))+
#   xlab("Month")
# 
# gp_cpv_sub_summary <- ggplotly(gg_cpv_sub_summary, tooltip = c("text"))%>%
#   layout(title = list(text = paste0(g.title,
#                                     '<br>',
#                                     '<sup>',
#                                     g.subtitle,
#                                     '</sup>')),
#          margin=list(t = 75),
#          annotations = 
#            list(x = 1, y = -0.15, 
#                 text = g.caption, 
#                 showarrow = F, xref='paper', yref='paper', 
#                 xanchor='right', yanchor='auto', xshift=0, yshift=0,
#                 font=list(size=12, color="black"))
#   )
# # gp <- layout(gp, margin=list(t = 75))
# # gp[['x']][['layout']][['annotations']][[2]][['x']] <- -0.06
# # gp[['x']][['layout']][['annotations']][[1]][['y']] <- -0.1
# # gp_cpv_sub_summary
# 
# 
# # *table of savings/loss ----
# dt_savings_loss <- df_cpv_sub_summary %>% 
#   ungroup() %>% 
#   na.omit() %>% 
#   dplyr::select(month,savings_loss) %>% 
#   set_colnames(c("Month","Profit/Loss"))
# ytd_PL <- dt_savings_loss %>% 
#   summarise("Profit/Loss" = sum(`Profit/Loss`))
# ytd_PL <- tibble("Month"="YTD",
#                  "Profit/Loss" = ytd_PL[[1]])
# dt_savings_loss <- dt_savings_loss %>% 
#   bind_rows(ytd_PL)
# 
# dt_dt_savings_loss <-
#   datatable(dt_savings_loss,
#           options = list(
#             autoWidth = FALSE,
#             dom = 't'
#             # dom = 'tip'
#           ),
#           class = 'cell-border stripe',
#           caption = "YTD profit/loss",
#           rownames = FALSE) %>%
#   formatCurrency(c(names(dt_savings_loss[2])), "$") %>%
#   formatStyle(
#     'Month',
#     target = 'row',
#     fontWeight = styleEqual("YTD",
#                             "bold"),
#     backgroundColor = styleEqual("YTD",
#                                  "yellow")
#   )
