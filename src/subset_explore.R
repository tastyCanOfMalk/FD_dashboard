if (!require(plyr)) install.packages("plyr")
library(plyr)
if (!require(magrittr)) install.packages("magrittr")
library(magrittr)
# if (!require(kableExtra)) install.packages("kableExtra")
# library(kableExtra)
if (!require(plotly)) install.packages("plotly")
library(plotly)
# if (!require(lubridate)) install.packages("lubridate")
# library(lubridate)
if (!require(scales)) install.packages("scales")
library(scales)
# if (!require(gridExtra)) install.packages("gridExtra")
# library(gridExtra)
# if (!require(corrplot)) install.packages("corrplot")
# library(corrplot)
# if (!require(ggcorrplot)) install.packages("ggcorrplot")
# library(ggcorrplot)
# if (!require(naniar)) install.packages("naniar")
# library(naniar)
# if (!require(jtools)) install.packages("jtools")
# library(jtools)
# if (!require(ggpointdensity)) install.packages("ggpointdensity")
# library(ggpointdensity)
# if (!require(ggridges)) install.packages("ggridges")
# library(ggridges)
if (!require(viridis)) install.packages("viridis")
library(viridis)
if (!require(DT)) install.packages("DT")
library(DT)
if (!require(ggrepel)) install.packages("ggrepel")
library(ggrepel)
# if (!require(ggcorrplot)) install.packages("ggcorrplot")
# library(ggcorrplot)

# **Carpenter filter** 
# 
#   * item: 3531970631
#   * item: 853852
#   * description: 215.5MMBODX38MM,45PPI,PSZM,FEC,1/8"FG

gg_costly <- df_merged %>% 
  dplyr::filter(CAUSE == "CW") %>%
  dplyr::group_by(DESCRIPTION) %>% 
  dplyr::summarise(total_cost = sum(reject_cost_single_row_D)) %>% 
  dplyr::arrange(-total_cost) %>% 
  dplyr::slice(1:20) %>% 
  
  ggplot(aes(x=reorder(DESCRIPTION,total_cost), y=total_cost))+
  geom_point(color="tomato", size=3, fill = alpha("orange", .8), alpha=.9, shape=21, stroke=.5) +
  geom_segment(aes(x=DESCRIPTION, xend=DESCRIPTION, y=0, yend=total_cost), color="grey70")+
  theme_minimal()+
  theme(panel.grid.major.y = element_blank())+
  scale_y_continuous(limits = c(0,90000),
                     breaks = seq(0,90000,10000),
                     label = dollar_format(suffix="K", scale= 1e-3 ))+
  xlab("Item description")+
  ylab("Total cost USD")+
  ggtitle("Top 20 most costly items")+
  labs(subtitle = "Cracked webs only",
       caption = "All Kilns, 2015-20")+
  coord_flip()

ggplotly(gg_costly) %>% 
  layout(title = list(text = paste0('Top 20 most costly items',
                                    '<br>',
                                    '<sup>',
                                    'Cracked webs only, all kilns, 2015-20',
                                    '</sup>')))
gg_frequent <- df_merged %>% 
  dplyr::filter(cause == "CW") %>%
  dplyr::group_by(description) %>% 
  dplyr::summarise(total_count = sum(reject_count_single_row)) %>% 
  dplyr::arrange(-total_count) %>% 
  dplyr::slice(1:20) %>% 
  
  ggplot(aes(x=reorder(description,total_count), y=total_count))+
  geom_point(color="tomato", size=3, fill = alpha("orange", .8), alpha=.9, shape=21, stroke=.5) +
  geom_segment(aes(x=description, xend=description, y=0, yend=total_count), color="grey70")+
  theme_minimal()+
  theme(panel.grid.major.y = element_blank())+
  scale_y_continuous(limits = c(0,3750),
                     breaks = seq(0,3750,500))+
  xlab("Item description")+
  ylab("Number of CW defects")+
  ggtitle("Top 20 highest count items")+
  labs(subtitle = "Cracked webs only",
       caption = "All Kilns, 2015-20")+
  coord_flip()

ggplotly(gg_frequent) %>% 
  layout(title = list(text = paste0('Top 20 highest count items',
                                    '<br>',
                                    '<sup>',
                                    'Cracked webs only, all kilns, 2015-20',
                                    '</sup>')))

## table combining costs and counts
# total count of rejects
rej_count <- df_merged %>% 
  dplyr::filter(cause == "CW") %>%
  dplyr::group_by(description) %>% 
  dplyr::summarise(total_count = round(sum(reject_count_single_row),0)) %>%
  dplyr::arrange(-total_count)

# total cost of rejects
rej_cost <- df_merged %>% 
  dplyr::filter(cause == "CW") %>%
  dplyr::group_by(description) %>% 
  dplyr::summarise(total_cost = round(sum(cost_of_reject_single_row),2)) %>%
  dplyr::arrange(-total_cost)

# join
rej_cost_count <- full_join(rej_count, rej_cost, by="description") %>% 
  dplyr::mutate(cost_per_item = round(total_cost/total_count,2)) %>% 
  dplyr::arrange(-total_cost) %>% 
  magrittr::set_colnames(c("Description", "Total count", "Total cost", "Cost per item"))

## filtering
# top costly items
costly <- df_merged %>% 
  dplyr::filter(cause == "CW") %>%
  dplyr::group_by(description) %>% 
  dplyr::summarise(total_cost = sum(cost_of_reject_single_row)) %>% 
  dplyr::arrange(-total_cost) %>% 
  dplyr::slice(1:80)

# top frequency items
countly <- df_merged %>% 
  dplyr::filter(cause == "CW") %>%
  dplyr::group_by(description) %>% 
  dplyr::summarise(total_count = sum(reject_count_single_row)) %>% 
  dplyr::arrange(-total_count) %>% 
  dplyr::slice(1:80)

# get names that appear in both
in_both <- countly[countly$description %in% costly[[1]] ,]

# get statistics for the above names
in_both_stats <- rej_cost_count[rej_cost_count$Description %in% in_both[[1]],] %>% 
  set_colnames(c("description", "total_count", "total_cost", "cost_per_item"))

# plot
in_both_stats %>%
  arrange(desc(cost_per_item)) %>% 
  ggplot(aes(x=total_count, y=total_cost, size=cost_per_item)) +
  geom_point(alpha=.6,shape=21,color='dodgerblue', stroke=.2, aes(fill=cost_per_item))+
  geom_label_repel(aes(label=description), data=subset(in_both_stats, total_cost>15000),
                  size=3,
                  box.padding = .2,
                  point.padding = 0,
                  nudge_y = 10000,
                  nudge_x = 500000,
                  segment.color="grey50")+
  theme_minimal()+
  scale_size(guide='none')+
  scale_y_continuous(name = "Total cost (USD)",
                     limits = c(0,90000),
                     breaks = seq(0,90000,10000),
                     label = dollar_format(suffix="K", scale= 1e-3 ))+
  scale_x_continuous(name = "Total count",
                     limits = c(0,5000),
                     breaks = seq(0,5000,500),
                     label = unit_format(suffix="K", scale= 1e-3,accuracy=.5 ))+
  labs(title = "Items by total reject cost vs count",
       subtitle = "Cracked web rejects only, all kilns, 2015-2020",
       caption = "Size relative to cost per item; labels shown if total cost value > 15k")+
  scale_fill_viridis()

datatable(rej_cost_count,
          class = 'cell-border stripe',
          rownames = FALSE,
          filter = 'top',
          # caption = "",
          options = list(autoWidth = TRUE)) %>%
  formatCurrency(c('Total cost','Cost per item'), '$')
