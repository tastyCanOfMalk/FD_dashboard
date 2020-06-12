# get most costly items between time periods in subset
# get most frequent items between time periods in subset
# most costly subsets of filters? 

source("src/cleanCombine.R")
source("src/userFunctions.R")

library(scales)
library(plotly)

# subset selection --------------------------------------------------------
select_date_from <- as.Date("2015-01-01")
select_date_to <- today()
select_EC <- "None"
select_PPI <- c("10","15","20","30","3d","40","45","5","50","65","80")
select_COMPOSITION <- c("PSZT", "PSZT-FBG")
select_KILN <- c("A","AR","B","BR","C","CR","D","DR","E","ER","F","FR","G","GR","H","HR","U")

# entire filtered subset
filtered_subset <- df_merged %>%
  dplyr::filter(
    (FIRE_DATE >= select_date_from & FIRE_DATE <= select_date_to) &
    EC %in% select_EC &
    PPI %in% select_PPI &
    COMPOSITION %in% select_COMPOSITION & 
    KILN %in% select_KILN
    )

# top costly items --------------------------------------------------------
top_values_to_keep <- 20

df_top_costly <- filtered_subset %>% 
  group_by(DESCRIPTION, ITEM) %>% 
  dplyr::summarise(total_cost = sum(reject_cost_single_row_D)) %>% 
  ungroup() %>% 
  arrange(-total_cost) %>% 
  slice(1:top_values_to_keep)
  
x_breaks <- signif(ceiling(max(df_top_costly$total_cost, na.rm=TRUE)),1)/5
x_limit <- x_breaks * 7

g.title =    paste0("Top ", top_values_to_keep, " costly rejected items in selected subset")
g.subtitle = paste0("(", select_date_from, ") to (", select_date_to, ")")
g.caption =  "caption"

gg_top_costly <- df_top_costly %>% 
  ggplot(aes(x=reorder(DESCRIPTION,total_cost), 
             y=total_cost,
             text = paste(
               "Description:", DESCRIPTION,
               "<br>Item:", ITEM,
               "<br>Total cost:", mycurrency(total_cost)
               )
             ))+
  geom_point(color="tomato", size=3, fill = alpha("orange", .8), alpha=.9, shape=21, stroke=.5) +
  geom_segment(aes(x=DESCRIPTION, xend=DESCRIPTION, y=0, yend=total_cost), color="grey70")+
  theme_minimal()+
  theme(panel.grid.major.y = element_blank())+
  scale_y_continuous(limits = c(0,x_limit),
                     breaks = seq(0,x_limit,x_breaks),
                     label = dollar_format(suffix="K", scale= 1e-3 ))+
  xlab("Item description")+
  ylab("Rejected item cost (USD)")+
  labs(title    = g.title,
       subtitle = g.subtitle,
       caption  = g.caption)+
  coord_flip()

ggp_top_costly <- ggplotly(gg_top_costly, tooltip = c("text")) %>%
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
  
ggp_top_costly

# top frequent items --------------------------------------------------------
top_values_to_keep <- 20

df_top_frequent <- filtered_subset %>% 
  group_by(DESCRIPTION, ITEM) %>% 
  dplyr::summarise(total_rejected = sum(total_item_count_rejected_D)) %>% 
  ungroup() %>% 
  arrange(-total_rejected) %>% 
  slice(1:top_values_to_keep)
  
x_breaks <- signif(ceiling(max(df_top_frequent$total_rejected, na.rm=TRUE)),1)/5
x_limit <- x_breaks * 7

g.title =    paste0("Top ", top_values_to_keep, " frequently rejected items in selected subset")
g.subtitle = paste0("(", select_date_from, ") to (", select_date_to, ")")
g.caption =  "caption"

gg_top_frequent <- df_top_frequent %>% 
  ggplot(aes(x=reorder(DESCRIPTION,total_rejected), 
             y=total_rejected,
             text = paste(
               "Description:", DESCRIPTION,
               "<br>Item:", ITEM,
               "<br>Total rejected:", mynumber(total_rejected)
               )
             ))+
  geom_point(color="tomato", size=3, fill = alpha("orange", .8), alpha=.9, shape=21, stroke=.5) +
  geom_segment(aes(x=DESCRIPTION, xend=DESCRIPTION, y=0, yend=total_rejected), color="grey70")+
  theme_minimal()+
  theme(panel.grid.major.y = element_blank())+
  scale_y_continuous(limits = c(0,x_limit),
                     breaks = seq(0,x_limit,x_breaks),
                     label = dollar_format(suffix="K", scale= 1e-3 ))+
  xlab("Item description")+
  ylab("Rejected item count")+
  labs(title    = g.title,
       subtitle = g.subtitle,
       caption  = g.caption)+
  coord_flip()

ggp_top_frequent <- ggplotly(gg_top_frequent, tooltip = c("text")) %>%
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
  
ggp_top_frequent


# cost vs frequency -------------------------------------------------------

t <- filtered_subset %>% 
  group_by(DESCRIPTION, ITEM) %>% 
  dplyr::summarise(total_count = sum(total_item_count_rejected_D),
                   total_cost  = sum(reject_cost_single_row_D),
                   cost_pc     = mean(cost_piece),
                   cost_per_count = total_cost/total_count) %>%
  ungroup() %>% 
  ggplot(aes(x=total_count,
             y=total_cost,
             size=cost_pc,
             text = paste(
               "Description:",DESCRIPTION,
               "<br>Item:",ITEM,
               "<br>Total cost:",mycurrency(total_cost),
               "<br>Total count:",mynumber(total_count)
             )))+
  geom_point(alpha=.5,shape=21,color='dodgerblue', stroke=.2, aes(fill=cost_pc))+
  scale_fill_viridis()
ggplotly(t, tooltip = c("text"))
  geom_label_repel(aes(label=description), data=subset(in_both_stats, total_cost>15000),
                   size=3,
                   box.padding = .2,
                   point.padding = 0,
                   nudge_y = 10000,
                   nudge_x = 500000,
                   segment.color="grey50")+
  
  
  


