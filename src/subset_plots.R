# get most costly items between time periods in subset
# get most frequent items between time periods in subset

# most costly subsets of filters? 

# date_from  <- as.Date("2015-12-01")
# date_to    <- as.Date(today())
# defect_selection <- "CW"
# 
# filtered_subset_2 <- df_merged %>% 
#   dplyr::filter(
#     (FIRE_DATE >= date_from & FIRE_DATE <= date_to)
#   )

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

# get top costly items in subset
top_values_to_keep <- 20

df_top_costly <- filtered_subset %>% 
  group_by(DESCRIPTION) %>% 
  dplyr::summarise(total_cost = sum(reject_cost_single_row_D)) %>% 
  arrange(-total_cost) %>% 
  slice(1:top_values_to_keep)
  
x_breaks <- signif(ceiling(max(df_top_costly$total_cost, na.rm=TRUE)),1)/5
x_limit <- x_breaks * 7

g.title =    "title"
g.subtitle = "subtitle"
g.caption =  "caption"

df_top_costly %>% 
  ggplot(aes(x=reorder(DESCRIPTION,total_cost), y=total_cost))+
  geom_point(color="tomato", size=3, fill = alpha("orange", .8), alpha=.9, shape=21, stroke=.5) +
  geom_segment(aes(x=DESCRIPTION, xend=DESCRIPTION, y=0, yend=total_cost), color="grey70")+
  theme_minimal()+
  theme(panel.grid.major.y = element_blank())+
  scale_y_continuous(limits = c(0,x_limit),
                     breaks = seq(0,x_limit,x_breaks),
                     label = dollar_format(suffix="K", scale= 1e-3 ))+
  # xlab("Item description")+
  # ylab("Total cost USD")+
  # ggtitle("Top 20 most costly items")+
  # labs(subtitle = "Cracked webs only",
  #      caption = "All Kilns, 2015-20")+
  coord_flip()
  
  



year_to_compare  <- 2019
year_current     <- 2020
defect_selection <- "SBS"

filtered_subset <- df_merged %>% 
  dplyr::filter(
    PPI != "3d" &
      EC == "None" & 
      (COMPOSITION == "PSZT" | COMPOSITION == "PSZT-FBG")
  )


gg_costly <- filtered_subset %>% 
  dplyr::filter(cause == defect_selection) %>%
  dplyr::group_by(description) %>% 
  dplyr::summarise(total_cost = sum(cost_of_reject_single_row)) %>% 
  dplyr::arrange(-total_cost) %>% 
  dplyr::slice(1:20) %>% 
  
  ggplot(aes(x=reorder(description,total_cost), y=total_cost))+
  geom_point(color="tomato", size=3, fill = alpha("orange", .8), alpha=.9, shape=21, stroke=.5) +
  geom_segment(aes(x=description, xend=description, y=0, yend=total_cost), color="grey70")+
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



filtered_subset_2 %>% 
  dplyr::filter(CAUSE == defect_selection) %>% 
  group_by(DESCRIPTION) %>% 
  dplyr::summarise(total_count = sum(total_item_count_rejected_D),
                   total_cost = sum(reject_cost_single_row_D),
                   cost_piece = mean(cost_piece)) %>% 
  ggplot(aes(x=total_count, y=total_cost))+
  geom_point(aes(size=cost_piece))


defect_selection <- "SBS"

filtered_subset <- df_merged %>% 
  dplyr::filter(
    PPI != "3d" &
      EC == "None" & 
      (COMPOSITION == "PSZT" | COMPOSITION == "PSZT-FBG")
  )





df_merged


