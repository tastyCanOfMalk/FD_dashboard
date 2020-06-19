get_files <- function(x){
  # load csv files
  df <- read_csv(x) %>% clean_names()
}

clean_column_names <- function(x){
  # cleans column names
  names <- toupper(colnames(x))
  names <- str_replace_all(names, "%", "PCT")
  names <- str_replace_all(names, "/", "_")
  names <- str_replace_all(names, " ", "_")
  names <- str_replace_all(names, "___", "_")
  names <- str_replace_all(names, "[.]", "")
  colnames(x) <- names
  
  return(x)
}

replace_pr1_with_pr6 <- function(df){
  # replace PR1 codes with PR6 codes where available
  # load index file
  pr1_pr6_index <- read_csv(pr1_pr6_lookup) %>%
    set_colnames(c('PR1',
                   'PR6',
                   'DESCRIPTION')) %>%
    mutate(PR1 = gsub("[^0-9]", "", PR1),
           PR6 = gsub("[^0-9]", "", PR6),
           DESCRIPTION = toupper(DESCRIPTION))
  # join by PR1 codes
  new_df <- df %>% 
    left_join(pr1_pr6_index[-3], by = c("ITEM" = "PR1")) %>% 
    mutate(
      ITEM = ifelse( is.na(PR6), ITEM, PR6)
    ) %>% 
    dplyr::select(-PR6)
  
  return(new_df)
}

replace_cost_vol <- function(df){
  # replace old values with new values, where possible
  # load index file
  cost_vol_piece_index <- read_csv(vol_piece_lookup) %>%
    set_colnames(c('ITEM',
                   'DESCRIPTION',
                   'VOL_PIECE_NEW',
                   'COST_PIECE_NEW')) %>% 
    mutate(
      ITEM           = as.character(ITEM),
      COST_PIECE_NEW = as.numeric(gsub("[^0-9\\.]", "", COST_PIECE_NEW))
    )
  # use index value where difference between new and old values are > 0, if no match, use old value
  new_df <- df %>% 
    left_join(cost_vol_piece_index[-2], by = "ITEM") %>% 
    mutate(
      vol_diff  = abs(VOL_PIECE - VOL_PIECE_NEW),
      vol_piece = ifelse( is.na(VOL_PIECE_NEW), 
                          VOL_PIECE,
                          ifelse( vol_diff > 0,
                                  VOL_PIECE_NEW,
                                  VOL_PIECE) 
                          ),
      cost_diff = abs(COST_PIECE - COST_PIECE_NEW),
      cost_piece = ifelse( is.na(COST_PIECE_NEW), 
                           COST_PIECE,
                           ifelse( cost_diff > 0,
                                   COST_PIECE_NEW,
                                   COST_PIECE) 
                           )
    ) %>% 
    dplyr::select(-c(
      COST_PIECE,
      COST_PIECE_NEW,
      cost_diff,
      VOL_PIECE,
      VOL_PIECE_NEW,
      vol_diff
    ))
  
  return(new_df)
}

clean_composition <- function(df){
  
  new_df <- df[complete.cases(df$COMPOSITION),]

  new_df[new_df$COMPOSITION == "PSZT FBG",]$COMPOSITION = "PSZT-FBG"
  new_df[new_df$COMPOSITION == "PSZ-M-ALT",]$COMPOSITION = "PSZM-ALT"
  new_df[new_df$COMPOSITION == "PSZ-M ALT",]$COMPOSITION = "PSZM-ALT"
  
  return(new_df)

  # join duplicate composition values
  # new_df <- df %>%
  #   mutate_if(is.factor, as.character) %>%
  #   mutate(
  #     COMPOSITION = ifelse( COMPOSITION == "PSZ-M ALT",
  #                           "PSZ-M-ALT",
  #                           COMPOSITION),
  #     COMPOSITION = ifelse( COMPOSITION == "PSZT FBG",
  #                           "PSZT-FBG",
  #                           COMPOSITION))
    # mutate_if(is.character, as.factor)
  # 
  # return(new_df)
  }

create_year_month_cols <- function(df){
  # create and order month, year, year-month columns
  df_new <- df %>% 
    mutate(
      month      = as.factor(as.character(lubridate::month(FIRE_DATE, 
                                                           label = TRUE,
                                                           abbr = TRUE))),
      year       = as.factor(lubridate::year(FIRE_DATE)),
      year_month = as.factor(paste0(year, "-", month))
      )
  
  # reorder levels
  df_new$month <- factor(df_new$month, 
                         levels = c('Jan','Feb','Mar','Apr',
                                    'May','Jun','Jul','Aug',
                                    'Sep','Oct','Nov','Dec'))
  
  ym_levels <- c("2015-Jan", "2015-Feb", "2015-Mar", "2015-Apr", 
                 "2015-May", "2015-Jun", "2015-Jul", "2015-Aug", 
                 "2015-Sep", "2015-Oct", "2015-Nov", "2015-Dec", 
                 "2016-Jan", "2016-Feb", "2016-Mar", "2016-Apr", 
                 "2016-May", "2016-Jun", "2016-Jul", "2016-Aug", 
                 "2016-Sep", "2016-Oct", "2016-Nov", "2016-Dec", 
                 "2017-Jan", "2017-Feb", "2017-Mar", "2017-Apr", 
                 "2017-May", "2017-Jun", "2017-Jul", "2017-Aug", 
                 "2017-Sep", "2017-Oct", "2017-Nov", "2017-Dec", 
                 "2018-Jan", "2018-Feb", "2018-Mar", "2018-Apr", 
                 "2018-May", "2018-Jun", "2018-Jul", "2018-Aug", 
                 "2018-Sep", "2018-Oct", "2018-Nov", "2018-Dec", 
                 "2019-Jan", "2019-Feb", "2019-Mar", "2019-Apr", 
                 "2019-May", "2019-Jun", "2019-Jul", "2019-Aug", 
                 "2019-Sep", "2019-Oct", "2019-Nov", "2019-Dec", 
                 "2020-Jan", "2020-Feb", "2020-Mar", "2020-Apr", 
                 "2020-May", "2020-Jun", "2020-Jul", "2020-Aug", 
                 "2020-Sep", "2020-Oct", "2020-Nov", "2020-Dec")
  
  df_new$year_month = factor(df_new$year_month, levels = ym_levels)
  
  return(df_new)
}

mycurrency <- function(x){
  return(paste0("$",formatC(signif(x,3), format="fg",big.mark=",")))
}

mypercent <- function(x){
  return(paste0(formatC(signif(x,3)*100, format="fg",big.mark=","), "%" ))
}

mynumber <- function(x){
  return(paste0(formatC(signif(x,3), format="fg",big.mark=",")))
}









get_levels <- function(df, col){
  # checks number of levels for each column
  x.levels <- cbind(colnames(df),
                    (as.data.frame(sapply(df,function(x) length(unique(x)))))
  )
  colnames(x.levels) <- c("var","levels")
  row.names(x.levels) <- NULL
  levels <- x.levels[order(-x.levels[,2]),]
  return(levels[col,])
}

completeFun <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}

addSeasons <- function(df){
  # add season column
  spring = c('Mar', 'Apr', 'May','March','April')
  summer = c('Jun', 'Jul', 'Aug', 'June','July','August')
  fall   = c('Sep', 'Oct', 'Nov','September','October','November')
  winter = c('Dec', 'Jan', 'Feb','December','January','February')
  
  df <- df %>% mutate("season" = ifelse(month %in% spring, 'spring',
                                  ifelse(month %in% summer, 'summer',
                                         ifelse(month %in% fall, 'fall',
                                                ifelse(month %in% winter, 'winter', NA)))))
  
  df$season <- factor(df$season, levels=c('winter', 'spring',
                                          'summer', 'fall'))
  return(df)
}


# geom_split_violin ----
GeomSplitViolin <- ggproto(
  "GeomSplitViolin",
  GeomViolin,
  draw_group = function(self, data, ..., draw_quantiles = NULL) {
    data <- transform(data,
                      xminv = x - violinwidth * (x - xmin),
                      xmaxv = x + violinwidth * (xmax - x))
    grp <- data[1,'group']
    newdata <- plyr::arrange(
      transform(data, x = if(grp%%2==1) xminv else xmaxv),
      if(grp%%2==1) y else -y
    )
    newdata <- rbind(newdata[1, ], newdata, newdata[nrow(newdata), ], newdata[1, ])
    newdata[c(1,nrow(newdata)-1,nrow(newdata)), 'x'] <- round(newdata[1, 'x'])
    if (length(draw_quantiles) > 0 & !scales::zero_range(range(data$y))) {
      stopifnot(all(draw_quantiles >= 0), all(draw_quantiles <= 1))
      quantiles <- ggplot2:::create_quantile_segment_frame(data, draw_quantiles)
      aesthetics <- data[rep(1, nrow(quantiles)), setdiff(names(data), c("x", "y")), drop = FALSE]
      aesthetics$alpha <- rep(1, nrow(quantiles))
      both <- cbind(quantiles, aesthetics)
      quantile_grob <- GeomPath$draw_panel(both, ...)
      ggplot2:::ggname("geom_split_violin",
                       grid::grobTree(GeomPolygon$draw_panel(newdata, ...), quantile_grob))
    } else {
      ggplot2:::ggname("geom_split_violin", GeomPolygon$draw_panel(newdata, ...))
    }
  }
)

geom_split_violin <- function (mapping = NULL,
                               data = NULL,
                               stat = "ydensity",
                               position = "identity", ...,
                               draw_quantiles = NULL,
                               trim = TRUE,
                               scale = "area",
                               na.rm = FALSE,
                               show.legend = NA,
                               inherit.aes = TRUE) {
  layer(data = data,
        mapping = mapping,
        stat = stat,
        geom = GeomSplitViolin,
        position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list(trim = trim,
                      scale = scale,
                      draw_quantiles = draw_quantiles,
                      na.rm = na.rm, ...)
  )
}

