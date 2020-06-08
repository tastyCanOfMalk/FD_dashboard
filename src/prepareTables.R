
source("src/BESummaryReport.R")

dt_yields <- df_yields[1:300, -c(2:4)] %>% 
  set_colnames(c(
    "Fire date",
    # "month",
    # "year",
    # "year_month",
    "LotNo",
    "Kiln",
    "Item",
    "Type",
    "EC",
    "PPI",
    "Composition",
    "Description",
    "Total Item Fired",
    "Total Item Rejected",
    "Total Item Pct Yield",
    "Cost/Pc",
    "Vol/Pc",
    "Total Item Vol Fired",
    "Total Item Vol Rejected"
  ))

sketch = htmltools::withTags(table(
  class = 'display',
  thead(
    tr(
      th(rowspan = 1, 'a'),
      th(rowspan = 1, 'b'),
      th(rowspan = 1, 'c'),
      th(colspan = 1, 'Sepal'),
      th(colspan = 1, 'Petal')
    ),
    tr(
      lapply(rep(c('Length', 'Width'), 2), th)
    )
  )
))

datatable(dt_yields,
          container = sketch,
          rownames = FALSE)

datatable(dt_yields,
          extensions = c('ColReorder', 'Buttons', 'Scroller'),
          rownames = FALSE,
          filter = 'top',
          options = list(
            dom = 'Bfrti',
            buttons = c('colvis', 'csv', 'excel'),
            colReorder = list(realtime = FALSE),
            scrollX = TRUE,
            scrollY = 600,
            scroller = TRUE,
            order = list(list(0, 'desc'))
          )
) %>%
  formatPercentage("Total Item Pct Yield", 2) %>%
  formatCurrency("Cost/Pc")



datatable(df_defects[1:300, -c(2:4)] %>%
            set_colnames(c(
              "Fire date",
              # "month",
              # "year",
              # "year_month",
              "LotNo",
              "Kiln",
              "Item",
              "Type",
              "EC",
              "PPI",
              "Composition",
              "Description",
              "Cause",
              "Cost/in3",
              "Reject vol single row",
              "Reject cost single row",
              "Total item fired vol",
              "Total item rejected vol",
              "Total item cost of rejects",
              "Vol/Pc",
              "Total item count fired",
              "Total item count rejected"
            )),
          extensions = c('ColReorder', 'Buttons', 'Scroller'),
          rownames = FALSE,
          filter = 'top',
          options = list(
            dom = 'Bfrti',
            buttons = c('colvis', 'csv', 'excel'),
            colReorder = list(realtime = FALSE),
            scrollX = TRUE,
            scrollY = 800,
            scroller = TRUE,
            order = list(list(0, 'desc'))
          )
)
# formatCurrency(c("Total item cost of rejects",
# "Reject cost single row"))


for(message in join_messages){
  print(paste(paste(message, '\n')))
}

paste(join_messages, collapse="\n")

mapply(paste, sep="\n", join_messages)
do.call("paste0", list(join_messages))