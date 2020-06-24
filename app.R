options(java.parameters = "-Xmx8g")

source("src/cleanCombine.R")
source("src/userFunctions.R")


library(plotly)
library(DT)
library(shiny)
library(shinydashboard)
library(scales)


# UI ----------------------------------------------------------------------
ui <- dashboardPage(

# header ------------------------------------------------------------------
    dashboardHeader(title = "Filter defects"),

# sidebar -----------------------------------------------------------------
    dashboardSidebar(
        sidebarMenu(
            
            # DASHBOARD SECTION
            menuItem(
                "Dashboard",
                tabName = "dashboard",
                icon    = icon("dashboard")
            ),
            
            # SUBSET SELECTION
            selectInput(
                "defect_selection",
                label = "Defect: ",
                choices = levels(df_merged$CAUSE),
                selected = "BE"
            ),
            selectInput(
                "year_to_compare",
                label = "Year to compare: ",
                choices = levels(df_merged$year),
                selected = "2019"
            ),
            selectInput(
                "year_current",
                label = "Current year: ",
                choices = levels(df_merged$year),
                selected = "2020"
            ),
            selectizeInput(
                "select_EC",
                label = "Edge coat",
                choices = levels(df_merged$EC),
                multiple = TRUE,
                select = "None"
            ),
            selectizeInput(
                "select_PPI",
                label = "PPI",
                choices = levels(df_merged$PPI),
                multiple = TRUE,
                selected = levels(df_merged$PPI)[levels(df_merged$PPI) != "3d"]
            ),
            selectizeInput(
                "select_COMPOSITION",
                label = "Composition",
                choices = levels(df_merged$COMPOSITION),
                multiple = TRUE,
                selected = c("PSZT", "PSZT-FBG")
            ),
            selectizeInput(
                "select_KILN",
                label = "Kiln",
                choices = levels(df_merged$KILN),
                multiple = TRUE,
                selected = levels(df_merged$KILN)
            ), 
            # /SUBSET SELECTION
            
            # PROFIT/LOSS BREAKDOWN
            menuItem(
                "Profit Loss Breakdown",
                tabName = "plBreakdown",
                icon = icon("flask")
            ),
            
            # REFRESH DATA SECTION
            menuItem(
                "Refresh data",
                tabName = "refresh",
                icon    = icon("sync-alt")
            ),
            
            # RAW DATA SECTION
            menuItem(
                "Raw data",
                tabName = "rawData",
                icon = icon("cutlery"),
                menuSubItem("Yield",  tabName = "rawYield"),
                menuSubItem("Defect", tabName = "rawDefect"),
                menuSubItem("Merged", tabName = "rawMerged"),
                menuSubItem("Join warnings", tabName = "joinWarnings")
            )
            
        ) # /sidebarMenu
    ), # /dashboardSidebar

# body --------------------------------------------------------------------
    dashboardBody(
        tabItems(
            
# * dashboard TAB -----------------------------------------------------------
            tabItem(tabName = "dashboard",
                    
                    fluidRow(
                        box(width = 12,
                            title = "Saved Subsets",
                            collapsible = TRUE,
                            actionButton("button_subset_1", "NoEC, not3d, PSZT/FBG")
                            ) # /box
                        ), # /fluidRow
                    
                    fluidRow(
                        valueBoxOutput("progress_box", width = 6),
                        valueBoxOutput("rate_change_box", width = 6)
                        ),  # /fluidRow

                    fluidRow(
                        
                        box(width = 6, 
                            title = "Monthly Profit/Loss",
                            status = "primary",
                            solidHeader = TRUE,
                            collapsible = TRUE,
                            dataTableOutput("profit_loss_monthly")
                        ), # /box
                        
                        box(width = 6,
                            title = "Annual Profit/Loss",
                            status = "info",
                            solidHeader = TRUE,
                            collapsible = TRUE,
                            dataTableOutput("profit_loss_annual")
                        ) # /box
                        
                    ), # /fluidRow
                    
                    fluidRow(
                        
                        tabBox(width = 9,
                               tabPanel("Line", plotlyOutput("plot_rate_comparison_line")),
                               tabPanel("Bar", plotlyOutput("plot_rate_comparison_bar"))
                               ), # /tabBox
                        
                        box(width = 3,
                            title = "Annual comparison",
                            plotlyOutput("plot_rate_comparison_boxplot")
                            ) # /box
                        
                        )# /fluidRow
                    
            ), # /tabItem

# * profit loss breakdown TAB ---------------------------------------------------------------
            tabItem(tabName = "plBreakdown",

                    fluidRow(
                        box(width = 12,
                            title = "Explaination",
                            collapsible = TRUE,
                            h5("There may be discrepancies between the Excel data versus the data on the Dashboard page. This is most often due to the Dashboard page using the merged data rather than the separate Defect and Yield files. Below shows how the calculations may be different based on which datasets are used. Using the below tables with the Join Warnings supplied in the Raw Data >> Join Warnings tab will explain most discrepancies.")
                            )
                    ),

                    fluidRow(
                        box(width = 6,
                            title = "Monthly Yield + Defect",
                            collapsible = TRUE,
                            dataTableOutput("breakdown_monthly_separate")
                            ),
                        
                        box(width = 6,
                            title = "Monthly Merged",
                            collapsible = TRUE,
                            dataTableOutput("breakdown_monthly_merged")
                            )
                    ),

                    fluidRow(
                        box(width = 6,
                            title = "Annual Yield + Defect",
                            collapsible = TRUE,
                            dataTableOutput("breakdown_annual_separate")
                            ),
                        
                        box(width = 6,
                            title = "Annual Merged",
                            collapsible = TRUE,
                            dataTableOutput("breakdown_annual_merged")
                            )
                    )
                    
                    ), # /tabItem




# * refresh data TAB ------------------------------------------------------

            tabItem(tabName = "refresh",
                    
                    fluidRow(box(
                        width = 12,
                        title = "Warning",
                        status = "warning",
                        solidHeader = TRUE,
                        collapsible = TRUE,
                        h4("The actions on this page may take a while to complete depending on available resources."),
                        h4("It is recommended to only refresh files as needed.")
                        )),

                    fluidRow(
                        
                        box(title = "Yield data",
                            
                            h4("Available data:"),
                            verbatimTextOutput("xlsx_raw_yield_files"),
                            br(),
                            
                            selectInput(
                                "select_yield_refresh",
                                label = "Yield data to refresh: ",
                                choices = yield_xlsx_files,
                                selected = yield_xlsx_files[length(yield_xlsx_files)]
                                ),
                            br(),
                            
                            actionButton("button_yield_refresh", "  Refresh Yield", icon = icon("sync"))
                        ),
                        
                        box(title = "Defect data",

                            h4("Available data:"),
                            verbatimTextOutput("xlsx_raw_defect_files"),
                            br(),

                            selectInput(
                                "select_defect_refresh",
                                label = "Defect data to refresh: ",
                                choices = defect_xlsx_files,
                                selected = defect_xlsx_files[length(defect_xlsx_files)]
                                ),
                            br(),

                            actionButton("button_defect_refresh", " Refresh Defect", icon = icon("sync"))
                            )
                        ) # /fluidRow
                    
                    
                    ), # /tabItem

# * raw data TAB -----------------------------------------------------------
            tabItem(tabName = "rawYield",
                    fluidRow(
                        box(width = 12,
                            h1("Yield data"),
                            dataTableOutput("yield_df")
                        )
                    )),
            
            tabItem(tabName = "rawDefect",
                    fluidRow(
                        box(width = 12,
                            h1("Defect data"),
                            dataTableOutput("defect_df")
                        )
                    )),

            tabItem(tabName = "rawMerged",
                    fluidRow(
                        box(width = 12,
                            status = 'warning',
                            h4("Join Warnings"),
                            textOutput("joinMessage1"),
                            textOutput("joinMessage2"),
                            textOutput("joinMessage3")
                            ),

                        box(width = 12,
                            h1("Merged data"),
                            dataTableOutput("merged_df")
                        )
                    )), # /rawMerged

            tabItem(tabName = "joinWarnings",
                    fluidRow(
                        tabBox(width = 12,
                               selected = "Summary", 
                               tabPanel("Summary",
                                        h5("J1-2:"),
                                        textOutput("JM1"),
                                        h5("J3:"),
                                        textOutput("JM2"),
                                        h5("J4:"),
                                        textOutput("JM3")
                                        ),
                               tabPanel("J1",
                                        h4("Yield rows removed due to lagging defect data:"),
                                        dataTableOutput("JW_1")
                                        ),
                               tabPanel("J2",
                                        h4("Defect rows removed due to lagging yield data:"),
                                        br(),
                                        dataTableOutput("JW_2")
                                        ),
                               tabPanel("J3",
                                        h4("Defect rows removed with no matching yield data:"),
                                        br(),
                                        dataTableOutput("JW_3")
                                        ),
                               tabPanel("J4",
                                        h4("Yield rows removed which show non-zero defects but have no corresponding defect data:"),
                                        br(),
                                        dataTableOutput("JW_4")
                                        )
                               ) # /box
                        ) # /fluidRow
                    ) # /tabItem

        ) # /tabItems
    ) # /dashboardBody
) # /dashboardPage


# SERVER ----
server <- function(input, output, session) {

    source("src/cleanCombine.R")
    source("src/userFunctions.R")
    # source("src/BESummaryReport.R")
    # source("src/BESummaryReport_DOD.R")
    # source("src/userFunctions.R")
    

# outputs -----------------------------------------------------------------

    output$select_EC          <- renderPrint({ input$select_EC })
    output$select_PPI         <- renderPrint({ input$select_PPI })
    output$select_COMPOSITION <- renderPrint({ input$select_COMPOSITION })
    output$select_KILN        <- renderPrint({ input$select_KILN })
    
    output$xlsx_raw_yield_files <- renderPrint({
        wd <- getwd()
        yield_xlsx_dir <- paste0(wd, "/data/xlsx/Yield_Data")
        yield_files    <- list.files(yield_xlsx_dir, full.names=TRUE)
        yield_files
    })
    
    output$xlsx_raw_defect_files <- renderPrint({
        wd <- getwd()
        defect_xlsx_dir <- paste0(wd, "/data/xlsx/Defect_Data")
        defect_files    <- list.files(defect_xlsx_dir, full.names=TRUE)
        defect_files
    })
    
    output$test0 <- renderPrint({ 
        input$select_yield_refresh
        })
    
    output$test1 <- renderPrint({ 
        })
    
    output$test2 <- renderPrint({ 
        })
    
    
    df_costs_sep <- reactive({

        df_defects %>% 
            dplyr::filter(
                EC %in% input$select_EC &
                PPI %in% input$select_PPI &
                COMPOSITION %in% input$select_COMPOSITION & 
                KILN %in% input$select_KILN & 
                (year == input$year_to_compare | year == input$year_current) & 
                CAUSE == input$defect_selection
                ) %>% 
            group_by(year, month, year_month, CAUSE) %>% 
            dplyr::summarise(total_reject_cost = sum(reject_cost_single_row)) %>% 
            pivot_wider(names_from = CAUSE, values_from = total_reject_cost) %>% 
            left_join(
                df_yields %>% 
                    dplyr::filter(
                        EC %in% input$select_EC &
                        PPI %in% input$select_PPI &
                        COMPOSITION %in% input$select_COMPOSITION & 
                        KILN %in% input$select_KILN & 
                        (year == input$year_to_compare | year == input$year_current)
                        ) %>% 
                    mutate(total_item_fired_cost = TOTAL_ITEM_FIRED * cost_piece) %>% 
                    group_by(year, month) %>% 
                    dplyr::summarise(total_fired_cost = sum(total_item_fired_cost)),
                by = c("year", "month")
            ) %>% 
            dplyr::select(year, month, year_month, total_fired_cost, everything()) %>% 
            ungroup() %>% 
            mutate(defect_rate = get(input$defect_selection) / total_fired_cost)
    })
    
    output$breakdown_monthly_separate <- renderDataTable({
        
        datatable(
            df_costs_sep() %>%
                dplyr::select(year_month, total_fired_cost, input$defect_selection, defect_rate) %>% 
                set_colnames(c("Date", "Total fired cost", "Total defect cost", "Defect rate")),
            options = list(dom = 't', pageLength = 24),
            class = 'cell-border stripe',
            caption = paste0("Monthly fired costs & ", input$defect_selection, " defect costs"),
            rownames = FALSE) %>% 
            formatCurrency(c("Total fired cost", "Total defect cost"), "$") %>% 
            formatPercentage("Defect rate", digits = 2)
        
    })
    
    output$breakdown_annual_separate <- renderDataTable({
        df_costs_sep_rates <- df_costs_sep() %>% 
            dplyr::select(year, month, total_fired_cost, input$defect_selection) %>% 
            group_by(year) %>% 
            dplyr::summarise(total_fired_cost = sum(total_fired_cost),
                             total_defect_cost = sum(get(input$defect_selection))) %>% 
            mutate(defect_rate = total_defect_cost / total_fired_cost)
        
        datatable(
            df_costs_sep_rates %>% 
                mutate(last_years_rate = ifelse(year == input$year_current, df_costs_sep_rates$defect_rate[1], NA),
                       savings_loss = (total_fired_cost * last_years_rate) - total_defect_cost) %>% 
                dplyr::select( -last_years_rate) %>% 
                set_colnames(c("Year", "Total fired cost", "Total defect cost", "Defect rate", "Savings / Loss")),
            options = list(dom = 't', pageLength = 2),
            class = 'cell-border stripe',
            caption = paste0("Annual fired costs & ", input$defect_selection, " defect costs"),
            rownames = FALSE) %>% 
            formatCurrency(c("Total fired cost", "Total defect cost", "Savings / Loss"), "$") %>% 
            formatPercentage("Defect rate", digits = 2)
    })
    
    df_costs_merged <- reactive({

        df_merged %>% 
            dplyr::filter(
                EC %in% input$select_EC &
                PPI %in% input$select_PPI &
                COMPOSITION %in% input$select_COMPOSITION & 
                KILN %in% input$select_KILN & 
                (year == input$year_to_compare | year == input$year_current) & 
                CAUSE == input$defect_selection
                ) %>% 
            group_by(year, month, year_month, CAUSE) %>% 
            dplyr::summarise(total_reject_cost = sum(reject_cost_single_row_D)) %>% 
            pivot_wider(names_from = CAUSE, values_from = total_reject_cost) %>% 
            left_join(
                df_merged %>% 
                    dplyr::filter(
                        EC %in% input$select_EC &
                        PPI %in% input$select_PPI &
                        COMPOSITION %in% input$select_COMPOSITION & 
                        KILN %in% input$select_KILN & 
                        (year == input$year_to_compare | year == input$year_current)
                        ) %>% 
                    group_by(ITEM, LOTNO, KILN) %>% slice(1) %>% ungroup() %>% 
                    mutate(total_item_fired_cost = TOTAL_ITEM_FIRED_Y * cost_piece) %>% 
                    group_by(year, month) %>% 
                    dplyr::summarise(total_fired_cost = sum(total_item_fired_cost)),
                by = c("year", "month")
            ) %>% 
            dplyr::select(year, month, year_month, total_fired_cost, everything()) %>% 
            ungroup() %>% 
            mutate(defect_rate = get(input$defect_selection) / total_fired_cost)
        
    })

    output$breakdown_monthly_merged <- renderDataTable({

        datatable(
            df_costs_merged() %>%
                dplyr::select(year_month, total_fired_cost, input$defect_selection, defect_rate) %>% 
                set_colnames(c("Date", "Total fired cost", "Total defect cost", "Defect rate")),
            options = list(dom = 't', pageLength = 24),
            class = 'cell-border stripe',
            caption = paste0("Monthly fired costs & ", input$defect_selection, " defect costs"),
            rownames = FALSE) %>% 
            formatCurrency(c("Total fired cost", "Total defect cost"), "$")%>% 
            formatPercentage("Defect rate", digits = 2)

    })
    
    output$breakdown_annual_merged <- renderDataTable({
        
        df_costs_merged_rates <- df_costs_merged() %>% 
            dplyr::select(year, month, total_fired_cost, input$defect_selection) %>% 
            group_by(year) %>% 
            dplyr::summarise(total_fired_cost = sum(total_fired_cost),
                             total_defect_cost = sum(get(input$defect_selection))) %>% 
            mutate(defect_rate = total_defect_cost / total_fired_cost)
        
        datatable(
            df_costs_merged_rates %>% 
                mutate(last_years_rate = ifelse(year == input$year_current, df_costs_merged_rates$defect_rate[1], NA),
                       savings_loss = (total_fired_cost * last_years_rate) - total_defect_cost) %>% 
                dplyr::select( -last_years_rate) %>% 
                set_colnames(c("Year", "Total fired cost", "Total defect cost", "Defect rate", "Savings / Loss")),
            options = list(dom = 't', pageLength = 2),
            class = 'cell-border stripe',
            caption = paste0("Annual fired costs & ", input$defect_selection, " defect costs"),
            rownames = FALSE) %>% 
            formatCurrency(c("Total fired cost", "Total defect cost", "Savings / Loss"), "$") %>% 
            formatPercentage("Defect rate", digits = 2)
    })
    
    
    # output$filtered_subset <- renderDataTable({
    #     filtered_subset()
    # })
    # 
    
    # output$test1 <- renderDataTable({ defect_rate() })
    # output$test2 <- renderPrint({ defect_rate_year_to_compare() })
    # output$test3 <- renderDataTable({  })
    
    # output$year_current <- renderPrint({
    #     input$year_current
    # })
    # output$defect_selection <- renderPrint({
    #     input$defect_selection
    # })
    


# events -----------------------------------------------------------

    # re-export yield xlsx to csv
    observeEvent(input$button_yield_refresh, {
        library(xlsx)
        library(stringr)
        library(readr)
        
        file_to_get <- input$select_yield_refresh
        xlsx_dir    <- yield_xlsx_dir
        full_path   <- paste0(xlsx_dir, file_to_get)
        yield_csv_name  <- paste0(str_extract(file_to_get, "(.*?)(?=\\.)"), ".csv")
        yield_csv_path <- paste0(yield_csv_dir, yield_csv_name)
        
        withProgress(
            message = "Working.",
            detail = "This may be slow...",
            value = 0,
            {
                incProgress(1/3)
                updated_yield_xlsx <- read.xlsx(full_path, "Detail",
                                                colIndex = seq(1:26))
                incProgress(2/3)
                write_csv(updated_yield_xlsx, yield_csv_path)
                incProgress(3/3)
            }
        )
        showModal(modalDialog(paste("Done! Refresh to view changes!")))
    })    

    # re-export defect xlsx to csv
    observeEvent(input$button_defect_refresh, {
        library(xlsx)
        library(stringr)
        library(readr)
        
        file_to_get <- input$select_defect_refresh
        xlsx_dir    <- defect_xlsx_dir
        full_path   <- paste0(xlsx_dir, file_to_get)
        defect_csv_name  <- paste0(str_extract(file_to_get, "(.*?)(?=\\.)"), ".csv")
        defect_csv_path <- paste0(defect_csv_dir, defect_csv_name)
        
        withProgress(
            message = "Working.",
            detail = "This may be slow...",
            value = 0,
            {
                incProgress(1/3)
                updated_defect_xlsx <- read.xlsx(full_path, "Detail",
                                                colIndex = seq(1:26))
                incProgress(2/3)
                write_csv(updated_defect_xlsx, defect_csv_path)
                incProgress(3/3)
            }
        )
        showModal(modalDialog(paste("Done! Refresh to view changes!")))
    })    
    
# reactive variables ------------------------------------------------------
    
    year_current     <- reactive({ input$year_current })
    year_to_compare  <- reactive({ input$year_to_compare })
    defect_selection <- reactive({ input$defect_selection })
    
    select_EC          <- reactive({ input$select_EC })
    select_PPI         <- reactive({ input$select_PPI })
    select_COMPOSITION <- reactive({ input$select_COMPOSITION })
    select_KILN        <- reactive({ input$select_KILN })

# reactive DFs ------------------------------------------------------------
    
    # entire filtered subset
    filtered_subset <- reactive({
        df_merged %>%
            dplyr::filter(
                EC %in% input$select_EC &
                PPI %in% input$select_PPI &
                COMPOSITION %in% input$select_COMPOSITION & 
                KILN %in% input$select_KILN
                )
    })
    
    # calculate defect rate using filtered subset
    defect_rate <- reactive({
        filtered_subset() %>% 
            dplyr::filter(year == input$year_to_compare |
                          year == input$year_current) %>%
            group_by(ITEM, LOTNO, KILN) %>% slice(1) %>% ungroup() %>%
            mutate(total_item_fired_cost = TOTAL_ITEM_FIRED_Y * cost_piece) %>%
            group_by(year) %>%
            dplyr::summarise(total_fired_cost = sum(total_item_fired_cost)) %>%
            left_join(
                # total defect_selection cost
                filtered_subset() %>%
                    dplyr::filter((year == input$year_to_compare |
                                   year == input$year_current) &
                                   CAUSE == input$defect_selection) %>%
                    group_by(year) %>%
                    dplyr::summarise(total_defect_cost = sum(reject_cost_single_row_D))
            ) %>%
            mutate(defect_cost_per_total_fired_cost = total_defect_cost / total_fired_cost)
    })
    
    # get defect rate for year_to_compare
    defect_rate_year_to_compare <- reactive({
        defect_rate()[defect_rate()$year == input$year_to_compare, "defect_cost_per_total_fired_cost"][[1]]
    })
    
    # calculate profit/loss between year_to_compare and year_current
    profit_loss_year_comparison <- reactive({
        defect_rate() %>%
            mutate(rate_compare = ifelse(year == input$year_current, defect_rate_year_to_compare(), NA)) %>%
            mutate(profit_loss = total_fired_cost * rate_compare - total_defect_cost)
    })
    
    # calculate profit/loss each month for year_current
    profit_loss_month_comparison <- reactive({
        filtered_subset() %>% 
            dplyr::filter(year == input$year_current) %>% 
            group_by(ITEM, LOTNO, KILN) %>% slice(1) %>% ungroup() %>% 
            mutate(total_item_fired_cost = TOTAL_ITEM_FIRED_Y * cost_piece) %>% 
            group_by(year, month, year_month) %>% 
            dplyr::summarise(total_fired_cost = sum(total_item_fired_cost)) %>% 
            left_join(
                # total defect_selection cost
                filtered_subset() %>% 
                    dplyr::filter((year == input$year_to_compare | 
                                   year == input$year_current) & 
                                  CAUSE == input$defect_selection) %>% 
                    group_by(year, month, year_month) %>% 
                    dplyr::summarise(total_defect_cost = sum(reject_cost_single_row_D))
            ) %>% 
            mutate(defect_cost_per_total_fired_cost = total_defect_cost / total_fired_cost) %>% 
            mutate(rate_compare = defect_rate_year_to_compare()) %>% 
            mutate(profit_loss = total_fired_cost * rate_compare - total_defect_cost) %>% 
            ungroup() %>% 
            dplyr::select(year_month, defect_cost_per_total_fired_cost, profit_loss)
    })

# action Buttons -----------------------------------------------------------------

    observeEvent(input$button_subset_1,{
        
        if (input$button_subset_1%%2 == 0){
            updateSelectizeInput(session, "select_EC", select = "None")
            updateSelectizeInput(session, "select_PPI", select = levels(df_merged$PPI)[levels(df_merged$PPI) != "3d"])
            updateSelectizeInput(session, "select_COMPOSITION", select = c("PSZT", "PSZT-FBG"))
            } 
        else {
            updateSelectizeInput(session, "select_EC", select = "")
            updateSelectizeInput(session, "select_PPI", select = "")
            updateSelectizeInput(session, "select_COMPOSITION", select = "")
            }
        })


    

# table outputs ------------------------------------------------------------

    # annual P/L
    output$profit_loss_annual <- renderDataTable({
        DT::datatable(
            profit_loss_year_comparison() %>% 
                dplyr::select(!c(rate_compare)) %>% 
                set_colnames(c("Year", "Total fired cost", "Total defect cost", "Defect rate", "Profit / Loss"))
            ,
            options = list(dom = 't'),
            class = 'cell-border stripe',
            caption = paste0("Profit/Loss table for ", defect_selection(), " defects"),
            rownames = FALSE
            ) %>% 
            formatCurrency(c("Total fired cost", "Total defect cost", "Profit / Loss"), "$") %>% 
            formatPercentage("Defect rate", digits = 2) %>% 
            formatStyle('Year', target = 'row', fontWeight = styleEqual(year_current(), 'bold'))
    })
    
    # monthly P/L
    output$profit_loss_monthly <- renderDataTable({
        DT::datatable(
            profit_loss_month_comparison() %>% 
                bind_rows(
                    profit_loss_year_comparison() %>% 
                        dplyr::select(year, defect_cost_per_total_fired_cost, profit_loss) %>% 
                        dplyr::filter(year == input$year_current) %>% 
                        set_colnames(c("year_month", "defect_cost_per_total_fired_cost", "profit_loss"))
                        ) %>% 
                set_colnames(c("Date", "Defect rate", "Profit / Loss" )),
            options = list(dom = 't', pageLength = 13 ),
            class = 'cell-border stripe',
            caption = paste0("Monthly Profit/Loss table for ", input$defect_selection, " defects"),
            rownames = FALSE) %>% 
            formatCurrency(c("Profit / Loss"), "$") %>% 
            formatPercentage("Defect rate", digits = 2) %>% 
            formatStyle('Date', target = 'row', 
                        fontWeight = styleEqual(input$year_current, "bold"),
                        backgroundColor = styleEqual(input$year_current, "yellow")
                        )
        })
    

# valueBoxes --------------------------------------------------------------
    
    # P/L progress
    output$progress_box <- renderValueBox({

        progress_profit_loss <- 
            paste0("$", formatC(profit_loss_year_comparison()[profit_loss_year_comparison()$year==input$year_current,'profit_loss'][[1]], 
                           format="fg",big.mark=","))
        valueBox(
            progress_profit_loss,
            "Savings Progress",
            icon = icon("money-bill-wave"),
            color = 'green'
            )
    })
    
    # defect rate change
    output$rate_change_box <- renderValueBox({
        year_to_compare_rate <- defect_rate()[defect_rate()$year == input$year_to_compare, "defect_cost_per_total_fired_cost"][[1]]
        year_current_rate    <- defect_rate()[defect_rate()$year == input$year_current, "defect_cost_per_total_fired_cost"][[1]]
        
        rate_change <- paste0(formatC(signif( 
            (year_current_rate - year_to_compare_rate) / year_to_compare_rate,
            3)*100, format="fg",big.mark=","), "%" )
        
        valueBox(
            rate_change,
            "Defect rate change vs *Year to compare*",
            icon = icon("history"),
            color = 'teal'
            )
    })

# plots -------------------------------------------------------------------

    output$plot_rate_comparison_bar <- renderPlotly({
        
        g.title =    paste0(input$defect_selection, 
                           " Defect Rate vs Month ",
                           input$year_to_compare, 
                           "-", 
                           input$year_current)
        g.subtitle = paste0("Defect Rate = (Total ", input$defect_selection, " Defect Cost / Total Fired Cost) of subset")
        g.caption =  ""
        
        df_defect_rate_compare <- filtered_subset() %>% 
            # total fired cost
            dplyr::filter(year == input$year_current | 
                          year == input$year_to_compare) %>% 
            group_by(ITEM, LOTNO, KILN) %>% slice(1) %>% ungroup() %>% 
            mutate(total_item_fired_cost = TOTAL_ITEM_FIRED_Y * cost_piece) %>% 
            group_by(year, month, year_month) %>% 
            dplyr::summarise(total_fired_cost = sum(total_item_fired_cost)) %>% 
            left_join(
                # total defect_selection cost
                filtered_subset() %>% 
                    dplyr::filter((year == input$year_to_compare | 
                                   year == input$year_current) & 
                                  CAUSE == input$defect_selection) %>% 
                    group_by(year, month, year_month) %>% 
                    dplyr::summarise(total_defect_cost = sum(reject_cost_single_row_D))
            ) %>% 
            mutate(defect_cost_per_total_fired_cost = total_defect_cost / total_fired_cost) %>% 
            left_join(
                profit_loss_year_comparison() %>% 
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
                               "<br>Total annual rate:", mypercent(annual_rate)
                           )))+
            geom_bar(aes(y = defect_cost_per_total_fired_cost), 
                     stat='identity',
                     position = 'dodge')+
            scale_fill_brewer(name = "Year", palette = "Paired")+
            theme_minimal()+
            labs(title    = g.title,
                 subtitle = g.subtitle,
                 caption  = g.caption)+
            xlab("Month")+
            ylab("Defect Rate")+
            scale_y_continuous(breaks = seq(0, 
                                            round(max(df_defect_rate_compare$defect_cost_per_total_fired_cost, na.rm = TRUE),2),
                                            .01),
                               labels = percent_format(accuracy = 1))
        
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
        
    })
    
    output$plot_rate_comparison_line <- renderPlotly({
        
        g.title =    paste0(input$defect_selection, 
                           " Defect Rate vs Month ",
                           input$year_to_compare, 
                           "-", 
                           input$year_current)
        g.subtitle = paste0("Defect Rate = (Total ", input$defect_selection, " Defect Cost / Total Fired Cost) of subset")
        g.caption =  "Dashed line = Total annual rates"
        
        df_defect_rate_compare <- filtered_subset() %>% 
            # total fired cost
            dplyr::filter(year == input$year_current | 
                              year == input$year_to_compare) %>% 
            group_by(ITEM, LOTNO, KILN) %>% slice(1) %>% ungroup() %>% 
            mutate(total_item_fired_cost = TOTAL_ITEM_FIRED_Y * cost_piece) %>% 
            group_by(year, month, year_month) %>% 
            dplyr::summarise(total_fired_cost = sum(total_item_fired_cost)) %>% 
            left_join(
                # total defect_selection cost
                filtered_subset() %>% 
                    dplyr::filter((year == input$year_to_compare | 
                                       year == input$year_current) & 
                                      CAUSE == input$defect_selection) %>% 
                    group_by(year, month, year_month) %>% 
                    dplyr::summarise(total_defect_cost = sum(reject_cost_single_row_D))
            ) %>% 
            mutate(defect_cost_per_total_fired_cost = total_defect_cost / total_fired_cost) %>% 
            left_join(
                profit_loss_year_comparison() %>% 
                    dplyr::select(year, defect_cost_per_total_fired_cost) %>% 
                    set_colnames(c("year", "annual_rate"))
            )
        
        gg_defect_rate_compare <- df_defect_rate_compare %>% 
            ungroup() %>% 
            ggplot(aes(x = month, 
                       fill = year,
                       text = 
                           paste(
                               "Date:", year_month,
                               "<br>Defect rate:", mypercent(defect_cost_per_total_fired_cost),
                               "<br>Total defect cost", mycurrency(total_defect_cost),
                               "<br>Total fired cost:", mycurrency(total_fired_cost),
                               "<br>Total annual rate:", mypercent(annual_rate)
                           )))+
            # geom_point(aes(y = defect_cost_per_total_fired_cost, color = year), shape = 1, fill = 'white')+
            geom_line(aes(y = defect_cost_per_total_fired_cost, group = year, color = year))+
            geom_hline(aes(yintercept = annual_rate,
                           color = year), 
                       linetype = 'dotted', 
                       alpha=.6)+
            scale_color_brewer(name = "Year", palette = "Paired")+
            # scale_fill_brewer(palette = "Paired")+
            # geom_bar(aes(y = defect_cost_per_total_fired_cost), 
            #          stat='identity',
            #          position = 'dodge')+
            # scale_fill_brewer(name = "Defect rate", palette = "Paired")+
            theme_minimal()+
            labs(title    = g.title,
                 subtitle = g.subtitle,
                 caption  = g.caption)+
            xlab("Month")+
            ylab("Defect Rate")+
            scale_y_continuous(breaks = seq(0, 
                                            round(max(df_defect_rate_compare$defect_cost_per_total_fired_cost, na.rm=TRUE),2),
                                            .01),
                               labels = percent_format(accuracy = 1))
        
        
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
    })
    
    output$plot_rate_comparison_boxplot <- renderPlotly({
        
        g.title =    ""
        g.subtitle = "Distribution of monthly rates"
        g.caption =  ""
        
        df_defect_rate_compare <- filtered_subset() %>% 
            # total fired cost
            dplyr::filter(year == input$year_current | 
                              year == input$year_to_compare) %>% 
            group_by(ITEM, LOTNO, KILN) %>% slice(1) %>% ungroup() %>% 
            mutate(total_item_fired_cost = TOTAL_ITEM_FIRED_Y * cost_piece) %>% 
            group_by(year, month, year_month) %>% 
            dplyr::summarise(total_fired_cost = sum(total_item_fired_cost)) %>% 
            left_join(
                # total defect_selection cost
                filtered_subset() %>% 
                    dplyr::filter((year == input$year_to_compare | 
                                       year == input$year_current) & 
                                      CAUSE == input$defect_selection) %>% 
                    group_by(year, month, year_month) %>% 
                    dplyr::summarise(total_defect_cost = sum(reject_cost_single_row_D))
            ) %>% 
            mutate(defect_cost_per_total_fired_cost = total_defect_cost / total_fired_cost) %>% 
            left_join(
                profit_loss_year_comparison() %>% 
                    dplyr::select(year, defect_cost_per_total_fired_cost) %>% 
                    set_colnames(c("year", "annual_rate"))
            )
        
        gg_defect_rate_compare <- df_defect_rate_compare %>%
            ggplot(aes(x = year, color = year,
                       text = paste(
                           "Month:", month,
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
                               labels = percent_format(accuracy=1))

        gp_defect_rate_compare <- ggplotly(gg_defect_rate_compare, tooltip = c("text")) %>%
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
            ) %>% 
            layout(yaxis = list(hoverformat = '.4f'))
            
        
        gp_defect_rate_compare$x$data[[1]]$showlegend = FALSE
        gp_defect_rate_compare$x$data[[2]]$showlegend = FALSE
        gp_defect_rate_compare$x$data[[1]]$marker$opacity = 0
        gp_defect_rate_compare$x$data[[2]]$marker$opacity = 0
        
        gp_defect_rate_compare
        
    })

    output$monthly_profit_loss <- renderDataTable({

        year_to_compare <- input$year_to_compare
        year_current <- input$year_current
        defect_selection <- input$defect_selection

        filter_subset <- df_merged %>%
            dplyr::filter(
                PPI != "3d" &
                    EC == "None" &
                    (COMPOSITION == "PSZT" | COMPOSITION == "PSZT-FBG")
            )

        dcptfc <- filter_subset %>%
            # total fired cost
            dplyr::filter(year == year_to_compare |
                              year == year_current) %>%
            group_by(ITEM, LOTNO, KILN) %>% slice(1) %>% ungroup() %>%
            mutate(total_item_fired_cost = TOTAL_ITEM_FIRED_Y * cost_piece) %>%
            group_by(year) %>%
            dplyr::summarise(total_fired_cost = sum(total_item_fired_cost)) %>%
            left_join(
                # total defect_selection cost
                filter_subset %>%
                    dplyr::filter((year == year_to_compare |
                                       year == year_current) &
                                      CAUSE == defect_selection) %>%
                    group_by(year) %>%
                    dplyr::summarise(total_defect_cost = sum(reject_cost_single_row_D))
            ) %>%
            mutate(defect_cost_per_total_fired_cost = total_defect_cost / total_fired_cost)

        year_to_compare_rate <- dcptfc[dcptfc$year == year_to_compare, "defect_cost_per_total_fired_cost"][[1]]

        profit_loss_monthly <- filter_subset %>%
            # total fired cost
            dplyr::filter(year == year_current) %>%
            group_by(ITEM, LOTNO, KILN) %>% slice(1) %>% ungroup() %>%
            mutate(total_item_fired_cost = TOTAL_ITEM_FIRED_Y * cost_piece) %>%
            group_by(year, month, year_month) %>%
            dplyr::summarise(total_fired_cost = sum(total_item_fired_cost)) %>%
            left_join(
                # total defect_selection cost
                filter_subset %>%
                    dplyr::filter((year == year_to_compare |
                                       year == year_current) &
                                      CAUSE == defect_selection) %>%
                    group_by(year, month, year_month) %>%
                    dplyr::summarise(total_defect_cost = sum(reject_cost_single_row_D))
            ) %>%
            mutate(defect_cost_per_total_fired_cost = total_defect_cost / total_fired_cost) %>%
            mutate(rate_compare = year_to_compare_rate) %>%
            mutate(profit_loss = total_fired_cost * rate_compare - total_defect_cost) %>%
            ungroup() %>%
            dplyr::select(year_month, defect_cost_per_total_fired_cost, profit_loss)

        dt_profit_loss_monthly <-
            DT::datatable(
                profit_loss_monthly %>%
                    set_colnames(c(
                        "Month",
                        "Defect rate",
                        "Profit / Loss"
                    )),
                options = list(
                    dom = 't'
                ),
                class = 'cell-border stripe',
                caption = paste0("Monthly Profit/Loss table for ", defect_selection, " defects"),
                rownames = FALSE) %>%
            formatCurrency(c("Profit / Loss"), "$") %>%
            formatPercentage("Defect rate",
                             digits = 2) %>%
            formatStyle(
                'Profit / Loss',
                target = 'row',
                fontWeight = styleEqual("YTD",
                                        "bold"),
                backgroundColor = styleEqual("YTD",
                                             "yellow")
            )
        dt_profit_loss_monthly
        })
    
    output$ggp_annual_comparison <- renderPlotly({

        year_to_compare <- input$year_to_compare
        year_current <- input$year_current
        defect_selection <- input$defect_selection

        filter_subset <- df_merged %>%
            dplyr::filter(
                PPI != "3d" &
                    EC == "None" &
                    (COMPOSITION == "PSZT" | COMPOSITION == "PSZT-FBG")
            )
        # get defect cost per fired cost
        dcptfc <- filter_subset %>% 
            # total fired cost
            dplyr::filter(year == year_to_compare | 
                              year == year_current) %>% 
            group_by(ITEM, LOTNO, KILN) %>% slice(1) %>% ungroup() %>% 
            mutate(total_item_fired_cost = TOTAL_ITEM_FIRED_Y * cost_piece) %>% 
            group_by(year) %>% 
            dplyr::summarise(total_fired_cost = sum(total_item_fired_cost)) %>% 
            left_join(
                # total defect_selection cost
                filter_subset %>% 
                    dplyr::filter((year == year_to_compare | 
                                       year == year_current) & 
                                      CAUSE == defect_selection) %>% 
                    group_by(year) %>% 
                    dplyr::summarise(total_defect_cost = sum(reject_cost_single_row_D))
            ) %>% 
            mutate(defect_cost_per_total_fired_cost = total_defect_cost / total_fired_cost)
        
        year_to_compare_rate <- dcptfc[dcptfc$year == year_to_compare, "defect_cost_per_total_fired_cost"][[1]]
        
        profit_loss <- dcptfc %>% 
            mutate(rate_compare = ifelse(year == year_current, year_to_compare_rate, NA)) %>% 
            mutate(profit_loss = total_fired_cost * rate_compare - total_defect_cost)
        
        # monthly defect rate comparison
        g.title =    paste0(defect_selection, " Defect Rate vs Month, ", year_to_compare, "-", year_current)
        g.subtitle = "Defect Rate = (Total Defect Cost / Total Fired Cost) of subset"
        g.caption =  ""
        
        df_defect_rate_compare <- filter_subset %>% 
            # total fired cost
            dplyr::filter(year == year_current | 
                              year == year_to_compare) %>% 
            group_by(ITEM, LOTNO, KILN) %>% slice(1) %>% ungroup() %>% 
            mutate(total_item_fired_cost = TOTAL_ITEM_FIRED_Y * cost_piece) %>% 
            group_by(year, month, year_month) %>% 
            dplyr::summarise(total_fired_cost = sum(total_item_fired_cost)) %>% 
            left_join(
                # total defect_selection cost
                filter_subset %>% 
                    dplyr::filter((year == year_to_compare | 
                                       year == year_current) & 
                                      CAUSE == defect_selection) %>% 
                    group_by(year, month, year_month) %>% 
                    dplyr::summarise(total_defect_cost = sum(reject_cost_single_row_D))
            ) %>% 
            mutate(defect_cost_per_total_fired_cost = total_defect_cost / total_fired_cost) %>% 
            left_join(
                profit_loss %>% 
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
            geom_hline(aes(yintercept = annual_rate), linetype = 'dotted', alpha=.16)+
            geom_point(aes(y = annual_rate),
                       shape = 20,
                       size = 2,
                       stroke = .1)+
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
                                            round(max(df_defect_rate_compare$defect_cost_per_total_fired_cost, na.rm = TRUE),2),
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
        
    })
    
    output$ggp_annual_comparison_line <- renderPlotly({

        year_to_compare <- input$year_to_compare
        year_current <- input$year_current
        defect_selection <- input$defect_selection

        filter_subset <- df_merged %>%
            dplyr::filter(
                PPI != "3d" &
                    EC == "None" &
                    (COMPOSITION == "PSZT" | COMPOSITION == "PSZT-FBG")
            )
        # get defect cost per fired cost
        dcptfc <- filter_subset %>% 
            # total fired cost
            dplyr::filter(year == year_to_compare | 
                              year == year_current) %>% 
            group_by(ITEM, LOTNO, KILN) %>% slice(1) %>% ungroup() %>% 
            mutate(total_item_fired_cost = TOTAL_ITEM_FIRED_Y * cost_piece) %>% 
            group_by(year) %>% 
            dplyr::summarise(total_fired_cost = sum(total_item_fired_cost)) %>% 
            left_join(
                # total defect_selection cost
                filter_subset %>% 
                    dplyr::filter((year == year_to_compare | 
                                       year == year_current) & 
                                      CAUSE == defect_selection) %>% 
                    group_by(year) %>% 
                    dplyr::summarise(total_defect_cost = sum(reject_cost_single_row_D))
            ) %>% 
            mutate(defect_cost_per_total_fired_cost = total_defect_cost / total_fired_cost)
        
        year_to_compare_rate <- dcptfc[dcptfc$year == year_to_compare, "defect_cost_per_total_fired_cost"][[1]]
        
        profit_loss <- dcptfc %>% 
            mutate(rate_compare = ifelse(year == year_current, year_to_compare_rate, NA)) %>% 
            mutate(profit_loss = total_fired_cost * rate_compare - total_defect_cost)
        
        # monthly defect rate comparison
        g.title =    paste0(defect_selection, " Defect Rate vs Month, ", year_to_compare, "-", year_current)
        g.subtitle = "Defect Rate = (Total Defect Cost / Total Fired Cost) of subset"
        g.caption =  ""
        
        df_defect_rate_compare <- filter_subset %>% 
            # total fired cost
            dplyr::filter(year == year_current | 
                              year == year_to_compare) %>% 
            group_by(ITEM, LOTNO, KILN) %>% slice(1) %>% ungroup() %>% 
            mutate(total_item_fired_cost = TOTAL_ITEM_FIRED_Y * cost_piece) %>% 
            group_by(year, month, year_month) %>% 
            dplyr::summarise(total_fired_cost = sum(total_item_fired_cost)) %>% 
            left_join(
                # total defect_selection cost
                filter_subset %>% 
                    dplyr::filter((year == year_to_compare | 
                                       year == year_current) & 
                                      CAUSE == defect_selection) %>% 
                    group_by(year, month, year_month) %>% 
                    dplyr::summarise(total_defect_cost = sum(reject_cost_single_row_D))
            ) %>% 
            mutate(defect_cost_per_total_fired_cost = total_defect_cost / total_fired_cost) %>% 
            left_join(
                profit_loss %>% 
                    dplyr::select(year, defect_cost_per_total_fired_cost) %>% 
                    set_colnames(c("year", "annual_rate"))
            )
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
                                            round(max(df_defect_rate_compare$defect_cost_per_total_fired_cost, na.rm=TRUE),2),
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
        
        
    })
    
    output$gp_cpv <- renderPlotly({
        gp_cpv
    })
    
    output$gp_cpv_sum <- renderPlotly({
        gp_cpv_sub_summary
    })
    
    output$dt_pl <- renderDataTable({
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
        })
    
    
    

# Raw data DTs ------------------------------------------------------------
    
    # * yield DF
    output$yield_df <- renderDataTable(
        datatable(df_yields[, -c(2:4)] %>%
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
                      )),
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
    )
    
    # * defect DF
    output$defect_df <- renderDataTable(
        datatable(df_defects[, -c(2:4)] %>%
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
                  caption = "T.I. = Total Item",
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
            formatCurrency(c("Total item cost of rejects",
                             "Reject cost single row",
                             "Cost/in3"))
    )
    
    # * merged DF
    output$merged_df <- renderDataTable(
        datatable(df_merged[, -c(2:4)] %>%
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
                          "Reject vol single row (D)",
                          "Reject cost single row (D)",
                          "T.I. fired (Y)",
                          "T.I. count fired (D)",
                          "T.I. rejected (Y)",
                          "T.I. count rejected (D)",
                          "T.I. pct yield (Y)",
                          "T.I. cost of rejects (D)",
                          "T.I. fired vol (D)",
                          "T.I. fired vol (Y)",
                          "T.I. rejected vol (D)",
                          "T.I. rejected vol (Y)",
                          "Cost/pc",
                          "Vol/pc",
                          "Cost/in3"
                          )),
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
            formatPercentage("T.I. pct yield (Y)", 2) %>% 
            formatCurrency(c("Reject cost single row (D)",
                             "T.I. cost of rejects (D)",
                             "Cost/pc",
                             "Cost/in3"))
    )

# join warning DTs ----------------------------------------------------
    
    output$JW_1 <- renderDataTable({
        datatable(
            df_yields_removed_date_out_of_range[, -c(2:4)] %>% 
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
                    )),
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
                )) %>%
            formatPercentage("Total Item Pct Yield", 2) %>%
            formatCurrency("Cost/Pc")
    })   
    
    output$JW_2 <- renderDataTable({
        datatable(
            df_defects_removed_date_out_of_range[, -c(2:4)] %>% 
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
            caption = "T.I. = Total Item",
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
            )) %>%
            formatCurrency(c("Total item cost of rejects",
                             "Reject cost single row",
                             "Cost/in3"))
        
    })   
    
    output$JW_3 <- renderDataTable({
        datatable(
            df_defect_no_matching_yield %>% 
                set_colnames(c(
                    "Fire date",
                    "LotNo",
                    "Kiln",
                    "Item",
                    "Description",
                    "Cause",
                    "Cost/in3",
                    "Reject vol single row",
                    "Reject cost single row",
                    "Total item fired vol",
                    "Total item rejected vol",
                    "Total item cost of rejects",
                    "Total item count fired",
                    "Total item count rejected"
                )),
            caption = "T.I. = Total Item",
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
            )) %>%
            formatCurrency(c("Total item cost of rejects",
                             "Reject cost single row",
                             "Cost/in3"))
    })   
    
    output$JW_4 <- renderDataTable({
        datatable(
            df_yield_no_matching_defect %>% 
                set_colnames(c(
                    "Fire date",
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
                )),
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
            )) %>%
            formatPercentage("Total Item Pct Yield", 2) %>%
            formatCurrency("Cost/Pc")
    })   

# join warning messages ---------------------------------------------------
    
    output$JM1 <- renderText({
        join_messages[1]
    })
    output$JM2 <- renderText({
        join_messages[2]
    })
    output$JM3 <- renderText({
        join_messages[3]
    })
    output$joinMessage1 <- renderText({
        join_messages[1]
    })
    output$joinMessage2 <- renderText({
        join_messages[2]
    })
    output$joinMessage3 <- renderText({
        join_messages[3]
    })
    
}

# RUN ----
shinyApp(ui = ui, server = server)
