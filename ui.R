# Define UI
shinyUI(fluidPage(
  useShinyjs(),
  extendShinyjs(text = js_year_over_year_bucket_transactions, 
                functions = c("get_bucket_transactions_bucket_group",
                              "get_year_over_year_bucket_group")),
    tags$head(
      tags$style( 
          str_c(zoom_reverse, #Reverse zoom for plotly graphs to avoid glitches and make hover work
                zoom_css, # Adjust zoom to fit 1 year without horizontal scrolling
                error_position_css, #Manually adjust position of validation messages
                form_group_css, 
                btn_css) 
          
      )
    ),
    
    # Application title
    titlePanel(tagList(
      h1("Buckets Dashboard", style = "display: inline"),
      actionButton("reload_app", "Reload .buckets file", style = "position: absolute; right: 18px;")
      ),
      windowTitle = "Buckets Dashboard"),

    # Sidebar with input controls / filters
    sidebarLayout(
        sidebarPanel(width = 2,
                     p(strong("Select months for the reports")),
            # Allow multiple inputs on same row if there is space
            div(style = "display: flex; flex-wrap: wrap;",
              airDatepickerInput("date_range",
                               autoClose = TRUE,
                               update_on = "change",
                               separator = " to ",
                               toggleSelected = FALSE,
                               addon = "none",
                               dateFormat = "yyyy-MMM",
                               range = TRUE,
                               view = "months",
                               minView = "months",
                               todayButton = FALSE,
                               value = c(date_from, date_to),
                               minDate = min(monthly$month),
                               maxDate = max(monthly$month)),
            actionButton(inputId = "select_all_dates", 
                         label = "All Months"),
            actionButton(inputId = "select_current_month", 
                         label = "Current Month"),
            actionButton(inputId = "select_current_year", 
                         label = "Current Year")
                     ), #Div end
            # Input controls for filtering Buckets
            p(strong("Select buckets for the reports"),
              style="margin-bottom: 5px;"),
            # Allow multiple inputs on same row
            div(style = "display: flex; flex-wrap: wrap;",
              actionButton(inputId = "select_all_buckets", 
                         label = "Show All Buckets",
                         style = "margin-bottom: 5px;"),
              pickerInput(inputId = "income_buckets_filter_choices",
                    label = NULL,
                    selected = unlist(income_named_list, use.names = FALSE),
                    choices = income_named_list,
                    multiple = TRUE,
                    width = "fit",
                    options = list(`actions-box` = TRUE,
                                   header = "Income Buckets",
                                   `selected-text-format` = "static",
                                   title = "Select Income Buckets"
                    )
              ),
              pickerInput(inputId = "expense_buckets_filter_choices",
                    label = NULL,
                    selected = unlist(expenses_named_list, use.names = FALSE),
                    choices = expenses_named_list,
                    multiple = TRUE,
                    width = "fit",
                    options = list(`actions-box` = TRUE,
                                   header = "Expense Buckets",
                                   `selected-text-format` = "static",
                                   title = "Select Expense Buckets")
              ),
              actionButton(inputId = "deselect_kicked", 
                     label = "Deselect Kicked Expenses",
                     style="margin-bottom: 5px;")
            ), #div end
        # Input controls for the Income/Expense and Transactions reports
            p(strong("Income/Expense and Transactions")),
            materialSwitch("toggle_report_view",
                           "Only Bucket Groups",
                           width = "auto"),
            materialSwitch("toggle_zero_totals",
                           "Show Buckets With All Zeros",
                           width = "auto"),
            actionButton(inputId = "clear_selection", 
                         label = "Clear Selection"),
        # Input controls for filtering accounts in net wealth report
        p(strong("Net Wealth and Forecast"),
          style="margin-bottom: 5px;"),
        # Allow multiple inputs on same row if there is space
        div(style = "display: flex; flex-wrap: wrap;",
        pickerInput(inputId = "netwealth_account_filter_choices",
                    label = NULL,
                    selected = unlist(accounts_named_list, use.names = FALSE),
                    choices = accounts_named_list,
                    multiple = TRUE,
                    width = "fit",
                    options = list(`actions-box` = TRUE,
                                   header = "Accounts",
                                   `selected-text-format` = "static",
                                   title = "Select Accounts")
        )
        ), #div end
        p(strong("Savings Buckets"),
          style="margin-bottom: 5px;"),
        # Allow multiple inputs on same row if there is space
        div(style = "display: flex; flex-wrap: wrap;",
        actionButton(inputId = "select_config_saving_buckets", 
                     label = "Select Saving Buckets From \"config.R\"",
                     style="margin-bottom: 5px;"),
        pickerInput(inputId = "saving_buckets_filter_choices",
                    label = NULL,
                    selected = savings_buckets,
                    choices = expenses_named_list,
                    multiple = TRUE,
                    width = "fit",
                    options = list(`actions-box` = TRUE,
                                   header = "Saving Buckets",
                                   `selected-text-format` = "static",
                                   title = "Select Saving Buckets")
                    )
        ), #div end
        p(HTML("<b>Stock data</b>"),
          style="margin-bottom: 5px;"),
        # Allow multiple inputs on same row if there is space
        div(style = "display: flex; flex-wrap: wrap;",
        pickerInput(inputId = "stock_account",
                    label = NULL,
                    choices = accounts_named_list,
                    multiple = TRUE,
                    width = "fit",
                    options = list(`actions-box` = TRUE,
                                   header = "Accounts",
                                   `selected-text-format` = "static",
                                   title = "Select the \"stock\" account(s)")),
        uiOutput("stock_transfers"),
        uiOutput("stock_gains")
        ) #div end 
        ), #sidepanel end
        # Use tabsets for the main panel to easily switch between reports
        mainPanel(width = 10, tabsetPanel(
            tabPanel("Income/Expense", 
                     DT::dataTableOutput("expenses_pr_month")
                     ),# end tabpanel
            tabPanel("Transactions", 
                     p(strong("Filter the transactions by selecting cells in the Income/Expense Report."),
                       style = "margin: 5px"),
                     DT::dataTableOutput("transactions_table")),
            tabPanel("Sunburst - Income",
                     p(strong("Click \"Other\" to see the buckets within that group and see \"config.R\" to change the threshold for \"Other\"."),
                       style="margin: 5px;"),
                     plotlyOutput("income_sunburstchart", height = height_income_piechart)),
            tabPanel("Sunburst - Expenses",
                     p(strong("Click a bucket group to see the buckets within that group."),
                       style="margin: 5px;"),
                     plotlyOutput("expense_sunburstchart", height = height_expenses_sunburst)),
            tabPanel("Net Wealth",
                     p(strong("Hover to see amounts."),
                       style="margin: 5px;"),
                     plotlyOutput("net_wealth")),
            tabPanel("Savings Rate",
                     p(strong("Specify standard saving buckets in \"config.R\". Saving buckets are buckets used for transfers to off-budget saving accounts."),
                       style="margin-top: 5px;"),
                     p(strong("Pension contributions - use if pension contributions are kept in off-budget accounts"), style="margin-top: 5px"),
                     pickerInput(inputId = "pension_account",
                                 label = NULL,
                                 choices = accounts_named_list,
                                 multiple = TRUE,
                                 width = "fit",
                                 options = list(`actions-box` = TRUE,
                                                header = "Accounts",
                                                `selected-text-format` = "static",
                                                title = "Select the \"pension\" account(s)")),
                     uiOutput("pension_savings"),
                     DT::dataTableOutput("savings_rate_table")),
            tabPanel("Bucket Balances",
                     p(strong("Always shows the current balance in the Buckets. Not affected by filters."),
                       style="margin-top: 5px;"),
                     materialSwitch("bucket_balances_labels", 
                                    "Show value labels",
                                    value = TRUE),
                     plotlyOutput("bucket_balances", height = height_bucket_balances)),
            tabPanel("Bucket Transactions",
                     p(strong("Select a bucket to show the plot. Buckets without any activities cannot be selected."),
                       style="margin: 5px;"),
                     pickerInput(inputId = "bucket_transactions_selected",
                                 label = NULL,
                                 selected = NULL,
                                 choices = bucket_transactions_list,
                                 multiple = FALSE,
                                 width = "fit",
                                 options = list(`actions-box` = FALSE,
                                                header = "Bucket Transactions",
                                                title = "Select Bucket",
                                                `none-selected-text` = "Select Bucket",
                                                `live-search` = TRUE,
                                                `live-search-normalize` = TRUE)
                     ),
                     plotlyOutput("bucket_transactions", height = height_bucket_transactions)),
            tabPanel("Year Over Year",
                     p(strong("Select a bucket to show the plot. Click the legend or change the date range to hide or show specific years."),
                       style="margin: 5px;"),
                     pickerInput(inputId = "year_over_year_selected",
                                 label = NULL,
                                 selected = NULL,
                                 choices = c(income_named_list, expenses_named_list),
                                 multiple = FALSE,
                                 width = "fit",
                                 options = list(`actions-box` = FALSE,
                                                header = "Year Over Year",
                                                title = "Select Bucket",
                                                `none-selected-text` = "Select Bucket",
                                                `live-search` = TRUE,
                                                `live-search-normalize` = TRUE)
                     ),
                     plotlyOutput("year_over_year", height = height_year_over_year)),
            tabPanel("Sankey",
                     p(strong("Select saving buckets in the dropdown menu or specify them in \"config.R\"."),
                       style = "margin: 0 0 0 5px;"),
                     p(strong("Use the date and bucket filters on the left to customize the plot."),
                       style = "margin: 0 0 0 5px;"),
                     p(strong("Cannot plot expense buckets with a positive total activity. Cannot plot income or savings buckets with a negative total activity."),
                       style="margin: 0 0 5px 5px;"),
                     plotlyOutput("sankey", height = height_sankey)),
            tabPanel("Forecast",
                     p(strong("Forecasting by sampling historical data (monthly). Inspired by YNAB Toolkit."),
                       style = "margin: 0 0 0 5px;"),
                     p(strong("Use the date, account and bucket filters on the left to customize the data used for forecasting."),
                       style = "margin: 0 0 0 5px;"),
                     plotlyOutput("forecast", height = height_sankey)),
            tabPanel("Stocks",
                     fluidPage(
                       fluidRow(
                         column(width = 3,
                                numericInput("stock_time", "Number of years to forecast", 10, min = 1, max = 50),
                                numericInput("stock_start_value",
                                             HTML("Initial value of stocks before forecasting<br>(leave blank to use the historical data)"),
                                             value = NA_real_),
                                numericInput("stock_invested_per_month",
                                             HTML("Amount invested per month<br>(leave blank to use the historical data)"),
                                             value = NA_real_),
                                numericInput("stock_gains_per_year",
                                             HTML("Gains per year in percent<br>(leave blank to use the historical data)"),
                                             value = NA_real_),
                                radioButtons("stock_mean_sample",
                                             label = "When forecasting based on historical data either sample from historical data or use mean values for monthly gains and transfers",
                                             choices = c("Sample", "Mean"),
                                             selected = c("Sample")),
                                numericInput("stock_nsims",
                                             label = "Number of simulations when sampling using historical data",
                                             value = 101,
                                             min = 2,
                                             max = 10000,
                                             step = 1)
                         ),
                         column(width = 9,
                                plotlyOutput("stock_historical")
                                )
                       ),
                       fluidRow(
                         column(width = 12),
                         uiOutput("stock_forecast")
                        )
                       )
                     ),
            tabPanel("Gains vs Expenses",
                     #plotlyOutput("gains_vs_expenses_plot")
                       
            )
            )
          )
        )
    )
)

    
