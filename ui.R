# Define UI
shinyUI(fluidPage(
    # Adjust zoom to fit 1 year without horizontal scrolling
    tags$style(
        zoom_css
    ),
    
    
    # Application title
    titlePanel("Buckets Dashboard"),

    # Sidebar with input controls / filters
    sidebarLayout(
        sidebarPanel(width = 2,
            # Input of date range for filtering
            airDatepickerInput("date_range",
                               "Select months for the reports",
                               autoClose = TRUE,
                               update_on = "change",
                               separator = " to ",
                               toggleSelected = FALSE,
                               addon = "none",
                               dateFormat = "yyyy-M",
                               range = TRUE,
                               view = "months",
                               minView = "months",
                               todayButton = FALSE,
                               value = c(date_from, date_to),
                               minDate = min(monthly$month),
                               maxDate = max(monthly$month)),
            actionButton(inputId = "select_all_dates", 
                         label = "All Months",
                         style="margin-bottom: 5px; margin-top: -10px;"),
            actionButton(inputId = "select_current_month", 
                         label = "Current Month",
                         style="margin-bottom: 5px; margin-top: -10px;"),
            br(),
            actionButton(inputId = "select_current_year", 
                         label = "Current Year",
                         style="margin-bottom: 5px;"),
            # Input controls for filtering Buckets
            p(strong("Select buckets for the reports"),
              style="margin-bottom: 5px;"),
            actionButton(inputId = "select_all_buckets", 
                         label = "Show All Buckets",
                         style="margin-bottom: 5px;"),
        dropdown(
            inputId = "income_buckets_filter",
            label = "Select Income Buckets",
            prettyCheckboxGroup(inputId = "income_buckets_filter_choices",
                                label = "Income Buckets",
                                selected = levels(monthly %>% filter(bucket_group == "Income") %>% pull(category) %>% fct_drop()),
                                choices = levels(monthly %>% filter(bucket_group == "Income") %>% pull(category) %>% fct_drop()))),
        tags$div(style = "height: 5px;"),
        dropdown(
            inputId = "expense_buckets_filter",
            label = "Select Expense Buckets",
            prettyCheckboxGroup(inputId = "expense_buckets_filter_choices",
                                label = "Expense Buckets",
                                selected = levels(monthly %>% filter(bucket_group != "Income") %>% pull(category) %>% fct_drop()),
                                choices = levels(monthly %>% filter(bucket_group != "Income") %>% pull(category) %>% fct_drop()))),
        tags$div(style = "height: 15px;"),
        
        # Input controls for the Income/Expense report
        p(strong("Income/Expense Report View"),
          style="margin-bottom: 5px;"),
        actionButton(inputId = "toggle_report_view", 
                     label = "Toggle: Buckets / Bucket Groups",
                     style="margin-bottom: 5px;"),
        actionButton(inputId = "toggle_zero_totals", 
                     label = "Toggle: Buckets With Zero Total",
                     style="margin-bottom: 5px;"),
        actionButton(inputId = "clear_selection", 
                     label = "Clear Selection",
                     style="margin-bottom: 5px;"),
        
        # Input controls for filtering accounts in net wealth report
        p(strong("Select accounts for Net Wealth"),
          style="margin-bottom: 5px;"),
        actionButton(inputId = "select_all_accounts", 
                     label = "Show All Accounts",
                     style="margin-bottom: 5px;"),
        dropdown(
          inputId = "netwealth_account_filter",
          label = "Select Accounts",
          prettyCheckboxGroup(inputId = "netwealth_account_filter_choices",
                              label = "Accounts",
                              selected = unique(assets_liabilities$name),
                              choices = unique(assets_liabilities$name))),
        p(strong("Savings Rate View"),
          style="margin-bottom: 5px;"),
        actionButton(inputId = "select_config_saving_buckets", 
                     label = "Select Saving Buckets From \"config.R\"",
                     style="margin-bottom: 5px;"),
        dropdown(
          inputId = "saving_buckets_filter",
          label = "Select Saving Buckets",
          prettyCheckboxGroup(inputId = "saving_buckets_filter_choices",
                              label = "Saving Buckets",
                              selected = savings_buckets,
                              choices = levels(monthly %>% filter(bucket_group != "Income") %>% pull(category) %>% fct_drop())))
        ),
        
        # Use tabsets for the main panel to easily switch between reports
        mainPanel(width = 10, tabsetPanel(
            tabPanel("Income/Expense Report", DT::dataTableOutput("expenses_pr_month")),
            tabPanel("Transactions", 
                     p(strong("Filter the transactions by selecting cells in the Income/Expense Report"),
                       style = "margin: 5px"),
                     DT::dataTableOutput("transactions_table")),
            tabPanel("Sunburst Chart - Income",
                     p(strong("Click \"Other\" to see the buckets within that group and see \"config.R\" to change the threshold for \"Other\"")),
                     plotlyOutput("income_sunburstchart", height = height_income_piechart)),
            tabPanel("Sunburst Chart - Expenses",
                     p(strong("Click a bucket group to see the buckets within that group")),
                     plotlyOutput("expense_sunburstchart", height = height_expenses_sunburst)),
            tabPanel("Net Wealth",
                     p(strong("Hover to see amounts")),
                     div(style = zoom_netwealth,
                         plotlyOutput("net_wealth"))),
            tabPanel("Savings Rate",
                     p(strong("Specify standard saving buckets in \"config.R\". Savings buckets are buckets used for transfers to off-budget saving accounts.")),
                     DT::dataTableOutput("savings_rate_table"))
                     
            )
        )
    )
    )
    )

    
