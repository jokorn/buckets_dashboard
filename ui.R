# Define UI
shinyUI(fluidPage(

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
                     label = "Toggle: Buckets Without Transactions",
                     style="margin-bottom: 5px;"),
        actionButton(inputId = "clear_selection", 
                     label = "Clear Selection",
                     style="margin-bottom: 5px;")
        ),
        
        # Use tabsets for the main panel to easily switch between reports
        mainPanel(tabsetPanel(
            tabPanel("Income/Expense Report", DT::dataTableOutput("expenses_pr_month")),
            tabPanel("Transactions", 
                     p(strong("Filter the transactions by selecting cells in the Income/Expense Report"),
                       style = "margin: 5px"),
                     DT::dataTableOutput("transactions_table")),
            tabPanel("Sunburst Chart - Income",
                     p(strong("Hover to see amounts and click \"Other\" to see the buckets within that group")),
                     plotlyOutput("income_sunburstchart", height = height_income_piechart)),
            tabPanel("Sunburst Chart - Expenses",
                     p(strong("Hover to see amounts and click a bucket group to see the buckets within that group")),
                     plotlyOutput("expense_sunburstchart", height = height_expenses_sunburst))
                     
            )
        )
    )
    )
    )

    
