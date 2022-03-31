# Load libraries - make sure to install them first with install.packages
library(tidyverse)
library(lubridate)
library(DBI)
library(RSQLite)
library(dplyr)
library(shiny)
library(shinyWidgets)
library(shinyjs)
library(DT)
library(janitor)
library(plotly)

# Load the config file
source("config.R")

# Convenience function for easy conversion to user's currency (variables from config.R)
format_currency <- function(value) {
  scales::dollar(value,
                 big.mark = user_mark,
                 decimal.mark = user_dec.mark,
                 accuracy = 0.01,
                 prefix = ifelse(currency_before, user_currency, ""),
                 suffix = ifelse(!currency_before, user_currency, ""))
}

# Hovertemplate for plotly plots
hovertemplate <- glue::glue("%{{x|%b %Y}}: %{{y:{plotly_separators}0f}}")

# Variables for correct zoom based on user input in config.R
zoom_css <- glue::glue("
            body {{
               -moz-transform: scale({zoom_value}, {zoom_value});
               zoom: {zoom_value};
               zoom: {scales::percent(zoom_value, accuracy = 1)};
            }}
            ")

zoom_reverse <- glue::glue("
            .plotly.html-widget {{
            moz-transform: scale({1/zoom_value}, {1/zoom_value});
            zoom: {1/zoom_value};
            zoom: {scales::percent(1/zoom_value, accuracy = 1)};
            }}")   # Zoom messes up hover in the bar chart (Net Wealth) so must be reversed. 
                 # The values here should be 1/[value in zoom_value], 
                 # e.g. 1/80% = 125%

error_position_css <- glue::glue(".shiny-output-error-validation {{
              position: absolute !important;
              top: {error_position_top} !important;
              left: {error_position_left} !important;
              display: block !important;
              height: 0px !important;
              }}")

form_group_css <- ".form-group {margin-bottom: 5px;}"

# Connect to database and the relevant tables
con <- dbConnect(SQLite(), path_to_buckets)
transactions <- tbl(con, "bucket_transaction") %>% collect()
buckets <- tbl(con, "bucket") %>% collect()
bucket_groups <- tbl(con, "bucket_group") %>% collect()
income <- tbl(con, "account_transaction") %>% collect()
acc_balance <- tbl(con, "account") %>% collect()
acc_trans <- tbl(con, "account_transaction") %>% collect()

# Add bucket groups to buckets
buckets_ready <- buckets %>% 
  select(bucket_id = id, 
         group_id,
         balance,
         ranking,
         category = name,
         kicked) %>%
  left_join(bucket_groups %>% 
              select(group_id = id,
                     bucket_group = name),
            by = "group_id") %>% 
  arrange(group_id) %>% 
  mutate(bucket_group = ifelse(kicked == 1, "Kicked", bucket_group)) %>% 
  mutate(category = ifelse(kicked == 1, str_c(category, " (kicked)"), category)) %>% 
  mutate(group_id = ifelse(bucket_group == "Kicked", 9999, group_id)) %>% 
  mutate(bucket_group = fct_reorder(bucket_group, group_id))

# Prepare income
income_ready <- income %>% 
  filter(general_cat == "income") %>% 
  mutate(bucket_group = "Income",
         group_id = -99,
         ranking = memo) %>% 
  left_join(acc_balance %>% select(account = name, id),
            by = c("account_id" = "id")) %>% 
  select(account,
         bucket_group,
         category = memo,
         posted,
         amount,
         group_id,
         ranking)

# Prepare expenses
expenses_ready <- transactions %>% 
  filter(!is.na(account_trans_id)) %>% 
  left_join(acc_trans %>% select(id, account_id),
            by = c("account_trans_id" = "id")) %>% 
  left_join(acc_balance %>% select(account = name, id),
            by = c("account_id" = "id")) %>% 
  select(account, id, posted, bucket_id, amount, memo) %>% 
  left_join(buckets_ready,
            by = "bucket_id")

# Merge the income and expenses tables and keep relevant columns only
everything <- bind_rows(income_ready,
                        expenses_ready) %>% 
  mutate(posted = ymd(str_sub(posted, 1, 10))) %>% 
  # Apply manual overrides
  left_join(manual_categorization,
            by = c("bucket_group", "category")) %>% 
  mutate(category = coalesce(category_new, category),
         bucket_group = coalesce(bucket_group_new, bucket_group),
         group_id = coalesce(group_id_new, group_id),
         ranking = coalesce(ranking_new, ranking)) %>% 
  # Ensure sorting
  arrange(group_id, ranking) %>% 
  mutate(category = factor(category) %>% fct_inorder) %>% 
  mutate(bucket_group = fct_reorder(bucket_group, group_id))

# Create monthly summary
monthly <- everything %>% 
  mutate(month = floor_date(posted, "month")) %>% 
  group_by(bucket_group, category, month) %>% 
  summarize(amount = sum(amount) / 100) %>% 
  ungroup() %>% 
  arrange(month) %>% 
  complete(nesting(category, bucket_group), month, fill = list(amount = 0)) %>% 
  relocate(bucket_group)

# Buckets summary pr month
buckets_monthly <- transactions %>%
  left_join(buckets, by = c("bucket_id" = "id")) %>%
  left_join(bucket_groups %>% select(id, bucket_group = name), by = c("group_id" = "id")) %>% 
  mutate(transaction = !is.na(account_trans_id)) %>%
  mutate(name = ifelse(kicked == 1, str_c(name, " (kicked)"), name)) %>% 
  mutate(bucket_group = ifelse(kicked == 1, "Kicked", bucket_group)) %>% 
  mutate(positive = amount > 0) %>%
  mutate(category = case_when(transaction & positive ~ "Transaction - Ingoing",
                              transaction & !positive ~ "Transaction - Outgoing",
                              !transaction & positive ~ "Budgeted - Ingoing",
                              !transaction & !positive ~ "Budgeted - Outgoing")) %>%
  select(posted,
         amount,
         name,
         bucket_group,
         category) %>%
  mutate(bucket_group = ifelse(is.na(bucket_group), "Kicked", bucket_group)) %>% 
  mutate(amount = amount / 100) %>%
  mutate(posted = floor_date(ymd(str_sub(posted, 1, 10)), "month")) %>%
  group_by(posted, bucket_group, name, category) %>%
  summarise(amount = sum(amount)) %>%
  mutate(name = factor(name),
         category = factor(category,
                           levels = c("Budgeted - Ingoing",
                                      "Transaction - Ingoing",
                                      "Budgeted - Outgoing",
                                      "Transaction - Outgoing")))%>%
  tidyr::complete(nesting(name, bucket_group), category, posted, fill = list(amount = 0)) %>% 
  distinct()

# Create account balances from end balance and transactions
assets_liabilities <- acc_trans %>%
  # Report last date in each month for easy matching with buckets reports
  # NB: There is a bug in Buckets so the last transactions in each month is not included
  # in the monthly net wealth report https://github.com/buckets/application/issues/542
  mutate(month = floor_date(str_sub(posted, 1,10) %>%
                              ymd(),"month")) %>% 
  # We need at least one transaction per month for accumulate to work correctly
  tidyr::complete(month, account_id, fill = list(amount = 0)) %>% 
  # Calculate net flow pr account pr month
  group_by(account_id, month) %>% 
  summarise(net_flow = sum(amount/100)) %>% 
  ungroup() %>% 
  # Add the end balance and account names
  right_join(acc_balance %>% select(account_id = id,
                                   end_balance = balance,
                                   name),
            by = "account_id") %>% 
  tidyr::complete(month, tidyr::nesting(account_id, name, end_balance), fill = list(amount = 0)) %>% 
  filter(!is.na(month)) %>% 
  mutate_if(is.numeric, ~replace_na(.x, 0)) %>% 
  # Prepare to accumulate = take end balance and then pr month add net flow (cumulatively)
  group_by(account_id, name, end_balance) %>%
  group_nest() %>% 
  # Do the actual accumulation
  mutate(balance = map2(end_balance, data, ~ .y %>%
                       arrange(desc(month)) %>%
                       mutate(net_flow = net_flow*-1) %>%
                       add_case(month = .$month[1] %m+% months(1), net_flow = .x/100) %>% 
                       arrange(desc(month)) %>%
                       mutate(month = month - days(1)) %>% 
                       mutate(balance = accumulate(net_flow, sum)))) %>% 
  unnest(balance) %>% 
  # Add assets/liabilities. Note that this means that these categories are pr 
  # month and account meaning that one account can be both an asset and a liability if
  # if the balance is positive in some months and negative in others
  mutate(account_category = if_else(balance < 0, "Liabilities", "Assets"))


# Create data to use with forecasting - includes ALL transactions onbudget and offbudget
all_transactions <- acc_trans %>%
  left_join(transactions %>% select(bucket_id, account_trans_id),
            by = c("id" = "account_trans_id")) %>%
  left_join(buckets_ready, by = "bucket_id") %>%
  mutate(category = ifelse(general_cat == "income", memo, category)) %>%
  mutate(bucket_group = ifelse(general_cat == "income", "Income", as.character(bucket_group))) %>%
  mutate(category = ifelse(is.na(category), "Off-budget", category)) %>%
  mutate(bucket_group = ifelse(category == "Off-budget", "Off-budget", bucket_group)) %>%
  left_join(acc_balance %>% select(id, account = name),
            by = c("account_id" = "id")) %>% 
  select(posted, account, category, bucket_group, memo, amount) %>% 
  mutate(posted = ymd(str_sub(posted, 1, 10))) %>% 
  mutate(month = floor_date(posted, "month"))

# Create date variable for use in UI/server 
dates_available <- range(monthly$month) 

# Set reporting period
date_from <- max(today() %>% floor_date("year"), dates_available[1])
date_to <- today()

# Create named lists with expenses, income and accounts for the dropdown menus
income_named_prepare <- monthly %>%
  filter(bucket_group == "Income") %>%
  distinct(bucket_group, category)

income_named_list <- lapply(split(as.character(income_named_prepare$category),
                                  income_named_prepare$bucket_group),
                            as.list)

expenses_named_prepare <- buckets_ready %>%
  distinct(bucket_group, category)

expenses_named_list <- lapply(split(expenses_named_prepare$category, 
                                    expenses_named_prepare$bucket_group),
                              as.list)

bucket_transactions_list_prepare <- buckets_ready %>%
  distinct(bucket_group, category)

bucket_transactions_list <- lapply(split(bucket_transactions_list_prepare$category,
                                         bucket_transactions_list_prepare$bucket_group),
                                   as.list)

accounts_named_prepare <- acc_balance %>% 
  mutate(category = case_when(closed == 1 ~ "Closed",
                              kind == "offbudget" ~ "Off Budget",
                              TRUE ~ "On Budget")) %>% 
  mutate(name = fct_inorder(name)) %>% 
  mutate(category = factor(category,
                           levels = c("On Budget",
                                      "Off Budget",
                                      "Closed"))) %>% 
  select(name, category) %>% 
  arrange(category, name)

accounts_named_list <- lapply(split(as.character(accounts_named_prepare$name),
                                    accounts_named_prepare$category),
                              as.list)


# Disconnect from DB
dbDisconnect(con)

# Table for the expense/income report
expense_income_table <- function(data_source,
                                 date_filter,
                                 buckets_filter,
                                 bucketgroups_view = FALSE,
                                 show_zero_totals = TRUE) {

  # Switch depending on whether it is buckets or bucket groups view
  if (bucketgroups_view) {
    
    data_source_prepare <- data_source %>% 
      filter(category %in% buckets_filter) %>% 
      group_by(bucket_group, month) %>% 
      summarize(amount = sum(amount)) %>% 
      ungroup() %>% 
      mutate(category = bucket_group) %>% 
      mutate(month = strftime(month, format = "%Y-%b")) %>%
      pivot_wider(names_from = month, values_from = c(amount)) %>% 
      rowwise() %>% 
      mutate(Average = mean(c_across(all_of(strftime(date_filter, format = "%Y-%b")))),
             Total = sum(c_across(all_of(strftime(date_filter, format = "%Y-%b"))))) %>% 
      ungroup()
    
  } else {
    
    data_source_prepare <- data_source %>% 
      filter(category %in% buckets_filter) %>% 
      mutate(month = strftime(month, format = "%Y-%b")) %>% 
      pivot_wider(names_from = month, values_from = amount) %>% 
      left_join(data_source %>% 
                  group_by(bucket_group, category) %>% 
                  summarize(Average = mean(amount) %>% round(2),
                            Total = sum(amount) %>% round(2)) %>% 
                  ungroup(),
                by = c("bucket_group", "category")) %>% 
      arrange(bucket_group, category)
  }
  
  # Depending on actionbutton status, filter out columns with total = 0
  if (show_zero_totals == FALSE){
    data_source_prepare <- data_source_prepare %>% 
      filter(if_any(is.numeric, ~ .x != 0))
  }
  
  # Add "total" row at the bottom
  data_source_ready <- data_source_prepare %>% 
    rename(Bucket = category) %>% 
    adorn_totals(fill = "Total")
  
  # Depending on the view, use rowGroup extension in the datatable
  if (bucketgroups_view) {
    datatable_prepare <- datatable(data_source_ready,
                                 options = list(dom = "t", 
                                                paging = FALSE,
                                                scrollY = height_expense_report,
                                                scrollX = TRUE,
                                                scrollCollapse = TRUE,
                                                columnDefs = list(list(visible=FALSE, targets=c(0))),
                                                fixedHeader = TRUE),
                                 rownames = FALSE,
                                 selection = list(target = "cell"))
  } else {
    datatable_prepare <- datatable(data_source_ready,
                                 options = list(dom = "t", 
                                                paging = FALSE,
                                                scrollY = height_expense_report,
                                                scrollX = TRUE,
                                                scrollCollapse = TRUE,
                                                rowGroup = list(dataSrc = 0),
                                                columnDefs = list(list(visible=FALSE, targets=c(0))),
                                                fixedHeader = TRUE),
                                 rownames = FALSE,
                                 extensions = 'RowGroup',
                                 selection = list(target = "cell"))
  }
  
  # Format columns with colors, string formatm etc.
  datatable_prepare %>% 
    formatStyle(c(date_filter %>% strftime("%Y-%b"),
                "Average",
                "Total"),
                "white-space" = "nowrap",
              color = DT::styleInterval(cuts = c(-0.0000001, 0.0000001),
                                        values = c("red", "gray", "green"))) %>%
    formatCurrency(c(date_filter %>% strftime("%Y-%b"),
                   "Average",
                   "Total"), 
                   currency = user_currency, 
                   interval = 3,
                   mark = user_mark,
                   dec.mark = user_dec.mark, 
                   before = currency_before)
}

transactions_table <- function(data_source,
                               cells_filter,
                               buckets_filter,
                               bucketgroups_view = FALSE,
                               expense_report) {
  
  # If cells are selected in the income/expense report, then filter based on these
  if (ncol(cells_filter) > 0){
    expense_report_data <- expense_report$x$data
    cells_filter_months <- colnames(expense_report_data)[cells_filter[,2] + 1] #+1 due to hidden column with bucket_group
    # income/expense report is slightly different in bucket vs bucket groups view
    if (bucketgroups_view == FALSE){
      cells_filter_categories <- expense_report_data[,2][cells_filter[,1]]
    } else {
      cells_filter_categories <- expense_report_data[,1][cells_filter[,1]]
    }
    
    # String for easy filtering = combination of categories and months
    filter_string <- str_c(cells_filter_categories, cells_filter_months)
    
    # Do the filtering
    data_source_prepare <- data_source %>%  
      filter(category %in% buckets_filter) %>%
      mutate(bucketgroups_view = bucketgroups_view) %>% 
      mutate(filter_str = if_else(bucketgroups_view,
                                  as.character(bucket_group),
                                  as.character(category))) %>% 
      filter(str_c(filter_str, strftime(posted, "%Y-%b")) %in% filter_string)
  } else {
    # Always filter based on buckets chosen in side menu in ui
    data_source_prepare <- data_source %>% 
    filter(category %in% buckets_filter)
  }
  
  # Finalize data and keep only relevant columns
  data_source_ready <- data_source_prepare %>% 
    mutate(row_num = row_number()) %>% 
    arrange(desc(row_num), posted) %>%
    mutate(amount = amount / 100) %>% 
    select("Date" = posted,
           "Bucket Group" = bucket_group,
           "Bucket" = category,
           "Amount" = amount,
           "Memo" = memo)
  
  # Create the datatable. Fixed header not possible due to bug with multiple 
  # datatables in tabsets, so use scrollY instead
  datatable(data_source_ready,
            options = list(dom = "fti", 
                           order = list(list(1, 'desc')),
                           paging = FALSE,
                           scrollY = height_transactions_report,
                           scrollCollapse = TRUE)) %>% 
    formatStyle("Amount",
                color = DT::styleInterval(cuts = c(-0.001, 0.001),
                                          values = c("red", "gray", "green"))) %>%
    formatCurrency("Amount",
                   currency = user_currency,
                   interval = 3,
                   mark = user_mark,
                   dec.mark = user_dec.mark,
                   before = currency_before)
}

plot_net_wealth <- function(assets_liabilities,
                            input_date_range,
                            input_accounts) {
  
  plot_assets_liabilities <- assets_liabilities %>% 
    # Filter based on user input
    filter(month >= input_date_range[1],
           month <= input_date_range[2] %m+% months(1)) %>% 
    filter(name %in% input_accounts) %>% 
    # Summarize pr assets/liabilites and month
    group_by(month, account_category) %>% 
    summarize(amount = sum(balance)) %>%
    ungroup() %>%
    mutate(account_category = factor(account_category, levels = c("Assets", "Liabilities"))) %>% 
    tidyr::complete(month, account_category, fill = list(amount = 0)) %>% 
    mutate(account_category = as.character(account_category)) %>% 
    # plotly prefers wide format
    pivot_wider(id_cols = month,
                names_from = account_category,
                values_from = amount) %>%
    # Plot all values as positive (liabilities are implicit negative)
    mutate(Liabilities = -1*Liabilities) %>%
    mutate(net = Assets-Liabilities) %>% 
    mutate(month = floor_date(month, "month"))

  # Create title with changes from start to end
  start_value = plot_assets_liabilities %>% filter(month == min(month)) %>% pull(net)
  end_value = plot_assets_liabilities %>% filter(month == max(month)) %>% pull(net)
  change_in_percent = scales::percent((end_value - start_value)/abs(start_value), accuracy = 0.1)
  
  # Make the title
  netwealth_title = str_c("Change from ", format_currency(start_value), " to ",
                          format_currency(end_value), " = ", format_currency(end_value - start_value),
                          " (", change_in_percent,")")
  
  plot_ly(plot_assets_liabilities,
          x = ~month,
          y=~Assets,
          type = "bar",
          name = "Assets",
          hovertemplate = hovertemplate,
          marker = list(color = "green")) %>% 
    add_trace(y =~Liabilities,
              data = plot_assets_liabilities,
              hovertemplate = hovertemplate,
              name = "Liabilities",
              marker = list(color = "red")) %>%
    add_trace(y =~net,
              hovertemplate = hovertemplate,
              data = plot_assets_liabilities,
              name = "Net Worth",
              type = "scatter",
              mode = "lines+markers",
              marker = list(color = "black", size = 10),
              line = list(color = "black")) %>%
    layout(yaxis = list(title = ""),
           xaxis = list(title = ""),
           legend = list(x = 0, y = 1.15),
           barmode = "group",
           title = netwealth_title) %>% 
    config(displayModeBar = FALSE) %>% 
    layout(separators = plotly_separators)
  
}


savings_rate <- function(monthly,
                         input_date_range,
                         input_saving_buckets_filter_choices) {
  
  # Separate into Income, Savings (based on buckets in config.R) and Expenses
  prepare_savings_rate <- monthly %>% 
    filter(month >= input_date_range[1],
           month <= input_date_range[2]) %>% 
    mutate(savings_cat = case_when(bucket_group == "Income" ~ "Income",
                                   category %in% input_saving_buckets_filter_choices ~ "Savings",
                                   TRUE ~ "Expenses")) %>%
    # Calculate per month and make wide
    group_by(month, savings_cat) %>% 
    summarize(amount = sum(amount)) %>%
    ungroup() %>% 
    mutate(savings_cat = factor(savings_cat, levels = c("Income",
                                                        "Expenses",
                                                        "Savings"))) %>% 
    tidyr::complete(month, savings_cat, fill = list(amount = 0)) %>% 
    mutate(savings_cat = as.character(savings_cat)) %>% 
    pivot_wider(names_from = savings_cat,
                values_from = amount,
                id_cols = month) %>%
    mutate(Savings = Savings * -1) %>% 
    mutate(not_spend = Income + Expenses - Savings) %>%
    mutate(percent_notspend = not_spend / Income,
           percent_savings = Savings / Income,
           savings_notspend = not_spend+Savings,
           percent_savings_and_notspend = savings_notspend / Income)
  
  # Calculate totals based on the above
  total_savings_rate <- summarise(prepare_savings_rate,
                                  month = "Total",
                                  Income = sum(Income),
                                  Expenses = sum(Expenses),
                                  "Saving buckets" = sum(Savings)) %>% 
    mutate("Not spend" = Income + Expenses - `Saving buckets`,
           "Not spend + Saving buckets" = `Not spend` + `Saving buckets`,
           "Proportion Not spend" = `Not spend` / Income,
           "Proportion Saving buckets" = `Saving buckets` / Income,
           "Proportion Not spend + Saving buckets" = `Not spend + Saving buckets` / Income) %>% 
    # Format the columns as currency and percent
    mutate(Income = format_currency(Income),
           Expenses = format_currency(Expenses),
           `Saving buckets` = format_currency(`Saving buckets`),
           `Not spend + Saving buckets` = format_currency(`Not spend + Saving buckets`),
           `Not spend` = format_currency(`Not spend`)) %>% 
    mutate(`Proportion Not spend` = scales::percent(`Proportion Not spend`, accuracy = 0.1),
           `Proportion Saving buckets` = scales::percent(`Proportion Saving buckets`, accuracy = 0.1),
           `Proportion Not spend + Saving buckets` = scales::percent(`Proportion Not spend + Saving buckets`, accuracy = 0.1)) %>% 
    # Trick to make wide with regards to months
    column_to_rownames("month") %>% 
    t() %>% 
    as.data.frame() %>%
    rownames_to_column(" ")
    
  
  final_savings_rate <- prepare_savings_rate %>% 
    # Format columns as currency and percent
    mutate(Income = format_currency(Income),
           Expenses = format_currency(Expenses),
           Savings = format_currency(Savings),
           savings_notspend = format_currency(savings_notspend),
           not_spend = format_currency(not_spend)) %>% 
    mutate(percent_notspend = scales::percent(percent_notspend, accuracy = 0.1),
           percent_savings = scales::percent(percent_savings, accuracy = 0.1),
           percent_savings_and_notspend = scales::percent(percent_savings_and_notspend, accuracy = 0.1)) %>% 
    # Align months with the rest of the Dahsboard
    mutate(month = strftime(month, "%Y-%b")) %>% 
    # Remove expenses and order the rows
    select(month,
           Income,
           Expenses,
           "Saving buckets" = Savings,
           "Not spend" = not_spend,
           "Not spend + Saving buckets" = savings_notspend,
           "Proportion Saving buckets" = percent_savings,
           "Proportion Not spend" = percent_notspend,
           "Proportion Not spend + Saving buckets" = percent_savings_and_notspend) %>% 
    # Trick to make wide with regards to months
    column_to_rownames("month") %>% 
    t() %>% 
    as.data.frame() %>%
    rownames_to_column(" ") %>% 
    # Add the "Total" column 
    left_join(total_savings_rate, by = " ")
  
  # Create the datatable
  final_savings_rate %>% 
    datatable(rownames = FALSE,
              selection = "none",
              extensions = 'FixedColumns',
              options = list(dom = "t", 
                             paging = FALSE,
                             scrollX = TRUE,
                             fixedColumns = list(leftColumns = 1))) %>% 
    formatStyle(" ",
                fontWeight = "bold")
}


plot_bucket_balance <- function(buckets_ready) {
  
  # Make a plot per bucket group
  bucket_plots <- buckets_ready %>%
    # Balance correction
    mutate(balance = balance / 100) %>% 
    # Enforce order of buckets similar to the one in Buckets app
    arrange(group_id, ranking) %>%
    mutate(category = fct_inorder(category) %>% fct_rev()) %>%
    mutate(bucket_group = fct_inorder(bucket_group)) %>%
    # Remove buckets not in a bucket group
    filter(!is.na(bucket_group)) %>% 
    # and remove buckets that were kicked
    filter(bucket_group != "Kicked") %>% 
    # Nest so we can make a plot per bucket_group
    group_nest(bucket_group) %>%
    # Create an index for the bucket_group - needed as input when plotting
    # to create the titles per bucket group
    mutate(row_num = row_number()) %>% 
    # Calculate height of the plot based on number of buckets in the bucket group
    # plus some (set to 1.5 here) for margins and titles
    mutate(height = map_int(data, nrow) + 1.5) %>% 
    mutate(height = height / sum(height)) %>% 
    # Create the actual plot
    mutate(p = pmap(list(data = data,
                         row_num = row_num,
                         bucket_group = bucket_group),
                    function(data, row_num, bucket_group) { 
                      data %>% 
                        # Only plot the buckets within the bucket group
                        mutate(category = fct_drop(category)) %>% 
                        # Create the plotly bar chart
                        plot_ly(x = ~balance,
                                y = ~category,
                                color = bucket_group,
                                type = "bar",
                                hovertemplate = glue::glue("%{{y}}: %{{x:{plotly_separators}0f}}")) %>% 
                        # Use annotations to create the title per subplot
                        # This is where the index (row_num) is needed for correct placement of the title
                        add_annotations(
                          x = 0,
                          y = 1,
                          xref = str_c("x", row_num, " domain"),
                          yref = str_c("y", row_num, " domain"),
                          text = str_c("<b>",bucket_group,"</b>"),
                          xanchor = "left",
                          yanchor = "bottom",
                          showarrow = F) %>% 
                        # Make sure we show all the buckets with dtick
                        layout(yaxis = list(type = "category", dtick = 1),
                               separators = plotly_separators)}))
  
  # Now put all the separate bucket group plots together using subplot
  bucket_plots %>% 
    pull(p) %>% 
    subplot(nrows = nrow(bucket_plots),
            margin = c(0, 0, 0.005, 0.01),
            shareX = TRUE,
            shareY = TRUE,
            titleY = FALSE,
            titleX = FALSE,
            heights = bucket_plots$height) %>% 
    layout(showlegend = FALSE) %>% 
    config(displayModeBar = FALSE)
           
}

# Get the label for the selected value using JS
js_year_over_year_bucket_transactions <- 'shinyjs.get_year_over_year_bucket_group = function() {
                        var bucket_group_yoy = document.querySelector("#year_over_year_selected option:checked").parentElement.label ??= "";
                        Shiny.setInputValue("year_over_year_bucket_group", bucket_group_yoy);
                      };
                      shinyjs.get_bucket_transactions_bucket_group = function() {
                        var bucket_group_bucket_transactions = document.querySelector("#bucket_transactions_selected option:checked").parentElement.label ??= "";
                        Shiny.setInputValue("bucket_transactions_bucket_group", bucket_group_bucket_transactions);
                      };
                      '

plot_bucket_transactions <- function(buckets_monthly,
                                     input_date_range,
                                     input_bucket_selected,
                                     input_bucket_group) {

  bucket_plot <- buckets_monthly %>% 
    filter(name == input_bucket_selected,
           bucket_group == input_bucket_group) %>% 
    filter(posted >= dates_available[1] &
             posted <= dates_available[2])
  
  if (nrow(bucket_plot) == 0) {
    return(plot_ly() %>% 
             layout(xaxis = list(visible = FALSE),
                    yaxis = list(visible = FALSE),
                    title = "No data available for this bucket"))
  }
  
  bucket_balance <- bucket_plot %>% 
    group_by(posted) %>% 
    summarise(change = sum(amount)) %>% 
    arrange(posted) %>% 
    mutate(balance = accumulate(change, `+`))
  
  bucket_plot_ready <- bucket_plot %>% 
    filter(posted >= input_date_range[1] &
             posted <= input_date_range[2])
  
  bucket_balance_ready <- bucket_balance %>% 
    filter(posted >= input_date_range[1] &
             posted <= input_date_range[2])

  plot_ly() %>%
    add_bars(data = bucket_plot_ready %>%
               filter(category %>% str_detect("Outgoing")),
             x = ~posted,
             y = ~ amount,
             hovertemplate = hovertemplate,
             color = ~category) %>%
    add_bars(data = bucket_plot_ready %>%
               filter(category %>% str_detect("Ingoing")),
             x = ~posted,
             y = ~ amount,
             hovertemplate = hovertemplate,
             base = 0,
             color = ~category) %>%
    add_trace(data = bucket_balance_ready,
              x = ~posted,
              y = ~balance,
              type = "scatter",
              mode = "lines+markers",
              marker = list(color = "black", size = 10),
              line = list(color = "black"),
              hovertemplate = hovertemplate,
              name = "Balance") %>% 
    layout(yaxis = list(title = "Change"),
           xaxis = list(title = ""),
           barmode = "stack",
           title = str_c(input_bucket_group,": ", input_bucket_selected)) %>% 
    config(displayModeBar = FALSE) %>% 
    layout(separators = plotly_separators)
  
}

plot_year_over_year <- function(monthly,
                                input_date_range,
                                input_year_over_year_selected,
                                input_bucket_group) {
  
  all_months_sorted <- seq(ymd("2020-01-01"), ymd("2020-12-31"), by = "month") %>% 
    strftime("%b")
  
  year_over_year <- monthly %>% 
    filter(category == input_year_over_year_selected,
           bucket_group == input_bucket_group) %>% 
    filter(month >= input_date_range[1] &
             month <= input_date_range[2]) %>% 
    mutate(amount = ifelse(bucket_group == "Income", amount, amount*-1)) %>% 
    mutate(year = strftime(month, "%Y")) %>% 
    mutate(month = strftime(month, "%b") %>% factor(levels = all_months_sorted)) %>% 
    arrange(year, month) %>% 
    mutate(year = fct_inorder(year)) %>% 
    tidyr::complete(year, month, fill = list(amount = NA_real_))
  
  plot_ly() %>% 
    add_trace(data = year_over_year,
              x = ~month,
              y = ~amount,
              color = ~year,
              hovertemplate = glue::glue("%{{x}}: %{{y:{plotly_separators}0f}}"),
              type = "bar") %>% 
    layout(yaxis = list(title = "Amount"),
           xaxis = list(title = ""),
           title = str_c(input_bucket_group, ": ", input_year_over_year_selected)) %>% 
    config(displayModeBar = FALSE) %>% 
    layout(separators = plotly_separators)

}

plot_sankey <- function(monthly,
                        input_date_range,
                        input_saving_buckets_filter_choices,
                        input_buckets_filter) {
  monthly_filtered <- monthly %>% 
    filter(month >= input_date_range[1] &
           month <= input_date_range[2]) %>% 
    filter(category %in% input_buckets_filter)
  
  # Prepare data for sankey diagram
  
  # Difference between total income and total expenses (including saving buckets)
  # is either "deficit" which is added to Income streams
  # or "Not spend" which is added to "Savings"
  
  income_expense_diff <- monthly_filtered %>% 
    mutate(income = bucket_group == "Income") %>% 
    group_by(income) %>% 
    summarize(value = sum(amount)) %>% 
    pull(value) %>% 
    sum()
  
  # Depending on whether above is positive or negative
  # Create "deficit" or "Not spend"
  if (income_expense_diff >= 0) {
    deficit_or_not_spend <- tibble(source = c("Income", "Savings"),
                                   target = c("Savings", "Not spend"),
                                   value = c(income_expense_diff, income_expense_diff))   
  } else {
    deficit_or_not_spend <- tibble(source = "Deficit",
                                   target = "Income",
                                   value = -income_expense_diff) 
  }
  
  
  # Income streams to income
  first_level <- monthly_filtered %>% 
    filter(bucket_group == "Income") %>% 
    group_by(category, bucket_group) %>% 
    summarize(value = sum(amount)) %>% 
    select(source = category,
           target = bucket_group,
           value) %>% 
    mutate_at(vars(source, target), as.character)
  
  
  
  # Income to savings and expenses
  # Savings = saving buckets and "not spend"
  second_level <- monthly_filtered %>% 
    filter(bucket_group != "Income") %>% 
    mutate(amount = amount * -1) %>% 
    mutate(target = ifelse(category %in% input_saving_buckets_filter_choices, "Savings", "Expenses")) %>% 
    group_by(target) %>% 
    summarize(value = sum(amount)) %>% 
    mutate(source = "Income")
  
  
  # Expenses to bucket groups and savings to categories
  third_level <- monthly_filtered %>% 
    filter(bucket_group != "Income") %>% 
    mutate(source = ifelse(category %in% input_saving_buckets_filter_choices, "Savings", "Expenses")) %>% 
    mutate_at(vars(category, bucket_group), as.character) %>% 
    mutate(amount = amount*-1) %>% 
    mutate(target = ifelse(source == "Savings", category, bucket_group)) %>% 
    group_by(source, target) %>% 
    summarize(value = sum(amount))
  
  # Bucket groups to individual buckets
  # fourth_level <- monthly_filtered %>% 
  #   filter(bucket_group != "Income") %>% 
  #   filter(!(category %in% input_saving_buckets_filter_choices)) %>% 
  #   group_by(bucket_group, category) %>% 
  #   summarize(value = sum(amount)*-1) %>% 
  #   select(source = bucket_group,
  #          target = category,
  #          value) %>% 
  #   mutate_at(vars(source, target), as.character)
  
  # Helper tibble for index value of nodes
  id_helper <- tibble(node = c(first_level$source, 
                               first_level$target,
                               deficit_or_not_spend$source,
                               deficit_or_not_spend$target,
                               second_level$source,
                               second_level$target,
                               third_level$source,
                               third_level$target) %>% #,
                               # fourth_level$source,
                               # fourth_level$target) %>%
                        unique()) %>% 
    mutate(id_source = row_number()-1,
           id_target = id_source)
  
  # Collect all the levels in one tibble
  # IMPORTANT respect the order they where added to id_helper!!!
  all_levels <- bind_rows(first_level,
                          deficit_or_not_spend,
                          second_level,
                          third_level) %>% 
    group_by(source, target) %>% 
    summarize(value = sum(value)) %>% 
    ungroup()
    
  all_levels_ready <- all_levels %>% 
    left_join(id_helper %>% select(id_source, source = node), by = "source") %>% 
    left_join(id_helper %>% select(id_target, target = node), by = "target") %>% 
    mutate(color = ifelse(source == "Deficit", "red", "lightgrey"))
  
  plot_ly(type = "sankey",
          orientation = "h",
          valueformat = glue::glue("{plotly_separators}0f"),
          valuesuffix = if (!currency_before) user_currency else "",
          node = list(label = id_helper$node %>% as.character(),
                      hovertemplate = "%{label}"),
          link = list(source = all_levels_ready$id_source,
                      target = all_levels_ready$id_target,
                      value = all_levels_ready$value,
                      color = all_levels_ready$color,
                      hovertemplate = "%{source.label} → %{target.label}")) %>% 
    config(displayModeBar = FALSE) %>% 
    layout(separators = plotly_separators)
    
}


plot_forecast <- function(all_transactions,
                          input_date_range,
                          input_accounts,
                          input_buckets) {
  
  netflow_pr_month <- all_transactions %>% 
    mutate(month = floor_date(posted, "month")) %>% 
    # # Filter based on user input
    filter(month >= input_date_range[1],
           month <= input_date_range[2]) %>%
    filter(account %in% input_accounts) %>%
    filter(category %in% input_buckets) %>%
    # Calculate net flow pr month
    group_by(month) %>% 
    summarize(netflow = sum(amount)/100)
  
  # Start netwealth with and without account filters
  netwealth_now_total <- assets_liabilities %>% 
    filter(month == ceiling_date(today(), "month") - 1) %>% 
    pull(balance) %>% 
    sum()
  
  netwealth_now_filtered <- assets_liabilities %>% 
    filter(name %in% input_accounts) %>% 
    filter(month == ceiling_date(today(), "month") - 1) %>% 
    pull(balance) %>% 
    sum()
  
  # Forecast 10 years
  n_sims <- 100
  n_years <- 10
  n_months <- n_years*12
  
  # Sample from netflow with replacement
  forecast <- tibble(sim = 1:n_sims) %>% 
    mutate(data = list(tibble(month = seq(today() %m+% months(1),
                                          today() %m+% months(n_months), "month") %>%
                                floor_date("month")))) %>% 
    mutate(data = map(data, ~ .x %>% mutate(netflow = sample(netflow_pr_month$netflow,
                                                             n_months,
                                                             replace = TRUE)) %>% 
                        mutate(netwealth = accumulate(netflow, `+`)))) %>% 
    mutate(last_netwealth = map_dbl(data, ~ tail(.x$netwealth, 1))) %>% 
    # Remove top and bottom 5%
    arrange(last_netwealth) %>% 
    slice((floor(0.05*n_sims)+1):floor(0.95*n_sims))
  
  forecast_mean <- forecast %>% 
    unnest(data) %>% 
    group_by(month) %>% 
    summarise(netflow = mean(netflow)) %>% 
    mutate(netwealth = accumulate(netflow, `+`))
  
  netwealth_change <- tail(forecast_mean$netwealth, 1)
  
  forecast_title <- str_c("Forecasting ", n_years, " years using ", n_sims, " simulations. Discarding top 5% and bottom 5%\n",
                          "Mean change in net wealth = ", format_currency(netwealth_change), "\n",
                          "Total net wealth now = ", format_currency(netwealth_now_total), " ",
                          "Total net wealth in ", n_years, " years = ", format_currency(netwealth_now_total + netwealth_change), "\n",
                          "Filtered net wealth now = ", format_currency(netwealth_now_filtered), " ",
                          "Filtered net wealth in ", n_years, " years = ", format_currency(netwealth_now_filtered + netwealth_change)
  )
  
  fig <- plot_ly(type = "scatter", mode = "lines")
  
  for (i in 1:nrow(forecast)) {
    data_to_plot <- forecast$data[[i]]
    fig <- fig %>% 
      add_trace(NULL,
                y = data_to_plot$netwealth, x = data_to_plot$month,
                line = list(color = 'rgb(211, 211, 211)', width = 0.5),
                showlegend = FALSE)
  }
  
  fig <- fig %>% 
    add_trace(NULL,
              y = forecast_mean$netwealth, x = forecast_mean$month,
              line = list(color = 'rgb(0, 0 , 0)', width = 3),
              showlegend = FALSE) %>% 
    layout(yaxis = list(title = ""),
           xaxis = list(title = ""),
           title = "Forecast") %>% 
    config(displayModeBar = FALSE) %>% 
    layout(separators = plotly_separators,
           title = forecast_title,
           annotations = list(x = tail(forecast_mean$month, 1), y = netwealth_change,
                              text = format_currency(netwealth_change),
                              yshift = 10,
                              showarrow = F,
                              font = list(size = 14)))
}
