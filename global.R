# Load libraries - make sure to install them first with install.packages
library(tidyverse)
library(lubridate)
library(DBI)
library(RSQLite)
library(dplyr)
library(shiny)
library(shinyWidgets)
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
transactions <- tbl(con, "bucket_transaction")
buckets <- tbl(con, "bucket")
bucket_groups <- tbl(con, "bucket_group")
income <- tbl(con, "account_transaction")
acc_balance <- tbl(con, "account") %>% collect()
acc_trans <- tbl(con, "account_transaction") %>% collect()

# Add bucket groups to buckets
buckets_ready <- buckets %>% 
  select(bucket_id = id, 
         group_id,
         balance,
         ranking,
         category = name) %>%
  collect() %>% 
  left_join(bucket_groups %>% 
              select(group_id = id,
                     bucket_group = name) %>% 
              collect(),
            by = "group_id")

# Prepare income
income_ready <- income %>% 
  collect() %>% 
  filter(general_cat == "income") %>% 
  mutate(bucket_group = "Income",
         group_id = -99,
         ranking = memo) %>% 
  select(bucket_group,
         category = memo,
         posted,
         amount,
         group_id,
         ranking)

# Prepare expenses
expenses_ready <- transactions %>% 
  filter(!is.na(account_trans_id)) %>% 
  collect() %>%
  select(id, posted, bucket_id, amount, memo) %>% 
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
  mutate(category = factor(category) %>% fct_inorder)
  

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
  collect() %>%
  mutate(transaction = !is.na(account_trans_id)) %>%
  mutate(positive = amount > 0) %>%
  mutate(category = case_when(transaction & positive ~ "Transaction - Ingoing",
                              transaction & !positive ~ "Transaction - Outgoing",
                              !transaction & positive ~ "Budgeted - Ingoing",
                              !transaction & !positive ~ "Budgeted - Outgoing")) %>%
  select(posted,
         amount,
         name,
         category) %>%
  mutate(amount = amount / 100) %>%
  mutate(posted = floor_date(ymd(str_sub(posted, 1, 10)), "month")) %>%
  group_by(posted, name, category) %>%
  summarise(amount = sum(amount)) %>%
  mutate(name = factor(name),
         category = factor(category,
                           levels = c("Budgeted - Ingoing",
                                      "Transaction - Ingoing",
                                      "Budgeted - Outgoing",
                                      "Transaction - Outgoing")))%>%
  tidyr::complete(name, category, posted, fill = list(amount = 0)) %>% 
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

# Create date variable for use in UI/server 
dates_available <- range(monthly$month) 

# Set reporting period
date_from <- max(today() %>% floor_date("year"), dates_available[1])
date_to <- today()

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
      mutate(bucket_group = fct_inorder(factor(bucket_group))) %>% 
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
                  group_by(category) %>% 
                  summarize(Average = mean(amount) %>% round(2),
                            Total = sum(amount) %>% round(2)) %>% 
                  ungroup(),
                by = "category")
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

plot_bucket_transactions <- function(buckets_monthly,
                                     input_date_range,
                                     input_bucket_selected) {
  bucket_plot <- buckets_monthly %>% 
    filter(name == input_bucket_selected) %>% 
    filter(posted >= dates_available[1] &
             posted <= dates_available[2])
  
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
           title = input_bucket_selected) %>% 
    config(displayModeBar = FALSE) %>% 
    layout(separators = plotly_separators)
  
}
