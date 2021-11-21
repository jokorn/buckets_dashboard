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

# Set reporting period
date_from <- today() %>% floor_date("year")
date_to <- today()

# Connect to database and the relevant tables
con <- dbConnect(SQLite(), path_to_buckets)
transactions <- tbl(con, "bucket_transaction")
buckets <- tbl(con, "bucket")
bucket_groups <- tbl(con, "bucket_group")
income <- tbl(con, "account_transaction")

# Add bucket groups to buckets
buckets_ready <- buckets %>% 
  select(bucket_id = id, 
         group_id, 
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

# Create date variable for use in UI/server 
dates_available <- range(monthly$month) 

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
      filter(Total != 0)
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
                                                columnDefs = list(list(visible=FALSE, targets=c(0))),
                                                fixedHeader = TRUE),
                                 rownames = FALSE,
                                 extensions = c('FixedHeader'),
                                 selection = list(target = "cell"))
  } else {
    datatable_prepare <- datatable(data_source_ready,
                                 options = list(dom = "t", 
                                                paging = FALSE,
                                                rowGroup = list(dataSrc = 0),
                                                columnDefs = list(list(visible=FALSE, targets=c(0))),
                                                fixedHeader = TRUE),
                                 rownames = FALSE,
                                 extensions = c('FixedHeader', 'RowGroup'),
                                 selection = list(target = "cell"))
  }
  
  # Format columns with colors, string formatm etc.
  datatable_prepare %>% 
    formatStyle(c(date_filter %>% strftime("%Y-%b"),
                "Average",
                "Total"),
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
                           paging = FALSE,
                           scrollY = height_transactions_report)) %>% 
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