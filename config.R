# Config ------------------------------------------------------------------

# Set the path to the .buckets file
path_to_buckets <- "path/to/budget.buckets" #IMPORTANT!!!

# Customize zoom to fit 1 year in the income/expense report (0.8 works for me)
zoom_value <- 0.75

# Currency settings
user_currency = "$" #Add space before or after as needed
currency_before = TRUE #Currency symbol before or after amount
user_mark = "," #Thousand separator
user_dec.mark = "." #Decimal separator
plotly_separators <- ".," #From documentation: Sets the decimal and thousand separators. For example, ". " puts a ‘.’ before decimals and a space between thousands. In English locales, dflt is ".," but other locales may alter this default.

# Category changes / override Bucket's setup (seldom needed)
# Can e.g. be used to change an expense bucket to an income bucket
manual_categorization <- tribble(
  ~bucket_group, ~category, ~bucket_group_new, ~category_new, ~group_id_new, ~ranking_new, #group_id can be difficult to find but is -99 for Income
  "Old_bucket_group", "Old_category_name", "New_bucket_group_eg_Income", "New_category_name", -99, "Sorting_within_new_group_eg_use_name_for_alphabetical_sorting" #Add more as needed
)

# Specify Buckets that are actually transfers to off budget saving accounts
# for use with the Savings Rate report
savings_buckets <- c("Saving Bucket 1",
                     "Saving Bucket 2") #Add more as needed

# Customize the charts and reports
height_expenses_sunburst <- "650px"
height_income_piechart <- "650px"
height_transactions_report <- "680px"
height_expense_report <- "740px"
height_bucket_balances <- "1600px"
height_bucket_transactions <- "615px"
height_year_over_year <- "615px"
height_sankey <- "650px"

income_sunburst_textinfo <- "label+value+percent entry" #You can remove some of these if the charts are too crowded
expenses_sunburst_textinfo <- "label+value+percent entry"
income_sunburst_hoverinfo <- "label+value+percent entry"
expenses_sunburst_hoverinfo <- "label+value+percent entry"

income_other_threshold <- 0.03 #Set threshold for when to add category to "Other" in "Sunburst Chart - Income"

# Error message position (may need to be changed if zoom is changed)
error_position_left <- "20px"
error_position_top <- "150px"
