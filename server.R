# Define server logic
shinyServer(function(input, output, session) {
  
  # Check if any bucket names are duplicated as this may cause issues
  # if duplicates exist create a modal to warn and encourage renaming of buckets
  if (show_modal == TRUE) {
    showModal(modalDialog(
      title = "WARNING!",
      HTML(modal_text)
    ))
  } 
  
  # Input validation common
  iv_common <- InputValidator$new()
  iv_common$add_rule("date_range", ~ if (length(.) != 2) "Select both start and end month for the reports.")
  iv_common$add_rule("date_range", ~ if (!checkDate(.)) "Enter valid start and end month for the reports.")
  iv_common$enable()
  
  # Create the income/expense report by calling the function defined in global.R
    output$expenses_pr_month <- DT::renderDataTable({
      # We need both start and end month to create the date filter
      validate(
        need(length(input$date_range) == 2, "Select both start and end month for the reports.")
      )
      
      # We need at least one income or expense bucket
      validate(
        need(length(c(input$income_buckets_filter_choices,
                      input$expense_buckets_filter_choices)) >= 1, "Select at least one income or expense bucket.")
      )
      
      # Provide all the user input to the function
      expense_income_table(data_source =  monthly %>% 
                             filter(month >= input$date_range[[1]],
                                    month <= input$date_range[[2]]),
                           date_filter = seq(input$date_range[[1]],
                                             input$date_range[[2]],
                                             by = "month"),
                           buckets_filter = c(input$income_buckets_filter_choices,
                                              input$expense_buckets_filter_choices),
                           bucketgroups_view = input$toggle_report_view,
                           show_zero_totals = input$toggle_zero_totals)
      })
    
    # Create the transactions table by calling the function defined in global.R
    output$transactions_table <- DT::renderDataTable({
      # We need both start and end month to create the date filter
      validate(
      need(length(input$date_range) == 2, "Select both start and end month for the reports.")
      )
     
      # We need at least one income or expense bucket to any transactions.
      validate(
        need(length(c(input$income_buckets_filter_choices,
                      input$expense_buckets_filter_choices)) >= 1, "Select at least one income or expense bucket.")
      )
       
      # Provide all the user input to the function
      transactions_table(data_source = everything %>% 
                           filter(floor_date(posted, "month") >= input$date_range[[1]],
                                  floor_date(posted, "month") <= input$date_range[[2]]),
                         cells_filter = input$expenses_pr_month_cells_selected,
                         buckets_filter = c(input$income_buckets_filter_choices,
                                            input$expense_buckets_filter_choices),
                         bucketgroups_view = input$toggle_report_view,
                         # Provide the expense report as well as we need the
                         # data from this to filter transactions based on 
                         # cell selections
                         expense_report = expense_income_table(data_source = monthly %>% 
                                                                 filter(month >= input$date_range[[1]],
                                                                        month <= input$date_range[[2]]),
                                                               date_filter = seq.Date(input$date_range[[1]],
                                                                                      input$date_range[[2]],
                                                                                      by = "month"),
                                                               buckets_filter = c(input$income_buckets_filter_choices,
                                                                                  input$expense_buckets_filter_choices),
                                                               bucketgroups_view = input$toggle_report_view,
                                                               show_zero_totals = input$toggle_zero_totals))
    })
    
    # Deselect all kicked buckets in the expenses drop down menu when clicked
    observeEvent(input$deselect_kicked, {
      selectCells(proxy = dataTableProxy("expenses_pr_month"), selected = NULL)
      updateCheckboxGroupInput(inputId="expense_buckets_filter_choices", 
                               choices = levels(monthly %>% filter(bucket_group != "Income") %>% pull(category) %>% fct_drop()),
                               selected = expenses_named_prepare %>% filter(bucket_group != "Kicked") %>% pull(category))
    })
    
    # Toggle the reactive value (used as input when generating the income/expense report)
    # when the Bucket Groups button is clicked and also clear any selections
    # in the table to avoid bugs
    observeEvent(input$toggle_report_view, {
      selectCells(proxy = dataTableProxy("expenses_pr_month"), selected = NULL)
    }
    )
    
    # Toggle the reactive value (used as input when generating the income/expense report)
    # when the show zero totals button is clicked and also clear any selections
    # in the table to avoid bugs
    observeEvent(input$toggle_zero_totals, {
      selectCells(proxy = dataTableProxy("expenses_pr_month"), selected = NULL)
    }
    )
    
    # Buttons for changing date interval quickly
    observeEvent(input$select_all_dates, {
      updateAirDateInput(session,
                         inputId = "date_range",
                         value = c(min(monthly$month), max(monthly$month)))
      selectCells(proxy = dataTableProxy("expenses_pr_month"), selected = NULL)
    }
    )
    
    observeEvent(input$select_current_year, {
      updateAirDateInput(session,
                         inputId = "date_range",
                         value = c(max(today2() %>% floor_date("year"), dates_available[1]),
                                   floor_date(today2(), "month")))
      selectCells(proxy = dataTableProxy("expenses_pr_month"), selected = NULL)
    }
    )
    
    
    # Clear all selections in the income/expense report table when the button
    # is clicked
    observeEvent(input$clear_selection, {
      selectCells(proxy = dataTableProxy("expenses_pr_month"), selected = NULL)
    }
    )
    
    # Clear all selections in the income/expense report table when the button
    # is clicked
    observeEvent(input$clear_selection, {
      selectCells(proxy = dataTableProxy("expenses_pr_month"), selected = NULL)
    }
    )
    
    # Create the Net Wealth chart
    output$net_wealth <- renderPlotly({
      # Again we need two dates to create the date filter
      validate(
        need(length(input$date_range) == 2, "Select both start and end month for the reports.")
      )
      
      # And we need at least one account
      validate(
        need(length(input$netwealth_account_filter_choices) >= 1, "Select at least one account to show net wealth.")
      )
      
      plot_net_wealth(assets_liabilities,
                      input$date_range,
                      input$netwealth_account_filter_choices)
      
    })
    
    # Create the Income sunburst chart
    output$income_sunburstchart <- renderPlotly({
      # Again we need two dates to create the date filter
      validate(
        need(length(input$date_range) == 2, "Select both start and end month for the reports.")
      )
      
      # We need at least one income bucket
      validate(
        need(length(input$income_buckets_filter_choices) >= 1, "Select at least one income bucket.")
      )
      
      # Render as sunburst chart
      plot_ly(data = {first <- monthly %>% 
                filter(month >= input$date_range[[1]],
                       month <= input$date_range[[2]]) %>% 
                filter(bucket_group == "Income") %>% 
                filter(category %in% input$income_buckets_filter_choices) %>% 
                group_by(category) %>% 
                summarize(amount = sum(amount)) %>% 
                mutate(parent = " Income ") %>% 
                filter(amount != 0) %>% 
                mutate(prop = amount / sum(amount)) %>%
                mutate(parent = if_else(prop < income_other_threshold, "Other", " Income "))
                  
                second <- first %>% 
                  group_by(parent) %>% 
                  summarize(amount = sum(amount)) %>%
                  filter(parent == "Other") %>%
                  mutate(category = "Other", parent = " Income ")
                
                bind_rows(first, second)
                },
              name = "income_sunburst",
              branchvalues = "total",
              maxdepth = 2,
              parents = ~parent,
              labels = ~category,
              values = ~amount,
              insidetextorientation = "horizontal",
              textinfo = income_sunburst_textinfo,
              hoverinfo = income_sunburst_hoverinfo,
              type = "sunburst") %>% 
        config(displayModeBar = TRUE,
               displaylogo = FALSE,
               modeBarButtonsToRemove = c("zoom", "pan", "select", "zoomIn", "zoomOut",
                                          "autoScale", "resetScale", "hoverClosestCartesian",
                                          "hoverCompareCartesian", "lasso2d"),
               toImageButtonOptions = list(height= NULL,
                                           width= NULL)) %>% 
        layout(separators = plotly_separators)
    })
      
    # Create the expenses sunburst chart
    output$expense_sunburstchart <- renderPlotly({
      # Again we need to dates to filer on the date range
      validate(
        need(length(input$date_range) == 2, "Select both start and end month for the reports.")
      )
      
      # Select at least one expense bucket to show the report
      validate(
        need(length(input$expense_buckets_filter_choices) >= 1, "Select at least one expense bucket.")
      )
      
      # Create the sunburst chart including create the data needed for the chart
      # (hierarchical instead of long format)
      # Also, remove amounts equal to zero or expenses with positive values
      # as I do not know how they should be visualized
      plot_ly(data = bind_rows(monthly %>% 
                filter(month >= input$date_range[[1]],
                       month <= input$date_range[[2]]) %>% 
                filter(bucket_group != "Income") %>% 
                filter(category %in% input$expense_buckets_filter_choices) %>% 
                  filter(amount < 0) %>% 
                group_by(bucket_group) %>% 
                summarize(amount = sum(amount)*-1) %>% 
                mutate(parent = "Expenses",
                       labels = bucket_group),
                monthly %>% 
                  filter(month >= input$date_range[[1]],
                         month <= input$date_range[[2]]) %>% 
                  filter(bucket_group != "Income") %>% 
                  filter(category %in% input$expense_buckets_filter_choices) %>% 
                  filter(amount < 0) %>% 
                  group_by(bucket_group, category) %>% 
                  summarize(amount = sum(amount)*-1) %>% 
                  mutate(parent = bucket_group,
                         labels = category)),
              name = "expense_piechart",
              parents = ~parent,
              labels = ~labels,
              textinfo = expenses_sunburst_textinfo,
              hoverinfo = expenses_sunburst_hoverinfo,
              values = ~amount,
              insidetextorientation = "horizontal",
              maxdepth = 2,
              branchvalues = "total",
              type = "sunburst") %>% 
        config(displayModeBar = TRUE,
               displaylogo = FALSE,
               modeBarButtonsToRemove = c("zoom", "pan", "select", "zoomIn", "zoomOut",
                                          "autoScale", "resetScale", "hoverClosestCartesian",
                                          "hoverCompareCartesian", "lasso2d"),
               toImageButtonOptions = list(height= NULL,
                                           width= NULL)) %>% 
        layout(separators = plotly_separators)
    })
    
    # Select the transactions contributing to pension savings rate
    output$pension_savings <- renderUI({
      shiny:::req(input$pension_account)
      pickerInput(inputId = "pension_savings",
                  label = NULL,
                  choices = all_transactions %>% 
                    filter(account %in% input$pension_account) %>% 
                    distinct(memo) %>% 
                    arrange(memo) %>% 
                    pull(memo),
                  multiple = TRUE,
                  width = "fit",
                  options = list(`actions-box` = TRUE,
                                 header = "Pension savings",
                                 `selected-text-format` = "static",
                                 title = "Select all the transactions that should contribute to pension savings"))
    })
    
    # Display the savings rate table
    output$savings_rate_table <- DT::renderDataTable({
      # We need both start and end month to create the date filter
      validate(
        need(length(input$date_range) == 2, "Select both start and end month for the reports.")
      )
      
      # We need at least one income or expense bucket
      validate(
        need(length(c(input$income_buckets_filter_choices,
                      input$expense_buckets_filter_choices)) >= 1, "Select at least one income or expense bucket.")
      )
      
      savings_rate(monthly %>% filter(category %in% c(input$income_buckets_filter_choices,
                                                      input$expense_buckets_filter_choices)),
                   input$date_range,
                   input$saving_buckets_filter_choices,
                   input$pension_account,
                   input$pension_savings)
    })
    
    # Create the Bucket Transactions plot
    output$bucket_transactions <- renderPlotly({
      # Again we need two dates to create the date filter
      validate(
        need(length(input$date_range) == 2, "")
      )
      
      # And we need one bucket selected
      validate(
        need(input$bucket_transactions_selected != "", "")
      )
      
      # Get the bucket group from the selector using js
      js$get_bucket_transactions_bucket_group()
      
      validate(
          need(input$bucket_transactions_bucket_group != "", "")
      )
      
      plot_bucket_transactions(buckets_monthly,
                               input$date_range,
                               input$bucket_transactions_selected,
                               input$bucket_transactions_bucket_group)
      
    })
    
    # Create the Year Over Year plot
    output$year_over_year <- renderPlotly({
      # Again we need two dates to create the date filter
      validate(
        need(length(input$date_range) == 2, "")
      )
      
      # And we need one bucket selected
      validate(
        need(input$year_over_year_selected != "", "")
      )
      
      # Get the bucket group from the selector using js
      js$get_year_over_year_bucket_group()
      
      validate(
        need(input$year_over_year_bucket_group != "", "")
      )
      
      plot_year_over_year(monthly,
                          input$date_range,
                          input$year_over_year_selected,
                          input$year_over_year_bucket_group)
      
    })
    
    # Create the Sankey plot
    output$sankey <- renderPlotly({
      # Again we need two dates to create the date filter
      validate(
        need(length(input$date_range) == 2, "")
      )
      
      # We need at least one income or expense bucket
      validate(
        need((length(input$income_buckets_filter_choices) >= 1 &
               length(input$expense_buckets_filter_choices) >= 1), "Select at least one income and one expense bucket.")
      )
      
      plot_sankey(monthly,
                  input$date_range,
                  input$saving_buckets_filter_choices,
                  c(input$income_buckets_filter_choices,
                    input$expense_buckets_filter_choices))
      
    })
    
    # Create the Forecast plot
    output$forecast <- renderPlotly({
      # Again we need two dates to create the date filter
      validate(
        need(length(input$date_range) == 2, "Select both start and end month for the reports.")
      )
      
      # And we need at least one account
      validate(
        need(length(input$netwealth_account_filter_choices) >= 1, "Select at least one account to forecast.")
      )
      
      plot_forecast(all_transactions,
                    input$date_range,
                    input$netwealth_account_filter_choices,
                    c(input$income_buckets_filter_choices,
                      input$expense_buckets_filter_choices))
      
    })
    
    # When a "stock" account has been selected render the input UI for selecting
    # transfers and gains transactions
    output$stock_transfers <- renderUI({
      pickerInput(inputId = "stock_transfers",
                  label = NULL,
                  choices = all_transactions %>% 
                    filter(account %in% input$stock_account) %>% 
                    distinct(memo) %>% 
                    arrange(memo) %>% 
                    pull(memo),
                  multiple = TRUE,
                  width = "fit",
                  options = list(`actions-box` = TRUE,
                                 header = "Transfers",
                                 `selected-text-format` = "static",
                                 title = "Select all the transfer transactions"))
    })
    
    output$stock_gains <- renderUI({
      pickerInput(inputId = "stock_gains",
                  label = NULL,
                  choices = all_transactions %>% 
                    filter(account %in% input$stock_account) %>% 
                    distinct(memo) %>% 
                    arrange(memo) %>% 
                    pull(memo),
                  multiple = TRUE,
                  width = "fit",
                  options = list(`actions-box` = TRUE,
                                 header = "Gains",
                                 `selected-text-format` = "static",
                                 title = "Select all the gains transactions"))
    })
    
    # When all the data needed for forecasting stocks, then render the historical data
    output$stock_historical <- renderPlotly({
      
      req(iv_common$is_valid())
      
      shiny::validate(
        need(input$stock_account,
             "Select at least one stock account to display historical stock data."
        )
      )
      
      stock_data <- create_stock_data(input$date_range[1],
                                      input$date_range[2],
                                      input$stock_account,
                                      input$stock_transfers,
                                      input$stock_gains)
      
      
      plot_stock_historical(stock_data)
      
    })
    
    # Validate the numeric inputs used in stock forecasting
    iv_stock_forecast <- InputValidator$new()
    iv_stock_forecast$add_rule("stock_time", compose_rules(sv_integer(), sv_between(1, 50)))
    iv_stock_forecast$add_rule("stock_nsims", compose_rules(sv_integer(), sv_between(2, 1001)))
    iv_stock_forecast$enable()
    
    # Calculate gains, transfers, and start value based on inputs for use
    # when forecasting stock values
    
    stock_forecast_start_value <- reactive(calculate_start_value(input$stock_start_value,
                                                                 input$stock_account,
                                                                 input$stock_transfers,
                                                                 input$stock_gains,
                                                                 input$date_range[[1]],
                                                                 input$date_range[[2]])) %>% 
      debounce(1000)
    
    stock_forecast_gains <- reactive(calculate_gains(input$stock_gains_per_year,
                                                     input$stock_account,
                                                     input$stock_transfers,
                                                     input$stock_gains,
                                                     input$date_range[[1]],
                                                     input$date_range[[2]],
                                                     input$stock_mean_sample)) %>% 
      debounce(1000)
    
    stock_forecast_transfers <- reactive(calculate_transfers(input$stock_invested_per_month,
                                                             input$stock_account,
                                                             input$stock_transfers,
                                                             input$stock_gains,
                                                             input$date_range[[1]],
                                                             input$date_range[[2]],
                                                             input$stock_mean_sample)) %>% 
      debounce(1000)
    
    # When we have the needed data, then forecast and plot the stock values 
    output$stock_forecast_fig <- renderPlotly({
      
      req(iv_common$is_valid(),
          iv_stock_forecast$is_valid())
      
      req(stock_forecast_start_value(),
          stock_forecast_gains(),
          stock_forecast_transfers(),
          input$stock_nsims,
          input$stock_time,
          input$stock_mean_sample,
          cancelOutput = TRUE)
      
      plot_stock_forecast(input$stock_time,
                          stock_forecast_start_value(),
                          stock_forecast_gains(), 
                          stock_forecast_transfers(),
                          input$stock_mean_sample,
                          input$stock_nsims)
      
    })
    
    # Now output the stock forecast figure to the UI - these 2 steps allows
    # dynamic resizing of height depending on the number of subplots
    ht <- reactive({
      if (input$stock_mean_sample == "Sample") {
        "1000px"
      } else {
        "400px"
      }
    }
    )
    
    output$stock_forecast <- renderUI({
      plotlyOutput("stock_forecast_fig",
                   height = ht())
    })
    
    # Calculate expenses from historical data
    expenses <- reactive(calculate_expenses(monthly,
                                            input$date_range,
                                            input$income_buckets_filter_choices,
                                            input$expense_buckets_filter_choices,
                                            input$saving_buckets_filter_choices)
                         )
    
    # Create the stock gains vs expenses plot
    output$gains_vs_expenses_plot <- renderPlotly({
      
      req(iv_common$is_valid())
      
      shiny::validate(
        need(input$stock_account,
             "Select at least one stock account to display historical stock data."
        )
      )
      
      plot_gains_vs_expenses(input$date_range,
                             input$stock_account,
                             input$stock_transfers,
                             input$stock_gains,
                             expenses())
      
    })
    
    expenses_per_month <- reactive({
      if (test_number(input$expenses_per_month)) {
        input$expenses_per_month
      } else {
        mean(expenses()$Expenses)
      }
    }) %>% debounce(1000)
    
    output$stock_cover_expenses_plot <- renderPlotly({
      
      req(iv_common$is_valid(),
          iv_stock_forecast$is_valid())
      
      req(stock_forecast_start_value(),
          stock_forecast_gains(),
          stock_forecast_transfers(),
          input$cover_expenses_until_date,
          expenses_per_month(),
          cancelOutput = TRUE)
      
      plot_stock_cover_expenses(stock_forecast_start_value(),
                                mean(stock_forecast_gains()),
                                mean(stock_forecast_transfers()),
                                input$cover_expenses_until_date,
                                expenses_per_month())
      
    })
    
    # Select the saving buckets from config.R in the drop down menu when the button is clicked
    observeEvent(input$select_config_saving_buckets, {
      updateCheckboxGroupInput(inputId="saving_buckets_filter_choices", 
                               selected = savings_buckets,
                               choices = levels(monthly %>% filter(bucket_group != "Income") %>% pull(category) %>% fct_drop()))
    }
    )
    
    # Reload data and app when the "Reload .buckets file" button is clicked
    observeEvent(input$reload_app, {
      source("global.R")
      session$reload()
    })
    
    # Plot bucket balances
    output$bucket_balances <- renderPlotly( {
      plot_bucket_balance(buckets_ready,
                          input$bucket_balances_labels)
    }) 
    
})
