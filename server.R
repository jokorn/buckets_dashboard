# Define server logic
shinyServer(function(input, output, session) {
    
  # Set up reactive values associated with the toggle actionbuttons
  show_bucketgroups <- reactiveVal(FALSE)
  show_zerototals <- reactiveVal(TRUE)
    
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
                           date_filter = seq.Date(input$date_range[[1]],
                                                  input$date_range[[2]],
                                                  by = "month"),
                           buckets_filter = c(input$income_buckets_filter_choices,
                                              input$expense_buckets_filter_choices),
                           bucketgroups_view = show_bucketgroups(),
                           show_zero_totals = show_zerototals())
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
                         bucketgroups_view = show_bucketgroups(),
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
                                                               bucketgroups_view = show_bucketgroups(),
                                                               show_zero_totals = show_zerototals()))
    })
    
    # Select all buckets in the drop down menus when the button is clicked
    observeEvent(input$select_all_buckets, {
      selectCells(proxy = dataTableProxy("expenses_pr_month"), selected = NULL)
      updateCheckboxGroupInput(inputId="income_buckets_filter_choices", 
                               choices = levels(monthly %>% filter(bucket_group == "Income") %>% pull(category) %>% fct_drop()),
                               selected = levels(monthly %>% filter(bucket_group == "Income") %>% pull(category) %>% fct_drop()))
      updateCheckboxGroupInput(inputId="expense_buckets_filter_choices", 
                               choices = levels(monthly %>% filter(bucket_group != "Income") %>% pull(category) %>% fct_drop()),
                               selected = levels(monthly %>% filter(bucket_group != "Income") %>% pull(category) %>% fct_drop()))
      
      }
    )
    
    # Toggle the reactive value (used as input when generating the income/expense report)
    # when the Bucket Groups button is clicked and also clear any selections
    # in the table to avoid bugs
    observeEvent(input$toggle_report_view, {
      show_bucketgroups(!show_bucketgroups())
      selectCells(proxy = dataTableProxy("expenses_pr_month"), selected = NULL)
    }
    )
    
    # Toggle the reactive value (used as input when generating the income/expense report)
    # when the show zero totals button is clicked and also clear any selections
    # in the table to avoid bugs
    observeEvent(input$toggle_zero_totals, {
      show_zerototals(!show_zerototals())
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
    
    observeEvent(input$select_current_month, {
      updateAirDateInput(session,
                         inputId = "date_range",
                         value = c(floor_date(today(), "month"), floor_date(today(), "month")))
      selectCells(proxy = dataTableProxy("expenses_pr_month"), selected = NULL)
    }
    )
    
    observeEvent(input$select_current_year, {
      updateAirDateInput(session,
                         inputId = "date_range",
                         value = c(max(today() %>% floor_date("year"), dates_available[1]),
                                   floor_date(today(), "month")))
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
                mutate(parent = "Income") %>% 
                filter(amount != 0) %>% 
                mutate(prop = amount / sum(amount)) %>%
                mutate(parent = if_else(prop < income_other_threshold, "Other", "Income"))
                  
                second <- first %>% 
                  group_by(parent) %>% 
                  summarize(amount = sum(amount)) %>%
                  filter(parent == "Other") %>%
                  mutate(category = "Other", parent = "Income")
                
                bind_rows(first, second)
                },
              name = "income_sunburst",
              branchvalues = "total",
              maxdepth = 2,
              parents = ~parent,
              labels = ~category,
              values = ~amount,
              textinfo = income_sunburst_textinfo,
              hoverinfo = income_sunburst_hoverinfo,
              type = "sunburst") %>% 
        config(displayModeBar = FALSE) %>% 
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
              maxdepth = 2,
              branchvalues = "total",
              type = "sunburst") %>% 
        config(displayModeBar = FALSE) %>% 
        layout(separators = plotly_separators)
    })
    
    # Select all accounts in the drop down menu when the button is clicked
    observeEvent(input$select_all_accounts, {
      updateCheckboxGroupInput(inputId="netwealth_account_filter_choices", 
                               choices = unique(assets_liabilities$name),
                               selected = unique(assets_liabilities$name))
    }
    )
    
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
                   input$saving_buckets_filter_choices)
    })
    
    # Create the Bucket Transactions plot
    output$bucket_transactions <- renderPlotly({
      # Again we need two dates to create the date filter
      validate(
        need(length(input$date_range) == 2, "Select both start and end month for the reports.")
      )
      
      # And we need one bucket selected
      validate(
        need(input$bucket_transactions_selected != "", "")
      )
      
      plot_bucket_transactions(buckets_monthly,
                               input$date_range,
                               input$bucket_transactions_selected)
      
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
      plot_bucket_balance(buckets_ready)
    }) 
    
})
