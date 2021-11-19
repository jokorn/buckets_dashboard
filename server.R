# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
  # Set up reactive values associated with the toggle actionbuttons
  show_bucketgroups <- reactiveVal(FALSE)
  show_zerototals <- reactiveVal(TRUE)
    
  # Create the income/expense report by calling the function defined in global.R
    output$expenses_pr_month <- DT::renderDataTable({
      # We need both start and end month to create the date filter
      validate(
        need(length(input$date_range) == 2, "Select both start and end month for the reports.")
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
      # Provide all the user input to the function
      transactions_table(data_source = everything %>% 
                           filter(posted >= input$date_range[[1]],
                                  posted <= input$date_range[[2]]),
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
    
    # Clear all selections in the income/expense report table when the button
    # is clicked
    observeEvent(input$clear_selection, {
      selectCells(proxy = dataTableProxy("expenses_pr_month"), selected = NULL)
    }
    )
    
    # Create the Income pie chart
    output$income_piechart <- renderPlotly({
      # Again we need two dates to create the date filter
      validate(
        need(length(input$date_range) == 2, "Select both start and end month for the reports.")
      )
      
      # Render as pie chart
      plot_ly(data = monthly %>% 
                filter(month >= input$date_range[[1]],
                       month <= input$date_range[[2]]) %>% 
                filter(bucket_group == "Income") %>% 
                filter(category %in% input$income_buckets_filter_choices) %>% 
                group_by(category) %>% 
                summarize(amount = sum(amount)) %>% 
                mutate(parent = "Income") %>% 
                filter(amount != 0),
              name = "income_piechart",
              title = "Income",
              labels = ~category,
              values = ~amount,
              textinfo = income_piechart_textinfo,
              showlegend = TRUE,
              automargin = TRUE, #Needed together with fixed height to make it fit in the tabset 
              type = "pie") %>% 
        config(displayModeBar = FALSE)
    })
      
    # Create the expenses sunburst chart
    output$expense_piechart <- renderPlotly({
      # Again we need to dates to filer on the date range
      validate(
        need(length(input$date_range) == 2, "Select both start and end month for the reports.")
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
              values = ~amount,
              maxdepth = 2,
              branchvalues = "total",
              type = "sunburst") %>% 
        config(displayModeBar = FALSE)
    })
})