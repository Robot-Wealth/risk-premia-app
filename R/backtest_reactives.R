# Reactives and their dependencies for backtesting tabs - scope for individual sessions

library(here)
library(tidyverse)
library(glue)

# Update rp_tickers global =====
# make dataframe of all prices in server shared, filter based on universe selection, include IRX, update rp_tickers global for aesthetics
# also move aesthetics around plotting here from server_shared
# only update values when runbactests button is pressed

# on change of universe, update target vol slider labels
observe({
  if(input$universeInput == "us_etfs") {
    universe <- us_etf_tickers
  } else if(input$universeInput == "lev_us_etfs") {
    universe <- us_lev_etf_tickers
  } else if(input$universeInput == "ucits_etfs") {
    universe <- ucits_etf_tickers
  } else {
    stop("Implement another option for target vol sliders!!")
  }
  updateSliderInput(inputId = "targetVolSlider1", label = glue("{universe[1]} Target Volatility, %"))
  updateSliderInput(inputId = "targetVolSlider2", label = glue("{universe[2]} Target Volatility, %"))
  updateSliderInput(inputId = "targetVolSlider3", label = glue("{universe[3]} Target Volatility, %"))
})

backtest_tickers <- eventReactive(input$runBacktestButton, {
  # get tickers from universe selection
  if(input$universeInput == "us_etfs") {
    us_etf_tickers
  } else if(input$universeInput == "lev_us_etfs") {
    us_lev_etf_tickers
  } else if(input$universeInput == "ucits_etfs") {
    ucits_etf_tickers
  }
})

prices <- eventReactive(input$runBacktestButton, {
  print(backtest_tickers())
  all_prices %>% 
    filter(ticker %in% backtest_tickers())
})

monthly_prices <- eventReactive(input$runBacktestButton, {
  all_monthly_prices %>% 
    filter(ticker %in% backtest_tickers())
})

monthly_unadjusted_prices <- eventReactive(input$runBacktestButton, {
  all_monthly_unadjusted %>% 
    filter(ticker %in% backtest_tickers())
})

# backtest start and end dates - reactive on universe selected
startDate <- eventReactive(input$runBacktestButton, {
  # get latest start date of ETFs in universe
  monthly_prices() %>% 
    summarise(min(date)) %>% 
    pull() %>% 
    max()
})

endDate <- eventReactive(input$runBacktestButton, {
  # et earilest end date of ETFs in universe
  monthly_prices() %>% 
    summarise(max(date)) %>% 
    pull() %>% 
    min()
}) 

initDate <- eventReactive(input$runBacktestButton, {
  startDate() - 1 # initDate is day before startdate
})

# plot aesthetics (names for aesthetic scales) - reactive on universe selected
app_cols <- eventReactive(input$runBacktestButton, {
  app_cols <- gg_color_hue(5)
  names(app_cols) <- c(backtest_tickers(), "Cash", "Portfolio")
  
  app_cols
})

app_col_scale <- eventReactive(input$runBacktestButton, {
  scale_colour_manual(name = "ticker", values = app_cols())
}) 

app_fill_scale <- eventReactive(input$runBacktestButton, {
  scale_fill_manual(name = "ticker", values = app_cols())
}) 

# Toggle inputs on panel selection ======

observe({
  if(input$backtestPanel == "ewbhTab") {
    shinyjs::disable(id = "volLookbackSlider")
    shinyjs::disable(id = "targetVolSlider1")
    shinyjs::disable(id = "targetVolSlider2")
    shinyjs::disable(id = "targetVolSlider3")
    shinyjs::disable(id = "rebalFreqSlider")
    shinyjs::disable(id = "capFreqSlider")
    shinyjs::disable(id = "sameVolCheckbox")
  } else if (input$backtestPanel == "ewrebalTab") {
    shinyjs::disable(id = "volLookbackSlider")
    shinyjs::disable(id = "targetVolSlider1")
    shinyjs::disable(id = "targetVolSlider2")
    shinyjs::disable(id = "targetVolSlider3")
    shinyjs::enable(id = "rebalFreqSlider")
    shinyjs::enable(id = "capFreqSlider")
    shinyjs::disable(id = "sameVolCheckbox")
  } else {
    shinyjs::enable(id = "rebalFreqSlider")
    shinyjs::enable(id = "capFreqSlider")
    shinyjs::enable(id = "volLookbackSlider")
    shinyjs::enable(id = "targetVolSlider1")
    shinyjs::enable(id = "targetVolSlider2")
    shinyjs::enable(id = "targetVolSlider3")
    shinyjs::enable(id = "sameVolCheckbox")
  }
})

# Vol target sliders control logic ====

# force vol target sliders to move together unless asset-specific vol checkbox is checked
observeEvent(input$targetVolSlider1, {
  if(!input$sameVolCheckbox) {
    updateSliderInput(inputId = "targetVolSlider2", value = input$targetVolSlider1)
    updateSliderInput(inputId = "targetVolSlider3", value = input$targetVolSlider1)
  }
})

observeEvent(input$targetVolSlider2, {
  if(!input$sameVolCheckbox) {
    updateSliderInput(inputId = "targetVolSlider1", value = input$targetVolSlider2)
    updateSliderInput(inputId = "targetVolSlider3", value = input$targetVolSlider2)
  }
})

observeEvent(input$targetVolSlider3, {
  if(!input$sameVolCheckbox) {
    updateSliderInput(inputId = "targetVolSlider1", value = input$targetVolSlider3)
    updateSliderInput(inputId = "targetVolSlider2", value = input$targetVolSlider3)
  }
})

observeEvent(input$sameVolCheckbox, {
  # when checkbox is unchecked, set sliders 2 and 3 to value of 1
  if(!input$sameVolCheckbox) {
    updateSliderInput(inputId = "targetVolSlider2", value = input$targetVolSlider1)
    updateSliderInput(inputId = "targetVolSlider3", value = input$targetVolSlider1)
  }
})

vol_targets <- reactive({
  if(input$sameVolCheckbox) {
    vt <- c(
      input$targetVolSlider1/100.,
      input$targetVolSlider2/100.,
      input$targetVolSlider3/100.
    )
    
    setNames(vt, backtest_tickers())
  } else {
    input$targetVolSlider1/100.
  }
})

# Initialise objects for backtest calcs, update backtest on button click ====

# put all bt calcs on observe event, select what to run basis tab selected
init_eq <- reactiveValues(data = NULL)
ew_norebal <- reactiveValues(data = NULL)
ew_rebal <- reactiveValues(data = NULL) 
volsize_prices <- reactiveValues(data = NULL)
rp_rebal <- reactiveValues(data = NULL)

observeEvent(input$runBacktestButton, {
  # set init_eq for use in later reactive output (prevents table updating without button click)
  init_eq$data <- input$initEqSlider
  
  if(input$backtestPanel == "ewbhTab") {
    # ew no rebal calculations
    shares <- num_shares(monthly_prices(), input$initEqSlider*input$maxLeverageSlider, start_date = startDate())
    pos <- monthly_prices() %>% 
      ew_norebal_positions(shares, input$commKnob/100., as.double(input$minCommKnob))
    
    initcashbal <- pos %>%
      get_init_cash_bal(input$initEqSlider, start_date = startDate())
    
    pos <- pos %>% 
      bind_cash_positions(initcashbal, input$initEqSlider, input$marginInterestSlider, start_date = startDate())
    
    ew_norebal$data <- pos %>% 
      adust_bh_backtest_for_margin_calls(monthly_prices(), input$initEqSlider, input$commKnob/100., as.double(input$minCommKnob), input$marginInterestSlider)
    
  } else if (input$backtestPanel == "ewrebalTab") {
    # ew rebal calcs
    ew_rebal$data <- share_based_backtest(monthly_prices(), unadjusted_prices = monthly_unadjusted_prices(), input$initEqSlider, input$capFreqSlider, input$rebalFreqSlider,  input$commKnob/100., as.double(input$minCommKnob), margin_interest_rate = input$marginInterestSlider, rebal_method = "ew", leverage = input$maxLeverageSlider)
    
  } else {
    # rp calcs
    theosize_constrained <- calc_vol_target(prices(), tickers = backtest_tickers(), input$volLookbackSlider, vol_targets()) %>% 
      cap_leverage(max_leverage = input$maxLeverageSlider)
    
    # Get the snapshots at the month end boundaries
    volsize_prices$data <- monthly_prices() %>%
      inner_join(select(theosize_constrained, ticker, date, theosize_constrained), by = c('ticker','date'))
    
    # backtest
    rp_rebal$data <- share_based_backtest(volsize_prices$data, unadjusted_prices = monthly_unadjusted_prices(), input$initEqSlider, input$capFreqSlider, input$rebalFreqSlider, input$commKnob/100., as.double(input$minCommKnob), margin_interest_rate = input$marginInterestSlider, rebal_method = "rp", leverage = input$maxLeverageSlider)
  }
})

# EW backtest reactives =================

output$ewbhEqPlot <- renderPlot({
  req(input$runBacktestButton)
  if(is.null(ew_norebal$data))
    return()
  
  # get portfolio NAV ("net exposure") and add to dataframe
  port_nav <- ew_norebal$data %>% 
    select(date, exposure, ticker) %>%
    group_by(date) %>%
    summarise(exposure = sum(exposure)) %>%
    mutate(ticker = "NAV")
  
  # TODO: get nav and maintenance margin as in bh_margin_call()
  # then pass to stacked_area_chart to include main-margin line on plot
  # print(port_nav)
  
  ew_norebal$data %>% 
    select(date, exposure, ticker) %>%
    bind_rows(port_nav) %>%
    stacked_area_chart(title = 'Equal Weight, No Rebalancing', tickers = backtest_tickers(), colours = app_cols())
})

output$ewbhRollPerfPlot <- renderPlot({
  if(is.null(ew_norebal$data))
    return()
  ew_norebal$data %>% 
    combine_port_asset_returns(monthly_prices()) %>% 
    rolling_ann_port_perf() %>% 
    rolling_ann_port_perf_plot(tickers = backtest_tickers(), ticker_colours = app_cols(), colour_scale = app_col_scale()) 
})

output$ewbhTradesPlot <- renderPlot({
  if(is.null(ew_norebal$data))
    return()
  ew_norebal$data %>% 
    trades_chart(title = 'Trades', fill_scale = app_fill_scale(), colour_scale = app_col_scale()) 
})

output$ewbhCommPlot <- renderPlot({
  if(is.null(ew_norebal$data))
    return()
  ew_norebal$data %>% 
    comm_chart(title = 'Commission ($)', fill_scale = app_fill_scale(), colour_scale = app_col_scale()) 
})

output$ewbhCommExpPlot <- renderPlot({
  if(is.null(ew_norebal$data))
    return()
  ew_norebal$data %>% 
    comm_pct_exp_chart(title = 'Commission as pct of exposure', fill_scale = app_fill_scale(), colour_scale = app_col_scale()) 
})

output$ewbhInterestPlot <- renderPlot({
  if(is.null(ew_norebal$data))
    return()
  ew_norebal$data %>% 
    interest_chart(title = 'Interest ($)', fill_scale = app_fill_scale(), colour_scale = app_col_scale()) 
})

output$ewbhInterestRatePlot <- renderPlot({
  if(is.null(ew_norebal$data))
    return()
  monthly_yields %>% 
    interest_rate_chart(input$marginInterestSlider, 'Margin Interest Rate Spread') 
})

output$ewbhPerfTable <- renderTable({
  if(is.null(ew_norebal$data))
    return()
  ew_norebal$data %>% 
    summary_performance(init_eq$data, start_date = startDate(), end_date = endDate()) 
})

output$ewbhTradesTable <- renderDataTable({
  if(is.null(ew_norebal$data))
    return()
  ew_norebal$data %>% 
    mutate(across(where(is.numeric), as.numeric)) %>%  # alleged fix for datatable instability...
    DT::datatable(options = list(order = list(list(2, 'desc'), list(1, 'asc'))))
}, server = FALSE)

# EW rebalanced backtest reactives =================

output$ewrebEqPlot <- renderPlot({
  if(is.null(ew_rebal$data))
    return()
  
  # get portfolio NAV ("net exposure") and add to dataframe
  port_nav <- ew_rebal$data %>% 
    select(date, exposure, ticker) %>% 
    group_by(date) %>% 
    summarise(exposure = sum(exposure)) %>% 
    mutate(ticker = "NAV")
  
  ew_rebal$data %>% 
    select(date, ticker, exposure) %>% 
    bind_rows(port_nav) %>% 
    stacked_area_chart(title = 'Equal Weight, Rebalancing', tickers = backtest_tickers(), colours = app_cols())
})

output$ewrebRollPerfPlot <- renderPlot({
  if(is.null(ew_rebal$data))
    return()
  ew_rebal$data %>% 
    combine_port_asset_returns(monthly_prices()) %>% 
    rolling_ann_port_perf() %>% 
    rolling_ann_port_perf_plot(tickers = backtest_tickers(), ticker_colours = app_cols(), colour_scale = app_col_scale())
})

output$ewrebTradesPlot <- renderPlot({
  if(is.null(ew_rebal$data))
    return()
  ew_rebal$data %>% 
    trades_chart(title = 'Trades', fill_scale = app_fill_scale(), colour_scale = app_col_scale())
})

output$ewrebCommPlot <- renderPlot({
  if(is.null(ew_rebal$data))
    return()
  ew_rebal$data %>% 
    comm_chart(title = 'Commission ($)', fill_scale = app_fill_scale(), colour_scale = app_col_scale())
})

output$ewrebCommExpPlot <- renderPlot({
  if(is.null(ew_rebal$data))
    return()
  ew_rebal$data %>% 
    comm_pct_exp_chart(title = 'Commission as pct of exposure', fill_scale = app_fill_scale(), colour_scale = app_col_scale())
})

output$ewrebInterestPlot <- renderPlot({
  if(is.null(ew_rebal$data))
    return()
  ew_rebal$data %>% 
    interest_chart(title = 'Interest ($)', fill_scale = app_fill_scale(), colour_scale = app_col_scale()) 
})

output$ewrebInterestRatePlot <- renderPlot({
  if(is.null(ew_rebal$data))
    return()
  monthly_yields %>% 
    interest_rate_chart(input$marginInterestSlider, 'Margin Interest Rate Spread') 
})

# TODO: figure out why performance output is repeated in shiny output
output$ewrebPerfTable <- renderTable({
  if(is.null(ew_rebal$data))
    return()
  ew_rebal$data %>% 
    summary_performance(init_eq$data, start_date = startDate(), end_date = endDate()) %>% 
    slice(1)
})

output$ewrebTradesTable <- renderDataTable({
  if(is.null(ew_rebal$data))
    return()
  ew_rebal$data %>% 
    mutate(across(where(is.numeric), as.numeric)) %>%  # alleged fix for datatable instability...
    DT::datatable(options = list(order = list(list(2, 'desc'), list(1, 'asc'))))
}, server = FALSE)


# RP backtest reactives =================

output$rpTheoSizePlot <- renderPlot({
  if(is.null(volsize_prices$data))
    return()
  volsize_prices$data %>% 
    constrained_sizing_plot(title = 'Theoretical Constrained Sizing (% of Portfolio Equity', fill_scale = app_fill_scale())
})

output$rpEqPlot <- renderPlot({
  if(is.null(rp_rebal$data))
    return()
  
  # get portfolio NAV ("net exposure") and add to dataframe
  port_nav <- rp_rebal$data %>% 
    select(date, exposure, ticker) %>% 
    group_by(date) %>% 
    summarise(exposure = sum(exposure)) %>% 
    mutate(ticker = "NAV")
  
  rp_rebal$data %>% 
    select(date, exposure, ticker) %>% 
    bind_rows(port_nav) %>% 
    arrange(date) %>% 
    stacked_area_chart(title = 'Risk Parity', tickers = backtest_tickers(), colours = app_cols())
})

output$rpRollPerfPlot <- renderPlot({
  if(is.null(rp_rebal$data))
    return()
  rp_rebal$data %>% 
    combine_port_asset_returns(monthly_prices()) %>% 
    rolling_ann_port_perf() %>% 
    rolling_ann_port_perf_plot(tickers = backtest_tickers(), ticker_colours = app_cols(), colour_scale = app_col_scale())
})

output$rpTradesPlot <- renderPlot({
  if(is.null(rp_rebal$data))
    return()
  rp_rebal$data %>% 
    trades_chart(title = 'Trades', fill_scale = app_fill_scale(), colour_scale = app_col_scale())
})

output$rpCommPlot <- renderPlot({
  if(is.null(rp_rebal$data))
    return()
  rp_rebal$data %>% 
    comm_chart(title = 'Commission ($)', fill_scale = app_fill_scale(), colour_scale = app_col_scale())
})

output$rpCommExpPlot <- renderPlot({
  if(is.null(rp_rebal$data))
    return()
  rp_rebal$data %>% 
    comm_pct_exp_chart('Commission as pct of exposure', fill_scale = app_fill_scale(), colour_scale = app_col_scale())
})

output$rprebInterestPlot <- renderPlot({
  if(is.null(rp_rebal$data))
    return()
  rp_rebal$data %>% 
    interest_chart(title = 'Interest ($)', fill_scale = app_fill_scale(), colour_scale = app_col_scale()) 
})

output$rprebInterestRatePlot <- renderPlot({
  if(is.null(rp_rebal$data))
    return()
  monthly_yields %>% 
    interest_rate_chart(input$marginInterestSlider, 'Margin Interest Rate Spread') 
})

# TODO: figure out why performance output is repeated in shiny output
output$rpPerfTable <- renderTable({
  if(is.null(rp_rebal$data))
    return()
  rp_rebal$data %>% 
    summary_performance(init_eq$data, start_date = startDate(), end_date = endDate()) %>% 
    slice(1)
})

output$rpTradesTable <- renderDataTable({
  if(is.null(rp_rebal$data))
    return()
  rp_rebal$data %>% 
    mutate(across(where(is.numeric), as.numeric)) %>%  # alleged fix for datatable instability...
    DT::datatable(options = list(order = list(list(2, 'desc'), list(1, 'asc'))))
}, server = FALSE)