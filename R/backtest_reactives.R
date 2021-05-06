# Reactives and their dependencies for backtesting tabs - scope for individual sessions

library(here)
library(tidyverse)
library(glue)

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
    
    setNames(vt, rp_tickers)
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
    shares <- num_shares(monthlyprices, input$initEqSlider)
    pos <- monthlyprices %>% 
      ew_norebal_positions(shares, input$commKnob/100., as.double(input$minCommKnob))
    
    initcashbal <- pos %>%
      get_init_cash_bal(input$initEqSlider)
    
    ew_norebal$data <- pos %>% 
      bind_cash_positions(initcashbal, input$initEqSlider)
    
  } else if (input$backtestPanel == "ewrebalTab") {
    # ew rebal calcs
    ew_rebal$data <- share_based_backtest(monthlyprices, input$initEqSlider, input$capFreqSlider, input$rebalFreqSlider,  input$commKnob/100., as.double(input$minCommKnob))
    
  } else {
    # rp calcs
    theosize_constrained <- calc_vol_target(prices, input$volLookbackSlider, vol_targets()) %>% 
      cap_leverage()
    
    # Get the snapshots at the month end boundaries
    volsize_prices$data <- monthlyprices %>%
      inner_join(select(theosize_constrained, ticker, date, theosize_constrained), by = c('ticker','date'))
    
    # backtest
    rp_rebal$data <- share_based_backtest(volsize_prices$data, input$initEqSlider, input$capFreqSlider, input$rebalFreqSlider, input$commKnob/100., as.double(input$minCommKnob), rebal_method = "rp")
  }
})

# EW backtest reactives =================

output$ewbhEqPlot <- renderPlot({
  req(input$runBacktestButton)
  if(is.null(ew_norebal$data))
    return()
  ew_norebal$data %>% 
    stacked_area_chart('3 ETF USD Risk Premia - Equal Weight, No Rebalancing') 
})

output$ewbhRollPerfPlot <- renderPlot({
  if(is.null(ew_norebal$data))
    return()
  ew_norebal$data %>% 
    combine_port_asset_returns(monthlyprices) %>% 
    rolling_ann_port_perf() %>% 
    rolling_ann_port_perf_plot() 
})

output$ewbhTradesPlot <- renderPlot({
  if(is.null(ew_norebal$data))
    return()
  ew_norebal$data %>% 
    trades_chart('3 ETF USD Risk Premia - Trades') 
})

output$ewbhCommPlot <- renderPlot({
  if(is.null(ew_norebal$data))
    return()
  ew_norebal$data %>% 
    comm_chart('3 ETF USD Risk Premia -  Commission ($)') 
})

output$ewbhCommExpPlot <- renderPlot({
  if(is.null(ew_norebal$data))
    return()
  ew_norebal$data %>% 
    comm_pct_exp_chart('3 ETF USD Risk Premia -  Commission as pct of exposure') 
})

output$ewbhPerfTable <- renderTable({
  if(is.null(ew_norebal$data))
    return()
  ew_norebal$data %>% 
    summary_performance(init_eq$data) 
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
  ew_rebal$data %>% 
    stacked_area_chart('3 ETF USD Risk Premia - Equal Weight, Rebalancing')
})

output$ewrebRollPerfPlot <- renderPlot({
  if(is.null(ew_rebal$data))
    return()
  ew_rebal$data %>% 
    combine_port_asset_returns(monthlyprices) %>% 
    rolling_ann_port_perf() %>% 
    rolling_ann_port_perf_plot()
})

output$ewrebTradesPlot <- renderPlot({
  if(is.null(ew_rebal$data))
    return()
  ew_rebal$data %>% 
    trades_chart('3 ETF USD Risk Premia - Trades')
})

output$ewrebCommPlot <- renderPlot({
  if(is.null(ew_rebal$data))
    return()
  ew_rebal$data %>% 
    comm_chart('3 ETF USD Risk Premia -  Commission ($)')
})

output$ewrebCommExpPlot <- renderPlot({
  if(is.null(ew_rebal$data))
    return()
  ew_rebal$data %>% 
    comm_pct_exp_chart('3 ETF USD Risk Premia -  Commission as pct of exposure')
})

# TODO: figure out why performance output is repeated in shiny output
output$ewrebPerfTable <- renderTable({
  if(is.null(ew_rebal$data))
    return()
  ew_rebal$data %>% 
    summary_performance(init_eq$data) %>% 
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

# Backtst outputs ====

output$rpTheoSizePlot <- renderPlot({
  if(is.null(volsize_prices$data))
    return()
  volsize_prices$data %>% 
    constrained_sizing_plot(title = '3 ETF USD Risk Premia - Theoretical Constrained Sizing (% of Portfolio Equity')
})

output$rpEqPlot <- renderPlot({
  if(is.null(rp_rebal$data))
    return()
  rp_rebal$data %>% 
    stacked_area_chart('3 ETF USD Risk Premia - Simple Risk Parity')
})

output$rpRollPerfPlot <- renderPlot({
  if(is.null(rp_rebal$data))
    return()
  rp_rebal$data %>% 
    combine_port_asset_returns(monthlyprices) %>% 
    rolling_ann_port_perf() %>% 
    rolling_ann_port_perf_plot()
})

output$rpTradesPlot <- renderPlot({
  if(is.null(rp_rebal$data))
    return()
  rp_rebal$data %>% 
    trades_chart('3 ETF USD Risk Premia - Trades')
})

output$rpCommPlot <- renderPlot({
  if(is.null(rp_rebal$data))
    return()
  rp_rebal$data %>% 
    comm_chart('3 ETF USD Risk Premia - Commission ($)')
})

output$rpCommExpPlot <- renderPlot({
  if(is.null(rp_rebal$data))
    return()
  rp_rebal$data %>% 
    comm_pct_exp_chart('3 ETF USD Risk Premia -  Commission as pct of exposure')
})

# TODO: figure out why performance output is repeated in shiny output
output$rpPerfTable <- renderTable({
  if(is.null(rp_rebal$data))
    return()
  rp_rebal$data %>% 
    summary_performance(init_eq$data) %>% 
    slice(1)
})

output$rpTradesTable <- renderDataTable({
  if(is.null(rp_rebal$data))
    return()
  rp_rebal$data %>% 
    mutate(across(where(is.numeric), as.numeric)) %>%  # alleged fix for datatable instability...
    DT::datatable(options = list(order = list(list(2, 'desc'), list(1, 'asc'))))
}, server = FALSE)