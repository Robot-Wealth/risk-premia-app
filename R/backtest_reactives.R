# Reactives and their dependencies for backtesting tabs

library(here)
library(tidyverse)
library(glue)

# Toggle inputs on panel selection ======

observe({
  if(input$backtestPanel == "ewbhTab") {
    shinyjs::disable(id = "volLookbackSlider")
    shinyjs::disable(id = "targetVolSlider")
    shinyjs::disable(id = "rebalFreqSlider")
    shinyjs::disable(id = "capFreqSlider")
    
  } else if (input$backtestPanel == "ewrebalTab") {
    shinyjs::disable(id = "volLookbackSlider")
    shinyjs::disable(id = "targetVolSlider")
    shinyjs::enable(id = "rebalFreqSlider")
    shinyjs::enable(id = "capFreqSlider")
  } else {
    shinyjs::enable(id = "rebalFreqSlider")
    shinyjs::enable(id = "capFreqSlider")
    shinyjs::enable(id = "volLookbackSlider")
    shinyjs::enable(id = "targetVolSlider")
  }
})

# EW backtest reactives =================

ew_norebal <- reactive({
  shares <- num_shares(monthlyprices, input$initEqSlider)
  
  pos <- monthlyprices %>% 
    ew_norebal_positions(shares, input$commKnob/100., as.double(input$minCommKnob))
  
  initcashbal <- pos %>%
    get_init_cash_bal(input$initEqSlider)
  
  pos <- pos %>% 
    bind_cash_positions(initcashbal, input$initEqSlider)
  
})

output$ewbhEqPlot <- renderPlot({
  ew_norebal() %>% 
    stacked_area_chart('3 ETF USD Risk Premia - Equal Weight, No Rebalancing')
})

output$ewbhRollPerfPlot <- renderPlot({
  ew_norebal() %>% 
    combine_port_asset_returns(monthlyprices) %>% 
    rolling_ann_port_perf() %>% 
    rolling_ann_port_perf_plot()
})

output$ewbhTradesPlot <- renderPlot({
  ew_norebal() %>% 
    trades_chart('3 ETF USD Risk Premia - Trades')
})

output$ewbhCommPlot <- renderPlot({
  ew_norebal() %>% 
    comm_chart('3 ETF USD Risk Premia -  Commission ($)')
})

output$ewbhCommExpPlot <- renderPlot({
  ew_norebal() %>% 
    comm_pct_exp_chart('3 ETF USD Risk Premia -  Commission as pct of exposure')
})

output$ewbhPerfTable <- renderTable({
  ew_norebal() %>% 
    summary_performance(input$initEqSlider)
})

output$ewbhTradesTable <- renderDataTable({
  ew_norebal() %>% 
    mutate(across(where(is.numeric), as.numeric)) %>%  # alleged fix for datatable instability...
    DT::datatable(options = list(order = list(list(2, 'desc'), list(1, 'asc'))))
}, server = FALSE)

# EW rebalanced backtest reactives =================

ew_rebal <- reactive({
  share_based_backtest(monthlyprices, input$initEqSlider, input$capFreqSlider, input$rebalFreqSlider,  input$commKnob/100., as.double(input$minCommKnob))
})

output$ewrebEqPlot <- renderPlot({
  ew_rebal() %>% 
    stacked_area_chart('3 ETF USD Risk Premia - Equal Weight, Rebalancing')
})

output$ewrebRollPerfPlot <- renderPlot({
  ew_rebal() %>% 
    combine_port_asset_returns(monthlyprices) %>% 
    rolling_ann_port_perf() %>% 
    rolling_ann_port_perf_plot()
})

output$ewrebTradesPlot <- renderPlot({
  ew_rebal() %>% 
    trades_chart('3 ETF USD Risk Premia - Trades')
})

output$ewrebCommPlot <- renderPlot({
  ew_rebal() %>% 
    comm_chart('3 ETF USD Risk Premia -  Commission ($)')
})

output$ewrebCommExpPlot <- renderPlot({
  ew_rebal() %>% 
    comm_pct_exp_chart('3 ETF USD Risk Premia -  Commission as pct of exposure')
})

# TODO: figure out why performance output is repeated in shiny output
output$ewrebPerfTable <- renderTable({
  ew_rebal() %>% 
    summary_performance(input$initEqSlider) %>% 
    slice(1)
})

output$ewrebTradesTable <- renderDataTable({
  ew_rebal() %>% 
    mutate(across(where(is.numeric), as.numeric)) %>%  # alleged fix for datatable instability...
    DT::datatable(options = list(order = list(list(2, 'desc'), list(1, 'asc'))))
}, server = FALSE)


# RP backtest reactives =================

volsize_prices <- reactive({
  theosize_constrained <- calc_vol_target(prices, input$volLookbackSlider, input$targetVolSlider/100.) %>% 
    cap_leverage()
  
  # Get the snapshots at the month end boundaries
  monthlyprices %>%
    inner_join(select(theosize_constrained, ticker, date, theosize_constrained), by = c('ticker','date'))
})

output$rpTheoSizePlot <- renderPlot({
  volsize_prices() %>% 
    constrained_sizing_plot(title = '3 ETF USD Risk Premia - Theoretical Constrained Sizing (% of Portfolio Equity')
})


rp_rebal <- reactive({
  share_based_backtest(volsize_prices(), input$initEqSlider, input$capFreqSlider, input$rebalFreqSlider, input$commKnob/100., as.double(input$minCommKnob), rebal_method = "rp")
})

output$rpEqPlot <- renderPlot({
  rp_rebal() %>% 
    stacked_area_chart('3 ETF USD Risk Premia - Simple Risk Parity')
})

output$rpRollPerfPlot <- renderPlot({
  rp_rebal() %>% 
    combine_port_asset_returns(monthlyprices) %>% 
    rolling_ann_port_perf() %>% 
    rolling_ann_port_perf_plot()
})

output$rpTradesPlot <- renderPlot({
  rp_rebal() %>% 
    trades_chart('3 ETF USD Risk Premia - Trades')
})

output$rpCommPlot <- renderPlot({
  rp_rebal() %>% 
    comm_chart('3 ETF USD Risk Premia - Commission ($)')
})

output$rpCommExpPlot <- renderPlot({
  rp_rebal() %>% 
    comm_pct_exp_chart('3 ETF USD Risk Premia -  Commission as pct of exposure')
})

# TODO: figure out why performance output is repeated in shiny output
output$rpPerfTable <- renderTable({
  rp_rebal() %>% 
    summary_performance(input$initEqSlider) %>% 
    slice(1)
})

output$rpTradesTable <- renderDataTable({
  rp_rebal() %>% 
    mutate(across(where(is.numeric), as.numeric)) %>%  # alleged fix for datatable instability...
    DT::datatable(options = list(order = list(list(2, 'desc'), list(1, 'asc'))))
}, server = FALSE)