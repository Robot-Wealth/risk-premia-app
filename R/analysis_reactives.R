# reactives for performance and scatterplot tabs - these should be scoped for individual sessions

library(shiny)

df <- reactive({
  prices %>% 
    filter(ticker %in% input$assets)
})

output$cumReturnsPlot <- renderPlot({
  df() %>% 
    total_returns_plot()
})

output$rollingPerfPlot <- renderPlot({
  df() %>% 
    rolling_ann_perf() %>% 
    rolling_ann_perf_plot()
})

output$rollCorrPlot <- renderPlot({
  df() %>% 
    roll_pairwise_corrs() %>% 
    roll_pairwise_corrs_plot(facet = FALSE)
})

output$corrMatPlot <- renderPlot({
  df() %>% 
    cormat() %>% 
    cormat_plot()
})

output$laggedReturnsPlot <- renderPlot({
  df() %>% 
    lagged_returns_scatterplot(estimation_wdw = input$estWdwSize, forward_wdw = input$fwdWdwSize, remove_overlapping = input$removeOverlapping)
})

output$laggedVolPlot <- renderPlot({
  df() %>% 
    lagged_vol_scatterplot(estimation_wdw = input$estWdwSize, forward_wdw = input$fwdWdwSize, remove_overlapping = input$removeOverlapping)
})


output$volTargetPlot <- renderPlot({
  df() %>%
    vol_target_plot(estimation_wdw = input$estWdwSize, rebal_threshold =input$rebalThreshold , vol_target = input$volTarget/100 )
    
})