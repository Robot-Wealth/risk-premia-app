library(tidyverse)
library(glue)
library(roll)

recode_av_columns <- function(av_df) {
  # change columns of df from AV to RW format
  # symbol --> ticker
  # adjusted --> closeadjusted
}

add_total_returns_col <- function(prices_df) {
  prices_df %>% 
    arrange(date) %>%
    group_by(ticker) %>%
    mutate(totalreturns = ((closeadjusted) / dplyr::lag(closeadjusted)) - 1) %>%
    na.omit() %>%
    mutate(cumreturns = cumprod(1+totalreturns))
}


unadjusted_prices_plot <- function(prices_df) {
  prices_df %>% 
    ggplot(aes(x=date, y = close, color = ticker)) + geom_line()
}

adjusted_prices_plot <- function(prices_df) {
  prices_df %>% 
    ggplot(aes(x=date, y = closeadjusted, color = ticker)) + geom_line()
}

total_returns_plot <- function(returns_df) {
  returns_df %>% 
    ggplot(aes(x=date, y = cumreturns, color = ticker)) + geom_line() +
      labs(
        x = "Date",
        y = "Cumulative Return",
        title = "Cumulative Total Returns"
      )
  
}

rolling_ann_perf <- function(returns_df) {
  returns_df %>% 
    group_by(ticker) %>% 
    arrange(date) %>% 
    mutate(
      roll_ann_return = sqrt(250)*roll_mean(totalreturns, width = 250),
      roll_ann_sd = sqrt(250)*roll_sd(totalreturns, width = 250),
      roll_sharpe = roll_ann_return/roll_ann_sd
    ) %>% 
    select(date, ticker, roll_ann_return, roll_ann_sd, roll_sharpe) %>% 
    pivot_longer(cols = c(-ticker, -date), names_to = "metric", values_to = "value")
}

rolling_ann_perf_plot <- function(perf_df) {
  metric_names <- as_labeller(c(
    `roll_ann_return` = "Return",
    `roll_ann_sd` = "Volatility",
    `roll_sharpe` = "Sharpe"
  ))
  
  perf_df %>% 
    ggplot(aes(x = date, y = value, colour = ticker)) +
    geom_line() +
    facet_wrap(~metric, scales = "free_y", ncol = 1, labeller = metric_names) +
    labs(
      x = "Date",
      y = "Value",
      title = "Rolling Annualised Performance Statistics"
    )
}

# Correlation calcualtions ==============

# full join on date
fjoin_on_date <- function(df) {
  df %>%
    full_join(df, by = "date")
}

# ditch corr matrix diagonal, one half
wrangle_combos <- function(combinations_df) {
  combinations_df %>%
    ungroup() %>% 
    # drop diagonal 
    filter(ticker.x != ticker.y) %>% 
    # remove duplicate pairs (eg A-AAL, AAL-A)
    mutate(tickers = ifelse(ticker.x < ticker.y, glue("{ticker.x}, {ticker.y}"), glue("{ticker.y}, {ticker.x}"))) %>%
    distinct(date, tickers, .keep_all = TRUE) 
} 

pairwise_corrs <- function(combination_df, period) {
  combination_df %>%
    group_by(tickers) %>%
    arrange(date, .by_group = TRUE) %>%
    mutate(rollingcor = slider::slide2_dbl(
      .x = totalreturns.x, 
      .y = totalreturns.y, 
      .f = ~cor(.x, .y), 
      .before = period, 
      .complete = TRUE)
    ) %>%
    select(date, tickers, rollingcor)
  
} 

mean_pw_cors <- function(correlations_df) {
  correlations_df %>%
    group_by(date) %>%
    summarise(mean_pw_corr = mean(rollingcor, na.rm = TRUE))
} 


roll_pairwise_corrs <- function(returns_df) {
  returns_df %>% 
    fjoin_on_date() %>% 
    wrangle_combos() %>% 
    pairwise_corrs(250) 
  
} 
  
roll_pairwise_corrs_plot <- function(roll_corr_df, facet = FALSE) {
  lab <- c(x = "Date", y = "Correlation", title = "Rolling 12-month Correlation")
  
  if(facet) {
    roll_corr_df %>% 
      group_by(tickers) %>% 
      ggplot(aes(x = date, y = rollingcor)) + 
      geom_line() + 
      facet_wrap(~tickers, ncol = 1) +
      labs(
        x = lab["x"],
        y = lab["y"],
        title = lab["title"]
      )
  } else {
    roll_corr_df %>% 
      group_by(tickers) %>% 
      ggplot(aes(x = date, y = rollingcor, colour = tickers)) + 
      geom_line()  +
      labs(
        x = lab["x"],
        y = lab["y"],
        title = lab["title"]
      )
  }
}


cormat <- function(returns_df) {
  returns_df %>% 
    select(date, ticker, totalreturns) %>% 
    pivot_wider(names_from = ticker, values_from = totalreturns) %>% 
    select(-date) %>% 
    cor(method = "pearson", use = "complete.obs")
} 

cormat_plot <- function(cor_mat) {
  cor_mat %>% 
    as.data.frame() %>% 
    rownames_to_column("ticker.x") %>% 
    pivot_longer(-ticker.x, names_to = "ticker.y", values_to = "correlation") %>% 
    mutate(ticker.y = fct_rev(factor(ticker.y))) %>% 
    ggplot(aes(x = ticker.x, y = ticker.y, fill = correlation)) +
      geom_tile() +
      scale_fill_gradient2(low = "#F065DD", mid = "#FFFFFF", high = "#092580", limits = c(-1, 1)) +
      labs(title = "Correlation Matrix", fill = "Correlation") +
      theme(axis.title.x = element_blank(), axis.title.y = element_blank())
}
    
# cormat_plot(cormat)

# scatterplots ==========================
"
can't predict returns scatterplot - showing returns against lagged (non-overlapping) returns over various horizons
can predict vol scatterplot - showing vol against lagged (non-overlapping) vol over various horizons
vol-price-dependency scatterplot - vol vs lagged momentum

"
lagged_returns_scatterplot <- function(returns_df, estimation_wdw, forward_wdw, remove_overlapping = TRUE) {
  
  if(remove_overlapping) {
    returns_df %>%
      group_by(ticker) %>% 
      arrange(date) %>% 
      mutate(
        estimation_return = sqrt(250)*dplyr::lag(roll_mean(totalreturns, width = estimation_wdw), 1),
        forward_return = sqrt(250)*dplyr::lead(roll_mean(totalreturns, forward_wdw), forward_wdw - 1)
      ) %>%  
      filter(row_number() %% min(c(estimation_wdw, forward_wdw)) == 0) %>%
      na.omit() %>% 
      ggplot(aes(x = estimation_return, y = forward_return)) +
        geom_point() +
        geom_smooth(method=lm) + 
        facet_wrap(~ticker, scales = 'free', ncol = 1) +
        labs(
          x = "Estimation Window Mean Return",
          y = "Forward Window Mean Return",
          title = "Returns vs Forward Returns (annualised)",
          subtitle = "Are returns predictive of future returns?"
        )
  } else {
    returns_df %>%
      group_by(ticker) %>% 
      arrange(date) %>% 
      mutate(
        estimation_return = sqrt(250)*dplyr::lag(roll_mean(totalreturns, width = estimation_wdw), 1),
        forward_return = sqrt(250)*dplyr::lead(roll_mean(totalreturns, forward_wdw), forward_wdw - 1)
      ) %>%  
      na.omit() %>% 
      ggplot(aes(x = estimation_return, y = forward_return)) +
      geom_point() +
      geom_smooth(method=lm) +
      facet_wrap(~ticker, scales = 'free', ncol = 1) +
    labs(
      x = "Estimation Window Mean Return",
      y = "Forward Window Mean Return",
      title = "Returns vs Forward Returns (annualised)",
      subtitle = "Are returns predictive of future returns?"
    )
  }
}

lagged_vol_scatterplot <- function(returns_df, estimation_wdw, forward_wdw, remove_overlapping = TRUE) {
  if(remove_overlapping) {
    returns_df %>%
      group_by(ticker) %>% 
      arrange(date) %>% 
      mutate(
        estimation_vol = sqrt(250)*dplyr::lag(roll_sd(totalreturns, width = estimation_wdw), 1),
        forward_vol = sqrt(250)*dplyr::lead(roll_sd(totalreturns, forward_wdw), forward_wdw - 1)
      ) %>%  
      filter(row_number() %% min(c(estimation_wdw, forward_wdw)) == 0) %>%
      na.omit() %>% 
      ggplot(aes(x = estimation_vol, y = forward_vol)) +
        geom_point() +
        geom_smooth(method=lm) +
        facet_wrap(~ticker, scales = 'free', ncol = 1) +
      labs(
        x = "Estimation Window Volatility",
        y = "Forward Window Volatility",
        title = "Volatility vs Forward Volatility (annualised)",
        subtitle = "Is volatility predictive of future volatility?"
      )
  } else {
    returns_df %>%
      group_by(ticker) %>% 
      arrange(date) %>% 
      mutate(
        estimation_vol = sqrt(250)*dplyr::lag(roll_sd(totalreturns, width = estimation_wdw), 1),
        forward_vol = sqrt(250)*dplyr::lead(roll_sd(totalreturns, forward_wdw), forward_wdw - 1)
      ) %>%  
      na.omit() %>% 
      ggplot(aes(x = estimation_vol, y = forward_vol)) +
        geom_point() +
        geom_smooth(method=lm) +
        facet_wrap(~ticker, scales = 'free', ncol = 1) +
      labs(
        x = "Estimation Window Volatility",
        y = "Forward Window Volatility",
        title = "Volatility vs Forward Volatility (annualised)",
        subtitle = "Is volatility predictive of future volatility?"
      )
  }
}


# using slider to get rolling windows, use desired window length - 1 to get the current observation plus the previous (wdw-1), then
# lag by 1 
# tlt <- prices %>%
#   group_by(ticker) %>%
#   filter(ticker == "TLT") %>%
#   arrange(date)
# 
# wdw <- 6
# fwd_wdw <- 4
# mean(tlt$totalreturns[1:wdw])
# 
# tlt %>%
#   mutate(r = dplyr::lag(slide_dbl(totalreturns, .f = mean, .before = wdw-1, .complete = TRUE), 1)) %>%
#   select(r)
# 
# # for fwd returns, we need to use wdw-1 again, but no need to lead/lag as we want to include the current return in the fwd calculation.
# tlt %>%
#   mutate(r = slide_dbl(totalreturns, .f = mean, .after = fwd_wdw-1, .complete = TRUE)) %>%
#   select(date, r)
# 
# # likewise with roll need to lag by 1 for the estimation window
# tlt %>%
#   mutate(r = dplyr::lag(roll_mean(totalreturns, width = wdw), 1)) %>%
#   select(r)
# 
# # but for fwd window lead by wdw-1 to include the current day's return in the calculaton:
# tlt %>%
#   mutate(f = roll_mean(totalreturns, width = fwd_wdw))
# 
# tlt %>%
#   mutate(f = dplyr::lead(roll_mean(totalreturns, width = fwd_wdw), fwd_wdw-1))



# lagged_returns_scatterplot(prices, 30)
# lagged_vol_scatterplot(prices, 30)

# library(roll)
# library(slider)
# library(microbenchmark)
# 
# mb <- microbenchmark(
# 
# roll <- prices %>% 
#     group_by(ticker) %>% 
#     arrange(date) %>% 
#     mutate(rolling_ann_return = roll_mean(totalreturns, width = 250)),
#   
# slider <-  prices %>% 
#     group_by(ticker) %>% 
#     arrange(date) %>% 
#     mutate(rolling_ann_return = slide_dbl(totalreturns, .f = mean, .before = 250))
#   
# )
# 
# boxplot(mb, log = FALSE)

# stacked performance plots of a single ticker
# prices %>% 
#   group_by(ticker) %>% 
#   arrange(date) %>% 
#   mutate(
#     roll_ann_return = roll_mean(totalreturns, width = 250),
#     roll_ann_sd = roll_sd(totalreturns, width = 250),
#     roll_sharpe = sqrt(250)*roll_ann_return/roll_ann_sd
#   ) %>% 
#   select(ticker, date, roll_ann_return, roll_ann_sd, roll_sharpe) %>% 
#   pivot_longer(cols = c(-ticker, -date), names_to = "metric", values_to = "value") %>% 
#   filter(ticker == "VTI") %>% 
#   ggplot(aes(x = date, y = value)) +
#     geom_line() +
#     facet_wrap(~metric, scales = "free_y", ncol = 1)

# stacked performance plots of all tickers
# prices %>%
#   group_by(ticker) %>%
#   arrange(date) %>%
#   mutate(
#     roll_ann_return = roll_mean(totalreturns, width = 250),
#     roll_ann_sd = roll_sd(totalreturns, width = 250),
#     roll_sharpe = sqrt(250)*roll_ann_return/roll_ann_sd
#   ) %>%
#   select(ticker, date, roll_ann_return, roll_ann_sd, roll_sharpe) %>%
#   pivot_longer(cols = c(-ticker, -date), names_to = "metric", values_to = "value") %>%
#   ggplot(aes(x = date, y = value, colour = ticker)) +
#     geom_line() +
#     facet_wrap(~metric, scales = "free_y", ncol = 1)

#   
# prices %>% 
#   group_by(ticker) %>% 
#   tq_performance(Ra = totalreturns, performance_fun = Return.annualized)




 