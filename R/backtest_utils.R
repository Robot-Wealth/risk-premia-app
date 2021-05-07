# Functions supporting backtesting tabs

library(tidyverse)
library(lubridate)

# Setup backtest =======================

get_monthends <- function(prices_df) {
  prices_df %>% 
      mutate(
        year = year(date),
        month = month(date)
      ) %>%
      group_by(year, month) %>%
      summarise(date = max(date))
}

make_monthly_prices <- function(prices_df, monthends) {
  prices_df %>%
    inner_join(monthends, by = 'date') %>%
    select(ticker, date, close = closeadjusted)
  
}

# Buy equal amount, don't rebalance =====

num_shares <- function(monthlyprices_df, equity) {
  monthlyprices_df %>%
    filter(date == startDate) %>%
    mutate(shares = trunc((equity / 3) / close)) %>%
    select(ticker, shares)
  
}

ew_norebal_positions <- function(monthlyprices_df, num_shares, perShareComm, minCommPerOrder) {
  # calculate positions, exposures, trades, commissions
  
  monthlyprices_df %>%
    inner_join(num_shares, by = 'ticker') %>%
    mutate(exposure = shares * close) %>%
    group_by(ticker) %>%
    mutate(trades = shares - lag(shares)) %>%
    # Initial trade to setup position comes through as NA
    mutate(trades = case_when(is.na(trades) ~ shares, TRUE ~ trades)) %>%
    mutate(tradevalue = trades * close) %>%
    mutate(commission = case_when(abs(trades) * perShareComm > minCommPerOrder ~ abs(trades) * perShareComm, trades == 0 ~ 0, TRUE ~ minCommPerOrder))
}

get_init_cash_bal <- function(positions, inital_equity) {
  positions %>% 
    ungroup() %>%
    filter(date == startDate) %>%
    summarise(cash = inital_equity - sum(exposure) - sum(commission)) %>%
    pull()
}

bind_cash_positions <- function(positions, initial_cash_balance, inital_equity, margin_interest_rate) {
  # bind cash balances to positions df
  
  ticker_temp <- positions %>% distinct(ticker) %>% first() %>% pull()
  
  Cash <- positions %>%
    ungroup() %>%
    filter(ticker == ticker_temp) %>% # just doing this to get the dates
    mutate(
      ticker = 'Cash', 
      date, 
      close = 0, 
      shares = 0, 
      exposure = initial_cash_balance, 
      trades = 0, 
      tradevalue = case_when(date == startDate ~ initial_cash_balance - inital_equity, TRUE ~ 0), 
      commission = 0
    ) 
  
  if(initial_cash_balance < 0) {
    # trading with leverage and have negative cash after first time step
    Cash <- Cash %>% 
      mutate(
        exposure = initial_cash_balance*(1 + margin_interest_rate/100/12)^row_number(),
        interest = replace_na(abs(exposure - dplyr::lag(exposure, n = 1)), 0))  # interest paid will appear as a positive 
  }
  
  print(Cash)
  
  positions %>%
    # Bind cash balances and sort by date again
    bind_rows(Cash) %>%
    arrange(date)
}

# Charts ================================

stacked_area_chart <- function(positions, title) {
  positions %>% 
    filter(ticker != "NAV") %>% 
    ggplot(aes(x = date, y = exposure, fill = ticker)) +
      geom_area() +
      # app_fill_scale +
      scale_fill_manual(name = "ticker", values = app_cols, limits = c(rp_tickers, 'Cash')) +
      geom_line(data = positions %>% filter(ticker == "NAV"), aes(x = date, y = exposure, colour = "ticker"), size = 1.5) +
      scale_colour_manual(values = "black", labels = "NAV") +
      labs(
        x = "Date",
        y = "Expsoure Value",
        title = title, 
        colour = ""
      ) 
}

trades_chart <- function(positions, title) {
  positions %>%
    filter(ticker != 'Cash') %>% 
    ggplot(aes(x = date, y = tradevalue, fill = ticker)) +
      geom_bar(stat = 'identity', position = position_dodge()) +
      app_fill_scale +
      labs(
        x = "Date",
        y = "Trade Value",
        title = title
      )
}

comm_chart <- function(positions, title) {
  # Trading cost as $
  
  positions %>%
    filter(ticker != 'Cash') %>% 
    ggplot(aes(x=date, y=commission , fill = ticker)) +
      geom_bar(stat = 'identity') +
      app_fill_scale +
      labs(
        x = "Date",
        y = "Commission",
        title = title
      )
}

comm_pct_exp_chart <- function(positions, title) {
  # Trading cost as % of total exposure in instrument
  
  positions %>%
    filter(ticker != 'Cash') %>% 
    mutate(commissionpct = commission / exposure) %>%
    ggplot(aes(x = date, y = commissionpct , fill = ticker)) +
      geom_bar(stat = 'identity', position = position_dodge()) +
      app_fill_scale +
      labs(
        x = "Date",
        y = "Commission",
        title = title
      )
}

interest_chart <- function(positions, title) {
  # Interest cost as $
  
  positions %>%
    filter(ticker == 'Cash') %>% 
    ggplot(aes(x = date, y = interest, fill = ticker)) +
    geom_bar(stat = 'identity') +
    app_fill_scale +
    labs(
      x = "Date",
      y = "Margin Interest Cost",
      title = title,
      subtitle = "Positive interest paid, negative interest received"
    )
}

# Performance metrics ===================

calc_ann_turnover <- function(positions, mean_equity) {
  # Calculate as total sell trades divided by mean equity * number of years
  
  totalselltrades <- positions %>%
    filter(
      ticker != 'Cash',
      tradevalue < 0,
      date != startDate
    ) %>%
      summarise(sellvalue = sum(tradevalue)) %>%
      pull() 
  
  if(length(totalselltrades) == 0) {
    return(0)
  } else {
    -100*totalselltrades / (mean_equity * (year(endDate) - year(startDate)))
  }
}

calc_port_returns <- function(positions) {
  positions %>%
    group_by(date) %>%
    summarise(
      totalequity = sum(exposure),
      totalcommission = sum(commission)    
    ) %>%
    ungroup() %>%
    arrange(date) %>%
    mutate(returns = totalequity / lag(totalequity) - 1)
}

summary_performance <- function(positions, initial_equity) {
  # binds metrics from tidyquant with ours
  
  port_returns <- positions %>%
    calc_port_returns() 
  
  mean_equity <- mean(port_returns$totalequity)
  total_profit <- tail(port_returns, 1)$totalequity - initial_equity
  costs_pct_profit <- 100*sum(port_returns$totalcommission) / total_profit
  
  port_returns %>% 
    tq_performance(Ra = returns, performance_fun = table.AnnualizedReturns) %>% 
    mutate(
      AnnualizedReturn = 100*AnnualizedReturn,
      AnnualizedStdDev = 100*AnnualizedStdDev
    ) %>% 
    rename(
      "Ann.Return(%)" = AnnualizedReturn,
      "Ann.Sharpe(Rf=0%)" = `AnnualizedSharpe(Rf=0%)`,
      "Ann.Volatility(%)" = AnnualizedStdDev
    ) %>% 
    bind_cols(c(
      port_returns %>%
        tq_performance(Ra = returns, performance_fun = table.DownsideRisk) %>%
        mutate(MaximumDrawdown = 100*MaximumDrawdown) %>% 
        select(MaximumDrawdown) %>% 
        rename("Max.DD(%)" = MaximumDrawdown),
      positions %>% 
        calc_ann_turnover(mean_equity) %>% 
        enframe(name = NULL, value = "Ave.Ann.Turnover(%)"),
      total_profit %>% 
        enframe(name = NULL, value = "Tot.Profit($)"),
      costs_pct_profit %>% 
        enframe(name = NULL, value = "Costs(%Profit)")
    ))
}

combine_port_asset_returns <- function(positions, returns_df) {
  # create df of port and asset returns
  
  positions %>%
    calc_port_returns() %>% 
    select(date, returns) %>%
    mutate(ticker = "Portfolio") %>% 
    bind_rows(returns_df %>% select(ticker, date, returns)) %>% 
    arrange(date)
}

rolling_ann_port_perf <- function(port_returns_df) {
  # rolling performance of portfolio and assets (not asset contribution to portfolio)
  
  port_returns_df %>% 
    group_by(ticker) %>% 
    arrange(date) %>% 
    mutate(
      roll_ann_return = 12*roll_mean(returns, width = 24, min_obs = 24),
      roll_ann_sd = sqrt(12)*roll_sd(returns, width = 24, min_obs = 24),
      roll_sharpe = roll_ann_return/roll_ann_sd
    ) %>% 
    select(date, ticker, roll_ann_return, roll_ann_sd, roll_sharpe) %>% 
    pivot_longer(cols = c(-ticker, -date), names_to = "metric", values_to = "value")
}

rolling_ann_port_perf_plot <- function(perf_df) {
  metric_names <- as_labeller(c(
    `roll_ann_return` = "Return",
    `roll_ann_sd` = "Volatility",
    `roll_sharpe` = "Sharpe"
  ))
  
  perf_df %>% 
    mutate(ticker = factor(ticker, levels = names(app_cols))) %>%
    ggplot(aes(x = date, y = value, colour = ticker, size = ticker)) +
      geom_line() +
      app_col_scale +
      scale_size_manual(
        values = c(rep(0.6, length(rp_tickers)), 1.2),
        labels = waiver()
      ) +
      facet_wrap(~metric, scales = "free_y", ncol = 1, labeller = metric_names) +
      labs(
        x = "Date",
        y = "Value",
        title = "2-year Rolling Annualised Performance Statistics"
      ) 
}

# Backtest ==============================

# TODO:
# - refactor
# - replace for loop with map_df

share_based_backtest <- function(monthlyprices_df, initial_equity, cap_frequency, rebal_frequency, per_share_Comm, min_comm, margin_interest_rate, rebal_method = "ew", leverage = 1) {
  "Margin interest accrues on previous month's Cash balance @ margin_interest_rate/100/12*Cash. Credited to positive Cash, debited from negative Cash.
  leverage parameter used to calculate positions in EW strategy (RP strategy calculates positions upstream). TODO: should make this consistent
  leverage parameter used to flag whether to charge/accrue interest in both cases (don't want to accrue interest when leverage < 1)
  "
  
  stopifnot(rebal_method %in% c("ew", "rp"))
  
  # Create wide data frames for simple share based backtest
  wideprices <- monthlyprices_df %>%
    pivot_wider(date, names_from = 'ticker', values_from = 'close')
  
  if(rebal_method == "rp") {
    widetheosize <- monthlyprices_df %>%
      pivot_wider(date, names_from = 'ticker', values_from = 'theosize_constrained')
  }
  
  rowlist <- list()
  
  Cash <- initial_equity
  sharepos <- c(0,0,0)
  sharevalue <- c(0,0,0)
  equity <- initial_equity
  capEquity <- initial_equity # Sticky equity amount that we're using to allow change cap frequency
  
  # Iterate through prices and backtest
  for (i in 1:(nrow(wideprices))) {
    currentdate <- wideprices[i,1] %>% pull() %>% as.Date()
    currentprice <- as.numeric(wideprices[i,2:4])
    if(rebal_method == "rp") {
      currenttheosize <- as.numeric(widetheosize[i, 2:4])
    }
    
    # charge/accrue interest on last month's balance if leverage > 1
    if(leverage > 1) {
      margin_interest <- Cash * margin_interest_rate/100/12  
    } else { 
      margin_interest <- 0
    }
    
    Cash <- Cash + margin_interest
    equity <- sum(sharepos * currentprice) + Cash
    
    if(equity > 0) {
    
      # force reduce position if exceeds maintenance margin requirements
      maint_margin = 0.25
      margin_call <- FALSE
      liq_shares <- c(0, 0, 0)
      liq_commissions <- c(0, 0, 0)
      if(equity < maint_margin*sum(sharepos * currentprice)) {
        margin_call <- TRUE
        
        # how much stock to liquidate? NOTE: this doesn't include the commission costs or other fees of liquidating in figuring out post-liquidation NAV
        liquidate_factor <- 1 - (equity/maint_margin)/sum(sharepos * currentprice) # 1 - (max share value/current share value)
        
        # liquidate equal proportions of stock
        liq_shares <- liquidate_factor * sharepos
        liq_commissions <- liq_shares * per_share_Comm
        liq_commissions[liq_commissions < min_comm] <- min_comm
        
        Cash <- Cash - sum(liq_shares*currentprice) - sum(liq_commissions) 
        sharepos <- sharepos - liq_shares
        sharevalue <- sharepos * currentprice
        equity <- sum(sharevalue) + Cash
        
      }
      
      # Update capEquity if it's re-capitalisation rebalance time
      if(cap_frequency > 0) {
        if(i %% cap_frequency == 0) capEquity <- equity 
      }
      
      # Update position sizing if its position rebalance time 
      if (i == 1 | i %% rebal_frequency == 0) {
        if(rebal_method == "ew") {
          targetshares <- trunc((leverage * capEquity / 3) / currentprice) 
        } else {
          targetshares <- trunc((capEquity * currenttheosize) / currentprice)  
        }
      }
      
      trades <- targetshares - sharepos
      tradevalue <- trades * currentprice
      commissions <- abs(trades) * per_share_Comm
      commissions[commissions < min_comm] <- min_comm
      
      # Can we do proposed trades given NAV and MM? If not, adjust trade size, value, commissions etc
      post_trade_equity <- sum(targetshares*currentprice) + Cash - sum(tradevalue) - sum(commissions)
      if(post_trade_equity < maint_margin*sum(targetshares * currentprice)) {
        # adjust trade sizes
        max_post_trade_shareval <- (equity - sum(commissions))/maint_margin 
        if(max_post_trade_shareval > 0) {  # this will only work when > 0
          reduce_by <- 1 - max_post_trade_shareval/sum(targetshares*currentprice) 
          targetshares <- targetshares - ceiling(reduce_by*targetshares)
          trades <- targetshares - sharepos
          tradevalue <- trades * currentprice
          commissions <- abs(trades) * per_share_Comm
          commissions[commissions < min_comm] <- min_comm
        }
      }
      
      # Adjust cash by value of trades 
      Cash <- Cash - sum(tradevalue) - sum(commissions)
      sharepos <- targetshares
      sharevalue <- sharepos * currentprice
      equity <- sum(sharevalue) + Cash
      
      if(equity <= 0) {  # rekt
        sharepos <- c(0,0,0)
        sharevalue <- c(0,0,0)
        sharevalue <- c(0,0,0)
        trades <- c(0, 0, 0)
        liq_shares <- c(0, 0, 0)
        commissions <- c(0, 0, 0)
        liq_commissions <- c(0, 0, 0)
        interst <- c(0, 0, 0)
        equity <- 0
        Cash <- 0
      }
      
    } else {  # rekt
      sharepos <- c(0,0,0)
      sharevalue <- c(0,0,0)
      tradevalue <- c(0,0,0)
      trades <- c(0, 0, 0)
      liq_shares <- c(0, 0, 0)
      commissions <- c(0, 0, 0)
      liq_commissions <- c(0, 0, 0)
      interst <- c(0, 0, 0)
      equity <- 0
      Cash <- 0
    }
      
    # Create data frame and add to list
    row_df <- data.frame(
       ticker = c('Cash', colnames(wideprices[2:4])),
       date = rep(currentdate, 4),
       close = c(0,currentprice),
       shares = c(0,sharepos),
       exposure = c(Cash, sharevalue),
       sharetrades = c(0, trades + liq_shares),
       tradevalue = c(-sum(tradevalue), tradevalue),
       commission = c(0, commissions + liq_commissions),
       interest = c(-margin_interest, rep(0, 3)),  # interest debited appears positive like other costs
       margin_call = margin_call  # was there a liquidation event this month?
    )
    
    rowlist[[i]] <- row_df
    
  }
  
  # Combine list into dataframe
  bind_rows(rowlist)
  
}

# Vol targeting functions ===============

calc_vol_target <- function(prices_df, vol_lookback, target_vol) {
  # Calculate vol target sizing on daily data
  stopifnot("target_vol must be either length 1 or same length as unique(tickers)" = length(target_vol) %in% c(1, length(rp_tickers)))
  
  if(length(target_vol) == 1) {
    # same vol target for each asset
    prices_df %>%
      group_by(ticker) %>%
      arrange(date) %>%
      mutate(returns = (closeadjusted / dplyr::lag(closeadjusted)) - 1) %>%
      mutate(vol = slider::slide_dbl(.x = returns, .f = sd, .before = vol_lookback, .complete = TRUE) * sqrt(252)) %>%
      mutate(theosize = lag(target_vol / vol))  
  } else {
    # separate vol targets for each asset
    prices_df %>%
      group_by(ticker) %>%
      arrange(date) %>%
      mutate(returns = (closeadjusted / dplyr::lag(closeadjusted)) - 1) %>%
      mutate(vol = slider::slide_dbl(.x = returns, .f = sd, .before = vol_lookback, .complete = TRUE) * sqrt(252)) %>%
      mutate(theosize = case_when(
        ticker == rp_tickers[1] ~ lag(target_vol[rp_tickers[1]] / vol),
        ticker == rp_tickers[2] ~ lag(target_vol[rp_tickers[2]] / vol),
        ticker == rp_tickers[3] ~ lag(target_vol[rp_tickers[3]] / vol),
        TRUE ~ as.numeric(NA)
        )
      )
  }
  
}

cap_leverage <- function(vol_targets, max_leverage = 1) {
  # Enforce leverage constraint
  
  total_size <- vol_targets %>%
    group_by(date) %>%
    summarise(totalsize = sum(theosize)) %>%
    mutate(adjfactor = case_when(totalsize > max_leverage ~ max_leverage/totalsize, TRUE ~ 1))
  
  vol_targets %>%
    inner_join(total_size, by = 'date') %>%
    mutate(theosize_constrained = theosize * adjfactor) %>%
    select(ticker, date, closeadjusted, returns, vol, theosize, theosize_constrained) %>%
    na.omit()
}

constrained_sizing_plot <- function(volsized_prices, title) {
  # Plot theoretical constrained sizing as a function of time
  
  volsized_prices %>%
    ggplot(aes(x = date, y = theosize_constrained, fill = ticker)) +
    geom_area() + 
    app_fill_scale +
    labs(
      x = "Date",
      y = "Constrained Sizing", 
      title = title
    )
}
