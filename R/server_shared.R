# common server elements (visible in server scope, shared across sessions)

library(tidyverse)
library(here)
library(DT)
library(lubridate)
library(tidyquant)
library(slider)

# load data, functions
load(here::here("data", "us_etf_prices.RData"))
load(here::here("data", "us_lev_etf_prices.RData"))
load(here::here("data", "ucits_etf_prices.RData"))
load(here::here("data", "tbill_yields.RData"))
source(here::here("R", "analysis_utils.R"), local = TRUE)
source(here::here("R", "backtest_utils.R"), local = TRUE)

theme_set(theme_bw())

gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

all_prices <- us_etf_prices %>% 
  bind_rows(us_lev_etf_prices) %>% 
  bind_rows(ucits_etf_prices) %>% 
  arrange(date)

# calculate total returns
all_prices <- all_prices %>% 
  add_total_returns_col()

# set up monthly prices, returns
month_ends <- get_monthends(all_prices)

all_monthly_prices <- make_monthly_prices(all_prices, month_ends) %>% 
  arrange(date) %>%
  group_by(ticker) %>%
  mutate(returns = ((close) / dplyr::lag(close)) - 1)

all_monthly_unadjusted <- make_monthly_prices(all_prices, month_ends, price_var = close) %>% 
  arrange(date) %>%
  group_by(ticker)

# set up monthly t-bill yields
irx_monthends <- get_monthends(irx)
monthly_yields <- make_monthly_prices(irx, irx_monthends, ticker_var = symbol, price_var = series) %>% 
  arrange(date)

# ensure prices and yields have common index
ticker_temp <- all_monthly_prices %>% distinct(ticker) %>% first() %>% pull()
monthly_yields <- monthly_yields %>% 
  right_join(all_monthly_prices %>% filter(ticker == ticker_temp) %>%  select(date, ticker), by = "date") %>% 
  tidyr::fill(symbol, .direction = "down") %>% 
  tidyr::fill(close, .direction = "down")



