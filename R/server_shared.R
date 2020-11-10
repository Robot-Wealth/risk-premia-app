# common server elements (visible in server scope, shared across sessions)

library(tidyverse)
library(here)
library(DT)
library(lubridate)
library(tidyquant)
library(slider)

# load data, functions
load(here::here("data", "assetclass_prices_copy.RData"))
source(here::here("R", "analysis_utils.R"), local = TRUE)
source(here::here("R", "backtest_utils.R"), local = TRUE)

theme_set(theme_bw())

gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

app_cols <- gg_color_hue(5)

names(app_cols) <- c(rp_tickers, "Cash", "Portfolio")
app_col_scale <- scale_colour_manual(name = "ticker", values = app_cols)
app_fill_scale <- scale_fill_manual(name = "ticker", values = app_cols)

# calculate total returns
prices <- prices %>% 
  filter(ticker %in% rp_tickers) %>% 
  add_total_returns_col()

# set up monthly prices, returns
monthends <- get_monthends(prices)

monthlyprices <- make_monthly_prices(prices, monthends) %>% 
  arrange(date) %>%
  group_by(ticker) %>%
  mutate(returns = ((close) / dplyr::lag(close)) - 1)

# backtest start and end dates
startDate <- monthlyprices %>% summarise(min(date)) %>% pull()
endDate <- monthlyprices %>% summarise(max(date)) %>% pull()
initDate <- startDate - 1 # initDate is day before startdate
