# backtest utils usage

library(here)

load(here::here("data", "assetclass_prices_copy.RData"))
source(here::here("R", "analysis_utils.R"))
source(here::here("R", "backtest_utils.R"))  

gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

app_cols <- gg_color_hue(5)
rp_tickers <- c("VTI", "TLT", 'GLD')
names(app_cols) <- c(rp_tickers, "Cash", "Portfolio")
app_col_scale <- scale_colour_manual(name = "ticker", values = app_cols)
app_fill_scale <- scale_fill_manual(name = "ticker", values = app_cols)

prices <- prices %>% 
  filter(ticker %in% rp_tickers) %>% 
  add_total_returns_col()

monthends <- get_monthends(prices)

monthlyprices <- make_monthly_prices(prices, monthends) %>% 
  arrange(date) %>%
  group_by(ticker) %>%
  mutate(returns = ((close) / dplyr::lag(close)) - 1)

startDate <- monthlyprices %>% summarise(min(date)) %>% pull()
endDate <- monthlyprices %>% summarise(max(date)) %>% pull()
initDate <- startDate - 1 # InitDate is day before startdate

theosize_constrained <- calc_vol_target(prices, 60, 5/100.) %>%
  cap_leverage()

volsize_prices <- monthlyprices %>%
  inner_join(select(theosize_constrained, ticker, date, theosize_constrained), by = c('ticker','date'))

volsize_prices %>%
  constrained_sizing_plot(title = '3 ETF USD Risk Premia - Theoretical Constrained Sizing (% of Portfolio Equity')

rp_rebal <- share_based_backtest(volsize_prices, 10000, 1, 1, 0.5/100., 0.5, rebal_method = "rp")

rp_rebal %>%
  stacked_area_chart('3 ETF USD Risk Premia - Simple Risk Parity')
