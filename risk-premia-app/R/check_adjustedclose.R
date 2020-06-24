# test differences in adjusted close bewteen us and AV

library(tidyverse)
library(tidyquant)
library(bigrquery)
library(here)

prices <- query_exec(
  "SELECT *  FROM `rw-algotrader.master_assetclass.assetclass_price`;", 
  project = 'rw-algotrader', 
  use_legacy_sql = FALSE
)

save(prices, file = here::here("data", "assetclass_prices.RData"))

rp_assets <- c("VTI", "TLT", "GLD")

bq <- prices %>% 
  filter(ticker %in% c("TLT")) %>% 
  group_by(ticker) %>% 
  arrange(date) %>% 
  rename("symbol" = ticker)

av_api_key(Sys.getenv("AV_API_KEY"))
p <- tq_get(c("TLT", "VTI"), av_fun = "TIME_SERIES_DAILY_ADJUSTED")  # multiple symbols in one API call

av <- p %>%  
  filter(symbol %in% c("TLT")) %>% 
  group_by(symbol) %>% 
  arrange(date) 

diff <- av %>% 
  left_join(bq, by = c("symbol", "date")) %>% 
  select(date, adjusted, closeadjusted) %>% 
  mutate(diff = adjusted - closeadjusted) 

diff %>% 
  ggplot(aes(x = date, y = diff)) + geom_line()

diff %>% 
  filter(date > "2014-08-01", date < "2015-01-01") %>% 
  ggplot(aes(x = date, y = diff)) + geom_line()



# calculate cumulative returns
totalreturns <- prices %>%
  arrange(date) %>%
  group_by(ticker) %>%
  mutate(totalreturns = ((close + dividends) / dplyr::lag(close)) - 1) %>%
  select(-closeadjusted) %>%
  na.omit() %>%
  mutate(cumreturns = cumprod(1+totalreturns))

adjratios <- totalreturns %>%
  group_by(ticker) %>%
  summarise(date = max(date)) %>%
  inner_join(totalreturns, by = c('ticker', 'date')) %>%
  mutate(ratio = close / cumreturns) %>%
  select(ticker, ratio)

returns <- totalreturns %>%
  inner_join(adjratios, by = 'ticker') %>%
  mutate(adjclose = cumreturns * ratio)

# check against adjusted prices
p <- prices %>% 
  arrange(date) %>%
  group_by(ticker) %>%
  mutate(totalreturns = ((closeadjusted) / dplyr::lag(closeadjusted)) - 1) %>%
  na.omit() %>%
  mutate(cumreturns = cumprod(1+totalreturns)) %>% 
  ggplot(aes(x=date, y = cumreturns, color = ticker)) + geom_line()

returns %>% 
  filter(ticker == "GLD") %>% 
  select(ticker, date, totalreturns) %>% 
  inner_join(p, by = c("ticker", "date")) %>% 
  mutate(diff = totalreturns.x - totalreturns.y) %>% 
  ggplot(aes(x = date, y = diff)) + geom_line()