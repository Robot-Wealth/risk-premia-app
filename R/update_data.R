library(tidyverse)
library(bigrquery)
library(lubridate)
library(timeDate)
library(glue)
"
BQ pricing for queries:
- first 5TB/month free
- then $5/TB
"

# check last completed business day

# x: tz-aware datetime object. If no tz specified, will assume UTC
is_ny_business_day <- function(x) {
  
  x <- x %>% 
    with_tz(tzone = "America/New_York")
  
  hols <- holidayNYSE(year(x)) %>% 
    ymd() 
  
  if(wday(x, week_start = 1) > 5 | date(x) %in% hols) {
    return(FALSE)
  } else {
    return(TRUE)
  }
}

# x: tz-aware datetime object. If no tz specified, will assume UTC
is_after_ny_close <- function(x) {
  
  x <- x %>% 
    with_tz(tzone = "America/New_York")
  
  if(hour(x) >= 16)
    TRUE
  else
    FALSE
}

# update price data with latest from bq
# won't deal with data gaps
bq_fetch_rp_prices <- function(last_price_date) {
  
  # download anything we don't have
  tb <- bq_project_query(
    'rw-algotrader',
    query = glue(
    "SELECT *  
    FROM `rw-algotrader.master_assetclass.assetclass_price` 
    WHERE ticker in ('VTI','GLD','TLT') AND date > '{last_price_date}';"
    )
  )
  
  if(as.numeric(bq_table_meta(tb)$numRows) > 0) {
    latest_prices <- bq_table_download(tb)
    return(latest_prices)
  }
}

# update data
# cases
# last price date == previous bd and current time is < 4pm: do nothing
# last price date == yesterday's close and current time is > 4pm: start trying to update
# last price date == today's close : do nothing
# last price date < previous bd: start trying to update
# attempt update once per day or when the process restarts (eg no users were connected, then one connects)

get_current_ny_datetime <- function() {
  Sys.time() %>% 
    with_tz(tzone = "America/New_York")  
}

get_previous_bd <- function(current_datetime) {
  # get prior business day 
  # if today is a saturday, sunday or monday, get friday
  # otherwise get the previous day
  # consider nyse holidays
  previous_bd <- current_datetime - days(x = 1)
  while(is_ny_business_day(previous_bd) == FALSE) {
    previous_bd <- previous_bd - days(x = 1)
  }
  
  previous_bd
  
}

get_last_price_date <- function(prices) {
  "This returns the date of the earliest last price of all the tickers"
  prices %>% 
    group_by(ticker) %>% 
    summarise(last_update = max(date)) %>% 
    summarise(earliest_last = min(last_update)) %>% 
    pull()
}

update_price_data <- function() {
  
  load(here::here("data", "assetclass_prices.RData"))
  
  now_ny <- get_current_ny_datetime()
  last_price_date <- get_last_price_date(prices)
  previous_bd <- get_previous_bd(now_ny)
  
  if((last_price_date < previous_bd) || (last_price_date == previous_bd && wday(now_ny, week_start = 1) && is_after_ny_close(now_ny))) {
    # example: 12:01am Tuesday we start trying to download Monday's prices
    latest_prices <- bq_fetch_rp_prices(last_price_date)
  } 
  
  # latest_prices could contain dates that overlap with prices for certain tickers - see get_last_price_date()
  # TODO: also we want this to fail and raise the alarm if there has been a split/corporate action that 
  # requires reloading of the historical data
  if(!is.null(latest_prices)) {
    prices <- prices %>% 
      bind_rows(latest_prices) %>% 
      distinct(ticker, date, .keep_all = TRUE) %>% 
      arrange(date)
    
    save(prices, file = here::here("data", "assetclass_prices_copy.RData"))   
  }
  
}

# update_price_data()

# tests
# last_bd <- "2020-07-14 14:12:55 AEST" %>% 
#   with_tz(tzone = "America/New_York") 
# 
# is_after_ny_close(last_bd) == FALSE
# 
# while(is_ny_business_day(last_bd) == FALSE) {
#   last_bd <- last_bd - days(x = 1)
# }
# 
# date(last_bd) == "2020-07-14"
# is_after_ny_close(last_bd) == FALSE
# 
# last_bd <- "2020-07-12" %>% 
#   ymd() %>% 
#   force_tz(tzone = "America/New_York") 
# 
# while(is_ny_business_day(last_bd) == FALSE) {
#   last_bd <- last_bd - days(x = 1)
# }
# 
# date(last_bd) == "2020-07-10"
# 
# is_ny_business_day("2020-07-11")  # This is a Saturday. No tz --> assumption of UTC