# Risk Premia Harvesting

Simple risk premia harvesting strategy in an interactive shiny application. 

No timing - rather, get long assets that tend to go up in most economic environments. Explore buy and hold, equal weight rebalancing and risk parity approaches. 
See `./background/` for background research. 

## Lanuch shiny application

Open project then run

```R
library(shiny)
options(shiny.autoload.r = FALSE)
runApp()
```
## Data

Upon startup, the application attempts to download any recent data missing from the data distributed with the application. 

Aplication startup occurs when:

- it is deployed anew 
- it is restarted from the shinyapps dashboard
- or when all users disconnect for >15 minutes such that the application sleeps, then someone else connects

The download attempt proceeds thus:

From midnight, the application attempts to download the previous trading day's prices. Success depends on when the data in BQ is updated. eg at 12:01 Tuesday, we start trying to download Monday's prices. Obviously, if the data hasn't been added to BQ, the udpate won't happen.

There is an [open issue](https://github.com/Robot-Wealth/risk-premia-app/issues/10) for tighter integration with the BQ data update and triggering a re-deployment. 

Backtests and weight calculations are based on end of month data, but also includes the last date for which we have prices. This allows the user to rebalance to today's weights rather than the last end-of-month weights if, for example, they started trading the strategy in the middle of the month. 