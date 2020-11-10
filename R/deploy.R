library(rsconnect)

rsconnect::setAccountInfo(
  name = Sys.getenv("SHINYIO_RW_NAME"),
  token = Sys.getenv("SHINYIO_RW_TOKEN"),
  secret = Sys.getenv("SHINYIO_RW_SECRET")
)

rsconnect::deployApp(
  appDir = "C:/Users/Kris/Documents/risk-premia-app", 
  account = "robotwealth"
)