library(shiny)
library(tidyverse)
library(here)
library(shinyjs)
library(shinyWidgets)
library(shinyKnobs)
library(shinycssloaders)
# library(here)
# library(tidyverse)
library(glue)
library(roll)

# prevent automatic sourcing of R directory
shinyOptions(shiny.autoload.r = FALSE)

# update price data on startup of new process - if app is not running locally
if(Sys.getenv("SHINY_PORT") != "") {
    print("Updating data")
    source(here::here("R", "update_data.R"), local = TRUE) # visible to server, all sessions
    update_price_data()
}

source(here::here("R", "global.R"), local = FALSE)  # global scope: visible to server and ui, all sessions
source(here::here("R", "server_shared.R"), local = TRUE)  # visible to server, all sessions

# UI ====================================

ui <- navbarPage(
    shinyjs::useShinyjs(),
    
    title = "Risk Premia Harvesting",
    id = "tab",
    selected = "perfTab",
    
    # performance tab -------------------
    
    tabPanel(
        "Performance",
        value = "perfTab",
    
        sidebarLayout(
            sidebarPanel(
                selectizeInput("assets", "Select Assets", choices = c("VTI", "TLT", "GLD"), selected = c("VTI", "TLT", "GLD"), multiple = TRUE),
            ),
        
            mainPanel(
                plotOutput("cumReturnsPlot") %>% 
                    withSpinner(),
                plotOutput("rollingPerfPlot") %>% 
                    withSpinner(),
                plotOutput("rollCorrPlot") %>% 
                    withSpinner(),
                plotOutput("corrMatPlot")%>% 
                    withSpinner()
            )
        )
    ),
    
    # scatterplot tab -------------------
            
    tabPanel(
        "Scatterplots",
        value = "autcorrTab",
        sidebarLayout(
            sidebarPanel(
                selectizeInput("assets", "Select Assets", choices = c("VTI", "TLT", "GLD"), selected = c("VTI", "TLT", "GLD"), multiple = TRUE),
                sliderInput("estWdwSize", "Select Estimation Window Length", min = 10, max = 100, step = 10, value = 30),
                sliderInput("fwdWdwSize", "Select Forward Window Length", min = 10, max = 100, step = 10, value = 30),
                checkboxInput("removeOverlapping", label = "Show Non-Overlapping Periods Only", value = TRUE)
        
        ),
            mainPanel(
                fluidRow(
                    column(6, plotOutput("laggedReturnsPlot") %>% withSpinner()),
                    column(6, plotOutput("laggedVolPlot") %>% withSpinner())
                )
            )
        )
    ),
    
    # backtesting tab -------------------
    
    tabPanel(
        "Backtest",
        value = "backtestTab",
        sidebarLayout(
            sidebarPanel(
                fluidRow(
                    column(6, sliderInput("initEqSlider", "Initial Equity, $", min = 1000, max = 100000, step = 1000, value = 20000)),
                    column(6, sliderInput("maxLeverageSlider", "Maximum Leverage", min = 0, max = 5, step = 0.25, value = 1))
                ),
                fluidRow(
                    column(4, 
                           sliderInput("commKnob", "Comm. c/share", min = 0.1, max = 2, step = 0.01, value = 0.5)
                    ),
                    column(4, 
                           sliderInput("minCommKnob", "Min Comm. $/order", min = 0, max = 10, step = 0.5, value = 0.5)
                    ),
                    column(4,
                        sliderInput("marginInterestSlider", "Interest, %pa", min = 0, max = 10, step = 0.5, value = 0.25)
                    )
                ),
                fluidRow(
                    column(
                        6, 
                        sliderInput("targetVolSlider1", glue("{rp_tickers[1]} Target Volatility, %"), min = 1, max = 10, step = 0.5, value = 5),
                        sliderInput("targetVolSlider2", glue("{rp_tickers[2]} Target Volatility, %"), min = 1, max = 10, step = 0.5, value = 5),
                        sliderInput("targetVolSlider3", glue("{rp_tickers[3]} Target Volatility, %"), min = 1, max = 10, step = 0.5, value = 5)
                    ),
                    column(6, 
                        sliderInput("volLookbackSlider", "Volatility Estimation Window, days", min = 5, max = 120, step = 5, value = 60),
                        sliderInput("rebalFreqSlider", "Rebalance Frequency, months", min = 1, max = 12, step = 1, value = 1),
                        sliderInput("capFreqSlider", "Frequency to Capitalise Profits", min = 0, max = 12, step = 1, value = 1)
                    )
                ),
                fluidRow(
                    column(12, align = "left", checkboxInput("sameVolCheckbox", "Set asset-specific vol targets", value = FALSE))
                ),
                fluidRow(
                    column(
                        12, 
                        align = "center", 
                        actionButton(
                            "runBacktestButton", 
                            "UPDATE BACKTEST", 
                            style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
                        )
                    )
                )
            ),
            
            mainPanel(
                tabsetPanel(
                    id = "backtestPanel",
                    
                    # EW B&H tab --------
                    
                    tabPanel(
                        "Equal Weight Buy and Hold",
                        value = "ewbhTab",
                        fluidRow(
                            column(12, align = "center", 
                                plotOutput("ewbhEqPlot") %>% 
                                    withSpinner(),
                                tableOutput("ewbhPerfTable"),
                                plotOutput("ewbhRollPerfPlot") %>% 
                                    withSpinner(),
                                plotOutput("ewbhTradesPlot", height = "150px"),
                                plotOutput("ewbhCommPlot", height = "150px"),
                                plotOutput("ewbhCommExpPlot", height = "150px"),
                                plotOutput("ewbhInterestPlot", height = "150px"),
                                DT::dataTableOutput(("ewbhTradesTable"))
                            )
                        )
                    ),
                    
                    # EW Rebal tab ------
                    
                    tabPanel(
                    "Equal Weight Rebalance",
                    value = "ewrebalTab", 
                        fluidRow(
                            column(12, align = "center", 
                               plotOutput("ewrebEqPlot")%>% 
                                   withSpinner(),
                               tableOutput("ewrebPerfTable")%>% 
                                   withSpinner(),
                               plotOutput("ewrebRollPerfPlot"),
                               plotOutput("ewrebTradesPlot", height = "150px"),
                               plotOutput("ewrebCommPlot", height = "150px"),
                               plotOutput("ewrebCommExpPlot", height = "150px"),
                               plotOutput("ewrebInterestPlot", height = "150px"),
                               DT::dataTableOutput(("ewrebTradesTable"))
                            )
                        )
                    ),
                    
                    # Risk Parity tab ------
                    
                    tabPanel(
                        "Risk Parity",
                        fluidRow(
                            column(12, align = "center", 
                               plotOutput("rpEqPlot")%>% 
                                   withSpinner(),
                               plotOutput("rpTheoSizePlot", height = "150px")%>% 
                                   withSpinner(),
                               tableOutput("rpPerfTable"),
                               plotOutput("rpRollPerfPlot")%>% 
                                   withSpinner(),
                               plotOutput("rpTradesPlot", height = "150px"),
                               plotOutput("rpCommPlot", height = "150px"),
                               plotOutput("rpCommExpPlot", height = "150px"),
                               plotOutput("rprebInterestPlot", height = "150px"),
                               DT::dataTableOutput(("rpTradesTable"))
                            )
                        )
                    )
                )
            )
        )
    )
)

# Server ================================

server <- function(input, output) {
    
    # scoped to individual sessions
    
    ## Analysis reactives -------
    source(here::here("R", "analysis_reactives.R"), local = TRUE)
    
    ## Backtest reactives --------
    source(here::here("R", "backtest_reactives.R"), local = TRUE)
    

}

# Run the application 
shinyApp(ui = ui, server = server)
