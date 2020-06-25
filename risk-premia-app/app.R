library(shiny)
library(tidyverse)
library(here)
library(shinyjs)
library(shinyWidgets)
library(DT)
library(lubridate)
library(tidyquant)
library(slider)
#library(shinycssloaders)

theme_set(theme_bw())

gg_color_hue <- function(n) {
    hues = seq(15, 375, length = n + 1)
    hcl(h = hues, l = 65, c = 100)[1:n]
}

app_cols <- gg_color_hue(5)
rp_tickers <- c("VTI", "TLT", 'GLD')
names(app_cols) <- c(rp_tickers, "Cash", "Portfolio")
app_col_scale <- scale_colour_manual(name = "ticker", values = app_cols)
app_fill_scale <- scale_fill_manual(name = "ticker", values = app_cols)

# load data, functions

# to run locally
# load(here::here("risk-premia-app", "data", "assetclass_prices.RData"))  
# source(here::here("risk-premia-app", "R", "utils.R"), local = TRUE) 

# for deployment
source("R/utils.R", local = TRUE)
load("data/assetclass_prices.RData")
# source("R/backtest_utils.R", local = TRUE) 

# TODO:
    # maybe load data from BQ on the first startup of the day so it's always up to date without always hitting BQ
    # needs application service token
    # Apply colour scheme such that colours of already selected assets don't change: DONE
    # rolling correlation: implement with roll not slider: DONE
    # spinners on plot load
    # tour
    # rolling corrplot - should use different colours as they don't represent the things above....
    # numbers over tiles on corrplot
    # tooltip for freq to capitalise profits
    # setup backtest params could go into global_utils (same for all sessions....)
    # performance tables to three decimal places
    # disable inputs on backtest tabs: DONE



# calculate total returns
prices <- prices %>% 
    filter(ticker %in% rp_tickers) %>% 
    add_total_returns_col()

ui <- navbarPage(
    shinyjs::useShinyjs(),
    
    title = "Risk Premia Harvesting",
    id = "tab",
    selected = "perfTab",
    
    tabPanel(
        "Performance",
        value = "perfTab",
    
        sidebarLayout(
            sidebarPanel(
                selectizeInput("assets", "Select Assets", choices = c("VTI", "TLT", "GLD"), selected = c("VTI", "TLT", "GLD"), multiple = TRUE),
            ),
        
            mainPanel(
                plotOutput("cumReturnsPlot"),
                plotOutput("rollingPerfPlot"),
                plotOutput("rollCorrPlot"),
                plotOutput("corrMatPlot")
            )
        )
    ),
            
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
                    column(6, plotOutput("laggedReturnsPlot")),
                    column(6, plotOutput("laggedVolPlot"))
                )
            )
        )
    ),
    
    tabPanel(
        "Backtest",
        value = "backtestTab",
        sidebarLayout(
            sidebarPanel(
                sliderInput("initEqSlider", "Initial Equity, $", min = 1000, max = 1000000, step = 1000, value = 10000),
                fluidRow(
                    column(6, 
                        knobInput("commKnob", "Per Share Commission, cents", min = 0.1, max = 2, step = 0.01, value = 0.5, displayPrevious = TRUE)
                    ),
                    column(6, 
                        knobInput("minCommKnob", "Minimum Commission Per Order, $", min = 0, max = 10, step = 0.5, value = 0.5, displayPrevious = TRUE)
                    )
                ),
                fluidRow(
                    column(6, 
                        sliderInput("targetVolSlider", "Target Asset Volatility, %", min = 1, max = 10, step = 0.5, value = 5)
                    ),
                    column(6, 
                        sliderInput("volLookbackSlider", "Volatility Estimation Window, days", min = 5, max = 120, step = 5, value = 60)
                    )
                ),
                fluidRow(
                    column(6, 
                        sliderInput("rebalFreqSlider", "Rebalance Frequency, months", min = 1, max = 12, step = 1, value = 1)
                    ),
                    column(6, 
                        sliderInput("capFreqSlider", "Frequency to Capitalise Profits", min = 0, max = 12, step = 1, value = 1)
                    )
                ),
            ),
            
            mainPanel(
                tabsetPanel(
                    id = "backtestPanel",
                    tabPanel(
                        "Equal Weight Buy and Hold",
                        value = "ewbhTab",
                        fluidRow(
                            column(12, align = "center", 
                                plotOutput("ewbhEqPlot"),
                                tableOutput("ewbhPerfTable"),
                                plotOutput("ewbhRollPerfPlot"),
                                plotOutput("ewbhTradesPlot", height = "150px"),
                                plotOutput("ewbhCommPlot", height = "150px"),
                                plotOutput("ewbhCommExpPlot", height = "150px"),
                                DT::dataTableOutput(("ewbhTradesTable"))
                            )
                        )
                    ),
                    tabPanel(
                    "Equal Weight Rebalance",
                    value = "ewrebalTab", 
                        fluidRow(
                            column(12, align = "center", 
                               plotOutput("ewrebEqPlot"),
                               tableOutput("ewrebPerfTable"),
                               plotOutput("ewrebRollPerfPlot"),
                               plotOutput("ewrebTradesPlot", height = "150px"),
                               plotOutput("ewrebCommPlot", height = "150px"),
                               plotOutput("ewrebCommExpPlot", height = "150px"),
                               DT::dataTableOutput(("ewrebTradesTable"))
                            )
                        )
                    ),
                    tabPanel(
                        "Risk Parity",
                        fluidRow(
                            column(12, align = "center", 
                               plotOutput("rpEqPlot"),
                               plotOutput("rpTheoSizePlot", height = "150px"),
                               tableOutput("rpPerfTable"),
                               plotOutput("rpRollPerfPlot"),
                               plotOutput("rpTradesPlot", height = "150px"),
                               plotOutput("rpCommPlot", height = "150px"),
                               plotOutput("rpCommExpPlot", height = "150px"),
                               DT::dataTableOutput(("rpTradesTable"))
                            )
                        )
                    )
                )
            )
        )
    )
    
    # initEq <- 10000
    # perShareComm <- 0.005
    # minCommPerOrder <- 1 
    # # TODO: Switch to calc commission on close not adjustedclose
    # assetVolTarget <- 0.05
    # volLookback <- 60
    # 
    # rebalFrequency <- 1 # rebalance frequency in months. Can't be zero otherwise will crap out.
    # capFrequency <- 1 # frequence to capitalise profits. 0 = don't. 
)

server <- function(input, output) {
    
    # source(here::here("risk-premia-app", "R", "backtest_utils.R"), local = TRUE) 
    source("R/backtest_utils.R", local = TRUE) 
    
    observe({
        if(input$backtestPanel == "ewbhTab") {
            shinyjs::disable(id = "volLookbackSlider")
            shinyjs::disable(id = "targetVolSlider")
            shinyjs::disable(id = "rebalFreqSlider")
            shinyjs::disable(id = "capFreqSlider")
            
        } else if (input$backtestPanel == "ewrebalTab") {
            shinyjs::disable(id = "volLookbackSlider")
            shinyjs::disable(id = "targetVolSlider")
            shinyjs::enable(id = "rebalFreqSlider")
            shinyjs::enable(id = "capFreqSlider")
        } else {
            shinyjs::enable(id = "rebalFreqSlider")
            shinyjs::enable(id = "capFreqSlider")
            shinyjs::enable(id = "volLookbackSlider")
            shinyjs::enable(id = "targetVolSlider")
        }
    })
    
    df <- reactive({
        prices %>% 
            filter(ticker %in% input$assets)
    })

    output$cumReturnsPlot <- renderPlot({
        df() %>% 
            total_returns_plot()
    })
    
    output$rollingPerfPlot <- renderPlot({
        df() %>% 
            rolling_ann_perf() %>% 
            rolling_ann_perf_plot()
    })
    
    output$rollCorrPlot <- renderPlot({
        df() %>% 
            roll_pairwise_corrs() %>% 
            roll_pairwise_corrs_plot(facet = FALSE)
    })
    
    output$corrMatPlot <- renderPlot({
        df() %>% 
            cormat() %>% 
            cormat_plot()
    })
    
    output$laggedReturnsPlot <- renderPlot({
        df() %>% 
            lagged_returns_scatterplot(estimation_wdw = input$estWdwSize, forward_wdw = input$fwdWdwSize, remove_overlapping = input$removeOverlapping)
    })
    
    output$laggedVolPlot <- renderPlot({
        df() %>% 
            lagged_vol_scatterplot(estimation_wdw = input$estWdwSize, forward_wdw = input$fwdWdwSize, remove_overlapping = input$removeOverlapping)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
