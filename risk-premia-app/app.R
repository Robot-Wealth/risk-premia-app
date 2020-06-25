library(shiny)
library(tidyverse)
library(here)
library(shinyjs)
library(shinyWidgets)

theme_set(theme_bw())

# load data, functions
load(here::here("risk-premia-app", "data", "assetclass_prices.RData"))  # to run locally
source(here::here("risk-premia-app", "R", "utils.R")) # to run locally

# source("R/utils.R")  # for deployment
# load("data/assetclass_prices.RData")  # for deployment

# TODO:
    # load from BQ: do we want the unwashed masses hitting our cloud?
    # maybe load data from BQ on the first startup of the day so it's always up to date without always hitting BQ
    # needs application service token
    # Apply colour scheme such that colours of already selected assets don't change - 
    # rolling correlation: implement with roll not slider
    # spinners on plot load
    # tour
    # rolling corrplot - should use different colours as they don't represent the things above....
    # numbers over tiles on corrplot
    # tooltip for freq to capitalise profits
    



# calculate total returns
rp_tickers <- c("VTI", "TLT", 'GLD')

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
                sliderInput("initEqSlider", "Initial Equity, $", min = 1000, max = 100000, step = 1000, value = 10000),
                fluidRow(
                    column(6, 
                        knobInput("commKnob", "Per Share Commission, cents", min = 0, max = 5, step = 0.05, value = 0.5, displayPrevious = TRUE)
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
                        sliderInput("rebalFreqSlider", "Rebalance Frequency, months", min = 1, max = 6, step = 1, value = 1)
                    ),
                    column(6, 
                        sliderInput("capFreqSlider", "Frequency to Capitalise Profits", min = 0, max = 12, step = 1, value = 0)
                    )
                ),
            ),
            
            mainPanel()
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
    
    # observe({
    #     if(input$"tab" == "perfTab") {
    #         shinyjs::disable(id = "estWdwSize")
    #         shinyjs::disable(id = "fwdWdwSize")
    #         shinyjs::disable(id = "removeOverlapping")
    #     } else {
    #         shinyjs::enable(id = "estWdwSize")
    #         shinyjs::enable(id = "fwdWdwSize")
    #         shinyjs::enable(id = "removeOverlapping")
    #     }
    # })
    
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
